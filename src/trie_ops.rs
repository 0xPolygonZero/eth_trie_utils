//! Defines various operations for
//! [`PartialTrie`][crate::partial_trie::PartialTrie].

use std::fmt::Display;

use ethereum_types::H256;
use log::trace;

use crate::{
    partial_trie::{Nibble, Nibbles, PartialTrie, WrappedNode},
    utils::TrieNodeType,
};

/// A entry to be inserted into a `PartialTrie`.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct InsertEntry {
    pub nibbles: Nibbles,
    pub v: ValOrHash,
}

impl From<(Nibbles, ValOrHash)> for InsertEntry {
    fn from((nibbles, v): (Nibbles, ValOrHash)) -> Self {
        Self { nibbles, v }
    }
}

impl Display for InsertEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TrieEntry: (k: {}, v: {:?})", &self.nibbles, self.v)
    }
}

impl InsertEntry {
    pub(crate) fn truncate_n_nibbles(&mut self, n: usize) {
        self.nibbles = self.nibbles.truncate_n_nibbles_front(n);
    }
}

/// A "entry" in a [`PartialTrie`].
///  
/// Entries in the trie may either be actual values or
/// [`Hash`](crate::partial_trie::PartialTrie::Hash) nodes.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ValOrHash {
    /// A value in a trie.
    Val(Vec<u8>),

    /// A part of a larger trie that we are not storing but still need to know
    /// the merkle hash for.
    Hash(H256),
}

impl From<Vec<u8>> for ValOrHash {
    fn from(value: Vec<u8>) -> Self {
        Self::Val(value)
    }
}

impl From<H256> for ValOrHash {
    fn from(hash: H256) -> Self {
        Self::Hash(hash)
    }
}

impl ValOrHash {
    pub fn expect_leaf_val(self) -> Vec<u8> {
        match self {
            ValOrHash::Val(leaf_v) => leaf_v,
            ValOrHash::Hash(h) => panic!("Expected a value we were inserting to be a leaf value but instead found a hash! (hash: {})", h),
        }
    }
}

/// prefix/postfix info when comparing two `Nibbles`.
#[derive(Debug)]
struct ExistingAndNewNodePreAndPost {
    common_prefix: Nibbles,
    existing_postfix: Nibbles,
    new_postfix: Nibbles,
}

/// When splitting a leaf/extension node after an insert, there is a chance that
/// we may place one of the nodes right into the value node of the branch. This
/// enum just indicates whether or not a value needs to go into the branch node.
#[derive(Debug)]
enum ExistingOrNewBranchValuePlacement {
    BranchValue(Vec<u8>, (Nibble, WrappedNode)),
    BothBranchChildren((Nibble, WrappedNode), (Nibble, WrappedNode)),
}

#[derive(Debug)]
enum IterStackEntry {
    Root(WrappedNode),
    Extension(usize),
    Branch(BranchStackEntry),
}

#[derive(Debug)]
struct BranchStackEntry {
    children: [WrappedNode; 16],
    value: Vec<u8>,
    curr_nib: Nibble,
}

#[derive(Debug)]
pub struct PartialTrieIter {
    curr_key_after_last_branch: Nibbles,
    trie_stack: Vec<IterStackEntry>,
}

impl PartialTrieIter {
    fn advance_iter_to_next_empty_leaf_or_hash_node(
        &mut self,
        node: &WrappedNode,
        mut curr_key: Nibbles,
    ) -> Option<(Nibbles, ValOrHash)> {
        match node.as_ref().as_ref() {
            PartialTrie::Empty => None,
            PartialTrie::Hash(h) => Some((curr_key, ValOrHash::Hash(*h))),
            PartialTrie::Branch { children, value } => {
                self.trie_stack
                    .push(IterStackEntry::Branch(BranchStackEntry {
                        children: children.clone(),
                        value: value.clone(),
                        curr_nib: 1,
                    }));

                self.curr_key_after_last_branch = curr_key;
                curr_key.push_nibble_back(0);
                self.advance_iter_to_next_empty_leaf_or_hash_node(&children[0], curr_key)
            }
            PartialTrie::Extension { nibbles, child } => {
                self.trie_stack
                    .push(IterStackEntry::Extension(nibbles.count));
                curr_key = curr_key.merge_nibbles(nibbles);

                self.advance_iter_to_next_empty_leaf_or_hash_node(child, curr_key)
            }
            PartialTrie::Leaf { nibbles, value } => {
                curr_key = curr_key.merge_nibbles(nibbles);
                Some((curr_key, ValOrHash::Val(value.clone())))
            }
        }
    }
}

impl Iterator for PartialTrieIter {
    type Item = (Nibbles, ValOrHash);

    fn next(&mut self) -> Option<(Nibbles, ValOrHash)> {
        let mut next_iter_item = None;

        while next_iter_item.is_none() && let Some(mut stack_entry) = self.trie_stack.pop() {
            next_iter_item = match stack_entry {
                IterStackEntry::Root(root) =>
                    self.advance_iter_to_next_empty_leaf_or_hash_node(&root, Nibbles::default()),
                IterStackEntry::Extension(num_nibbles) => {
                    // Drop nibbles that extension added since we are going back up the trie.
                    self.curr_key_after_last_branch.truncate_n_nibbles_back_mut(num_nibbles);
                    None
                }
                IterStackEntry::Branch(ref mut branch_entry) => {
                    let curr_nib = branch_entry.curr_nib;

                    match curr_nib {
                        1..=15 => {
                            branch_entry.curr_nib += 1;
                            let next_child = branch_entry.children[curr_nib as usize].clone();
                            self.trie_stack.push(stack_entry);

                            let updated_key = self.curr_key_after_last_branch.merge_nibble(curr_nib);
                            self.advance_iter_to_next_empty_leaf_or_hash_node(&next_child, updated_key)
                        },
                        16 => {
                            let res = match branch_entry.value.is_empty() {
                                false => {
                                    let value_key = self.curr_key_after_last_branch;
                                    Some((value_key, ValOrHash::Val(branch_entry.value.clone())))
                                },
                                true => None,
                            };

                            if !self.curr_key_after_last_branch.is_empty() {
                                self.curr_key_after_last_branch.truncate_n_nibbles_back_mut(1);
                            }

                            res
                        }
                        _ => unreachable!("Trie iterator managed to reach nibble 17 or 0"),
                    }
                },
            }
        }

        next_iter_item
    }
}

impl PartialTrie {
    /// Inserts a node into the trie.
    pub fn insert<K, V>(&mut self, k: K, v: V)
    where
        K: Into<Nibbles>,
        V: Into<ValOrHash>,
    {
        let ins_entry = (k.into(), v.into()).into();
        trace!("Inserting new node {:?}...", ins_entry);

        // Inserts are guaranteed to update the root node.
        *self = *insert_into_trie_rec(&self.clone().into(), ins_entry)
            .unwrap()
            .as_ref()
            .clone();
    }

    /// Add more nodes to the trie through an iterator
    pub fn extend<K, V, I>(&mut self, nodes: I)
    where
        K: Into<Nibbles>,
        V: Into<ValOrHash>,
        I: IntoIterator<Item = (K, V)>,
    {
        for (k, v) in nodes {
            self.insert(k, v);
        }
    }

    /// Get a node if it exists in the trie.
    pub fn get<K>(&self, k: K) -> Option<&[u8]>
    where
        K: Into<Nibbles>,
    {
        self.get_intern(&mut k.into())
    }

    fn get_intern(&self, curr_nibbles: &mut Nibbles) -> Option<&[u8]> {
        match self {
            PartialTrie::Empty | PartialTrie::Hash(_) => {
                trace!("Get traversed {:?}", self);
                None
            }
            // Note: If we end up supporting non-fixed sized keys, then we need to also check value.
            PartialTrie::Branch { children, value } => {
                // Check against branch value.
                if curr_nibbles.is_empty() {
                    return (!value.is_empty()).then_some(value.as_slice());
                }

                let nib = curr_nibbles.pop_next_nibble_front();
                trace!("Get traversed Branch (nibble: {:x})", nib);
                children[nib as usize].get_intern(curr_nibbles)
            }
            PartialTrie::Extension { nibbles, child } => {
                trace!("Get traversed Extension (nibbles: {:?})", nibbles);
                let r = curr_nibbles.pop_nibbles_front(nibbles.count);

                match r.nibbles_are_identical_up_to_smallest_count(nibbles) {
                    false => None,
                    true => child.get_intern(curr_nibbles),
                }
            }
            PartialTrie::Leaf { nibbles, value } => {
                trace!("Get traversed Leaf (nibbles: {:?})", nibbles);
                match nibbles.nibbles_are_identical_up_to_smallest_count(curr_nibbles) {
                    false => None,
                    true => Some(value),
                }
            }
        }
    }

    /// Deletes a `Leaf` node or `Branch` value field if it exists.
    ///
    /// To agree with Ethereum specs, deleting nodes does not result in the trie
    /// removing nodes that are redundant after deletion. For example, a
    /// `Branch` node that is completely empty after all of its children are
    /// deleted is not pruned. Also note:
    /// - Deleted leaves are replaced with `Empty` nodes.
    /// - Deleted branch values are replaced with empty `Vec`s.
    ///
    /// # Panics
    /// If a `Hash` node is traversed, a panic will occur. Since `Hash` nodes
    /// are meant for parts of the trie that are not relevant, traversing one
    /// means that a `Hash` node was created that potentially should not have
    /// been.
    pub fn delete<K>(&mut self, k: K) -> Option<Vec<u8>>
    where
        K: Into<Nibbles>,
    {
        let k = k.into();
        trace!("Deleting a leaf node with key {} if it exists", k);

        delete_intern(&self.clone().into(), k).map(|(updated_root, deleted_val)| {
            // Final check at the root if we have an extension node
            *self = *try_collapse_if_extension(updated_root).as_ref().clone();
            deleted_val
        })
    }

    /// Returns an iterator over the trie that returns all key/value pairs for
    /// every `Leaf` and `Hash` node.
    pub fn items(&self) -> impl Iterator<Item = (Nibbles, ValOrHash)> {
        PartialTrieIter {
            curr_key_after_last_branch: Nibbles::default(),
            trie_stack: vec![IterStackEntry::Root(self.clone().into())],
        }
    }

    /// Returns an iterator over the trie that returns all keys for every `Leaf`
    /// and `Hash` node.
    pub fn keys(&self) -> impl Iterator<Item = Nibbles> {
        self.items().map(|(k, _)| k)
    }

    /// Returns an iterator over the trie that returns all values for every
    /// `Leaf` and `Hash` node.
    pub fn values(&self) -> impl Iterator<Item = ValOrHash> {
        self.items().map(|(_, v)| v)
    }
}

impl<K, V> FromIterator<(K, V)> for PartialTrie
where
    K: Into<Nibbles>,
    V: Into<ValOrHash>,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(nodes: T) -> Self {
        let mut root = PartialTrie::Empty;
        root.extend(nodes.into_iter());

        root
    }
}

fn insert_into_trie_rec(node: &WrappedNode, mut new_node: InsertEntry) -> Option<WrappedNode> {
    match node.as_ref().as_ref() {
        PartialTrie::Empty => {
            trace!("Insert traversed Empty");
            Some(create_node_from_insert_val(new_node.nibbles, new_node.v))
        }
        PartialTrie::Hash(_) => {
            trace!("Insert traversed {:?}", node);
            unreachable!(
                "Found a `Hash` node during an insert in a `PartialTrie`! These should not be able to be traversed during an insert!"
            )
        }
        PartialTrie::Branch { children, value } => {
            if new_node.nibbles.count == 0 {
                trace!("Insert traversed branch and placed value in node");
                return Some(branch_from_insert_val(children.clone(), new_node.v));
            }

            let nibble = new_node.nibbles.pop_next_nibble_front();
            trace!("Insert traversed Branch (nibble: {:x})", nibble);

            insert_into_trie_rec(&children[nibble as usize], new_node).map(|updated_child| {
                let mut updated_children = children.clone();
                updated_children[nibble as usize] = updated_child;
                branch(updated_children, value.clone())
            })
        }
        PartialTrie::Extension { nibbles, child } => {
            trace!("Insert traversed Extension (nibbles: {:?})", nibbles);

            // Note: Child is guaranteed to be a branch.
            assert!(matches!(***child, PartialTrie::Branch { .. }), "Extension node child should be guaranteed to be a branch, but wasn't! (Ext node: {:?})", node);

            let info = get_pre_and_postfixes_for_existing_and_new_nodes(nibbles, &new_node.nibbles);

            if nibbles.nibbles_are_identical_up_to_smallest_count(&new_node.nibbles) {
                new_node.truncate_n_nibbles(nibbles.count);

                return insert_into_trie_rec(child, new_node)
                    .map(|updated_child| extension(*nibbles, updated_child));
            }

            // Drop one since branch will cover one nibble.
            // Also note that the postfix is always >= 1.
            let existing_postfix_adjusted_for_branch =
                info.existing_postfix.truncate_n_nibbles_front(1);

            // If we split an extension node, we may need to place an extension node after
            // the branch.
            let updated_existing_node = match existing_postfix_adjusted_for_branch.count {
                0 => child.clone(),
                _ => extension(existing_postfix_adjusted_for_branch, child.clone()),
            };

            Some(place_branch_and_potentially_ext_prefix(
                &info,
                updated_existing_node,
                new_node,
            ))
        }
        PartialTrie::Leaf { nibbles, value } => {
            trace!("Insert traversed Leaf (nibbles: {:?})", nibbles);

            // Update existing node value if already present.
            if *nibbles == new_node.nibbles {
                return Some(leaf_from_insert_val(*nibbles, new_node.v));
            }

            let info = get_pre_and_postfixes_for_existing_and_new_nodes(nibbles, &new_node.nibbles);

            // This existing leaf is going in a branch, so we need to truncate the first
            // nibble since it's going to be represented by the branch.
            let existing_node_truncated = leaf(
                nibbles.truncate_n_nibbles_front(info.common_prefix.count + 1),
                value.clone(),
            );

            Some(place_branch_and_potentially_ext_prefix(
                &info,
                existing_node_truncated,
                new_node,
            ))
        }
    }
}

fn delete_intern(node: &WrappedNode, mut curr_k: Nibbles) -> Option<(WrappedNode, Vec<u8>)> {
    match node.as_ref().as_ref() {
        PartialTrie::Empty => {
            trace!("Delete traversed Empty");
            None
        }
        PartialTrie::Hash(_) => {
            panic!("Attempted to delete a value that ended up inside a hash node")
        } // TODO: Find a nice way to get the full key path...
        PartialTrie::Branch { children, value } => {
            if curr_k.is_empty() {
                return Some((branch(children.clone(), Vec::new()), value.clone()));
            }

            let nibble = curr_k.pop_next_nibble_front();
            trace!("Delete traversed Branch nibble {:x}", nibble);

            delete_intern(&children[nibble as usize], curr_k).map(
                |(updated_child, value_deleted)| {
                    // If the child we recursively called is deleted, then we may need to reduce
                    // this branch to an extension/leaf.
                    let updated_node = match node_is_empty(&updated_child)
                        && get_num_non_empty_children(children) <= 2
                    {
                        false => {
                            // Branch stays.
                            let mut updated_children = children.clone();
                            updated_children[nibble as usize] =
                                try_collapse_if_extension(updated_child);
                            branch(updated_children, value.clone())
                        }
                        true => {
                            let (child_nibble, non_empty_node) =
                                get_other_non_empty_child_and_nibble_in_two_elem_branch(
                                    children, nibble,
                                );

                            // Extension may be collapsed one level above.
                            extension(Nibbles::from_nibble(child_nibble), non_empty_node.clone())
                        }
                    };

                    (updated_node, value_deleted)
                },
            )
        }
        PartialTrie::Extension {
            nibbles: ext_nibbles,
            child,
        } => {
            trace!("Delete traversed Extension (nibbles: {:?})", ext_nibbles);

            ext_nibbles
                .nibbles_are_identical_up_to_smallest_count(&curr_k)
                .then(|| {
                    curr_k.truncate_n_nibbles_front_mut(ext_nibbles.count);
                    delete_intern(child, curr_k).map(|(updated_child, value_deleted)| {
                        let updated_node = collapse_ext_node_if_needed(ext_nibbles, &updated_child);
                        (updated_node, value_deleted)
                    })
                })
                .flatten()
        }
        PartialTrie::Leaf { nibbles, value } => {
            trace!("Delete traversed Leaf (nibbles: {:?})", nibbles);
            (*nibbles == curr_k).then(|| (PartialTrie::Empty.into(), value.clone()))
        }
    }
}

fn try_collapse_if_extension(node: WrappedNode) -> WrappedNode {
    match node.as_ref().as_ref() {
        PartialTrie::Extension { nibbles, child } => collapse_ext_node_if_needed(nibbles, child),
        _ => node,
    }
}

fn collapse_ext_node_if_needed(ext_nibbles: &Nibbles, child: &WrappedNode) -> WrappedNode {
    match child.as_ref().as_ref() {
        PartialTrie::Branch { .. } => extension(*ext_nibbles, child.clone()),
        PartialTrie::Extension {
            nibbles: other_ext_nibbles,
            child: other_ext_child,
        } => extension(
            ext_nibbles.merge_nibbles(other_ext_nibbles),
            other_ext_child.clone(),
        ),
        PartialTrie::Leaf {
            nibbles: leaf_nibbles,
            value,
        } => leaf(ext_nibbles.merge_nibbles(leaf_nibbles), value.clone()),
        _ => panic!(
            "Extension managed to get a child node type that is impossible! (child: {})",
            TrieNodeType::from(child)
        ),
    }
}

fn get_pre_and_postfixes_for_existing_and_new_nodes(
    existing_node_nibbles: &Nibbles,
    new_node_nibbles: &Nibbles,
) -> ExistingAndNewNodePreAndPost {
    let nib_idx_of_difference =
        Nibbles::find_nibble_idx_that_differs_between_nibbles_different_lengths(
            existing_node_nibbles,
            new_node_nibbles,
        );

    let (common_prefix, existing_postfix) =
        existing_node_nibbles.split_at_idx(nib_idx_of_difference);
    let new_postfix = new_node_nibbles.split_at_idx_postfix(nib_idx_of_difference);

    ExistingAndNewNodePreAndPost {
        common_prefix,
        existing_postfix,
        new_postfix,
    }
}

fn place_branch_and_potentially_ext_prefix(
    info: &ExistingAndNewNodePreAndPost,
    existing_node: WrappedNode,
    new_node: InsertEntry,
) -> WrappedNode {
    let mut children = new_branch_child_arr();
    let mut value = vec![];

    match check_if_existing_or_new_node_should_go_in_branch_value_field(
        info,
        existing_node,
        new_node,
    ) {
        ExistingOrNewBranchValuePlacement::BranchValue(branch_v, (nib, node)) => {
            children[nib as usize] = node;
            value = branch_v;
        }
        ExistingOrNewBranchValuePlacement::BothBranchChildren((nib_1, node_1), (nib_2, node_2)) => {
            children[nib_1 as usize] = node_1;
            children[nib_2 as usize] = node_2;
        }
    }

    let branch = branch(children, value);

    match info.common_prefix.count {
        0 => branch,
        _ => extension(info.common_prefix, branch),
    }
}

/// Check if the new leaf or existing node (either leaf/extension) should go
/// into the value field of the new branch.
fn check_if_existing_or_new_node_should_go_in_branch_value_field(
    info: &ExistingAndNewNodePreAndPost,
    existing_node: WrappedNode,
    new_node_entry: InsertEntry,
) -> ExistingOrNewBranchValuePlacement {
    // Guaranteed that both postfixes are not equal at this point.
    match (
        info.existing_postfix.count,
        info.new_postfix.count,
        existing_node.as_ref().as_ref(),
    ) {
        (0, _, PartialTrie::Leaf { value, .. }) => ExistingOrNewBranchValuePlacement::BranchValue(
            value.clone(),
            ins_entry_into_leaf_and_nibble(info, new_node_entry),
        ),
        (_, 0, _) => ExistingOrNewBranchValuePlacement::BranchValue(
            new_node_entry.v.expect_leaf_val(),
            (info.existing_postfix.get_nibble(0), existing_node.clone()),
        ),
        (_, _, _) => ExistingOrNewBranchValuePlacement::BothBranchChildren(
            (info.existing_postfix.get_nibble(0), existing_node.clone()),
            ins_entry_into_leaf_and_nibble(info, new_node_entry),
        ),
    }
}

fn ins_entry_into_leaf_and_nibble(
    info: &ExistingAndNewNodePreAndPost,
    entry: InsertEntry,
) -> (Nibble, WrappedNode) {
    let new_first_nibble = info.new_postfix.get_nibble(0);
    let new_node = create_node_from_insert_val(
        entry
            .nibbles
            .truncate_n_nibbles_front(info.common_prefix.count + 1),
        entry.v,
    );

    (new_first_nibble, new_node)
}

fn new_branch_child_arr() -> [WrappedNode; 16] {
    // Hahaha ok there actually is no better way to init this array unless I want to
    // use iterators and take a runtime hit...
    [
        PartialTrie::Empty.into(),
        PartialTrie::Empty.into(),
        PartialTrie::Empty.into(),
        PartialTrie::Empty.into(),
        PartialTrie::Empty.into(),
        PartialTrie::Empty.into(),
        PartialTrie::Empty.into(),
        PartialTrie::Empty.into(),
        PartialTrie::Empty.into(),
        PartialTrie::Empty.into(),
        PartialTrie::Empty.into(),
        PartialTrie::Empty.into(),
        PartialTrie::Empty.into(),
        PartialTrie::Empty.into(),
        PartialTrie::Empty.into(),
        PartialTrie::Empty.into(),
    ]
}

fn get_num_non_empty_children(children: &[WrappedNode; 16]) -> usize {
    children.iter().filter(|c| !node_is_empty(c)).count()
}

fn get_other_non_empty_child_and_nibble_in_two_elem_branch(
    children: &[WrappedNode; 16],
    our_nib: Nibble,
) -> (Nibble, &WrappedNode) {
    children
        .iter()
        .enumerate()
        .find(|(i, c)| *i != our_nib as usize && !node_is_empty(c))
        .map(|(n, c)| (n as Nibble, c))
        .expect("Expected to find a non-empty node in the branch's children")
}

fn node_is_empty(node: &WrappedNode) -> bool {
    matches!(node.as_ref().as_ref(), PartialTrie::Empty)
}

fn branch(children: [WrappedNode; 16], value: Vec<u8>) -> WrappedNode {
    PartialTrie::Branch { children, value }.into()
}

fn branch_from_insert_val(children: [WrappedNode; 16], value: ValOrHash) -> WrappedNode {
    create_node_if_ins_val_not_hash(value, |value| {
        PartialTrie::Branch { children, value }.into()
    })
}

fn extension(nibbles: Nibbles, child: WrappedNode) -> WrappedNode {
    PartialTrie::Extension { nibbles, child }.into()
}

fn leaf(nibbles: Nibbles, value: Vec<u8>) -> WrappedNode {
    PartialTrie::Leaf { nibbles, value }.into()
}

fn leaf_from_insert_val(nibbles: Nibbles, value: ValOrHash) -> WrappedNode {
    create_node_if_ins_val_not_hash(value, |value| PartialTrie::Leaf { nibbles, value }.into())
}

fn create_node_from_insert_val(nibbles: Nibbles, value: ValOrHash) -> WrappedNode {
    match value {
        ValOrHash::Val(value) => PartialTrie::Leaf { nibbles, value },
        ValOrHash::Hash(h) => PartialTrie::Hash(h),
    }
    .into()
}

fn create_node_if_ins_val_not_hash<F: FnOnce(Vec<u8>) -> WrappedNode>(
    value: ValOrHash,
    create_node_f: F,
) -> WrappedNode {
    match value {
        ValOrHash::Val(leaf_v) => create_node_f(leaf_v),
        ValOrHash::Hash(h) => panic!(
            "Attempted to place a hash node on an existing node! (hash: {})",
            h
        ),
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use log::debug;

    use super::ValOrHash;
    use crate::{
        partial_trie::{Nibble, PartialTrie},
        testing_utils::{
            common_setup, entry, entry_with_value,
            generate_n_hash_nodes_entries_for_empty_slots_in_trie,
            generate_n_random_fixed_trie_entries, generate_n_random_variable_keys,
            get_non_hash_values_in_trie, unwrap_iter_item_to_val, TestInsertValEntry,
        },
        utils::create_mask_of_1s,
    };

    const MASSIVE_TRIE_SIZE: usize = 100000;
    const COW_TEST_TRIE_SIZE: usize = 500;
    const NODES_PER_BRANCH_FOR_HASH_REPLACEMENT_TEST: usize = 200;

    fn insert_entries_and_assert_all_exist_in_trie_with_no_extra(entries: &[TestInsertValEntry]) {
        let trie = PartialTrie::from_iter(entries.iter().cloned());
        assert_all_entries_in_trie(entries, &trie)
    }

    fn assert_all_entries_in_trie(entries: &[TestInsertValEntry], trie: &PartialTrie) {
        let entries_in_trie = get_non_hash_values_in_trie(trie);

        let all_entries_retrieved: Vec<_> = entries
            .iter()
            .filter(|e| !entries_in_trie.contains(e))
            .collect();

        // HashSet to avoid the linear search below.
        let entries_hashset: HashSet<TestInsertValEntry> =
            HashSet::from_iter(entries.iter().cloned());
        let additional_entries_inserted: Vec<_> = entries_in_trie
            .iter()
            .filter(|e| !entries_hashset.contains(e))
            .collect();

        let all_entries_retrievable_from_trie = all_entries_retrieved.is_empty();
        let no_additional_entries_inserted = additional_entries_inserted.is_empty();

        if !all_entries_retrievable_from_trie || !no_additional_entries_inserted {
            println!(
                "Total retrieved/expected: {}/{}",
                entries_in_trie.len(),
                entries.len()
            );

            println!("Missing: {:#?}", all_entries_retrieved);
            println!("Unexpected retrieved: {:#?}", additional_entries_inserted);
        }

        assert!(all_entries_retrievable_from_trie);
        assert!(no_additional_entries_inserted);
    }

    #[test]
    fn single_insert() {
        common_setup();
        insert_entries_and_assert_all_exist_in_trie_with_no_extra(&[entry(0x1234)]);
    }

    #[test]
    fn two_disjoint_inserts_works() {
        common_setup();
        let entries = [entry(0x1234), entry(0x5678)];

        insert_entries_and_assert_all_exist_in_trie_with_no_extra(&entries);
    }

    #[test]
    fn two_inserts_that_share_one_nibble_works() {
        common_setup();
        let entries = [entry(0x1234), entry(0x1567)];

        insert_entries_and_assert_all_exist_in_trie_with_no_extra(&entries);
    }

    #[test]
    fn two_inserts_that_differ_on_last_nibble_works() {
        common_setup();
        let entries = [entry(0x1234), entry(0x1235)];

        insert_entries_and_assert_all_exist_in_trie_with_no_extra(&entries);
    }

    #[test]
    fn diagonal_inserts_to_base_of_trie_works() {
        common_setup();
        let entries: Vec<_> = (0..=64).map(|i| entry(create_mask_of_1s(i * 4))).collect();

        insert_entries_and_assert_all_exist_in_trie_with_no_extra(&entries);
    }

    #[test]
    fn updating_an_existing_node_works() {
        common_setup();
        let mut entries = [entry(0x1234), entry(0x1234)];
        entries[1].1 = vec![100];

        let trie = PartialTrie::from_iter(entries.into_iter());
        assert_eq!(trie.get(0x1234), Some([100].as_slice()));
    }

    #[test]
    fn mass_inserts_fixed_sized_keys_all_entries_are_retrievable() {
        common_setup();
        let entries: Vec<_> = generate_n_random_fixed_trie_entries(MASSIVE_TRIE_SIZE, 0).collect();

        insert_entries_and_assert_all_exist_in_trie_with_no_extra(&entries);
    }

    #[test]
    fn mass_inserts_variable_sized_keys_all_entries_are_retrievable() {
        common_setup();
        let entries: Vec<_> = generate_n_random_variable_keys(MASSIVE_TRIE_SIZE, 0).collect();

        insert_entries_and_assert_all_exist_in_trie_with_no_extra(&entries);
    }

    #[test]
    fn mass_inserts_variable_sized_keys_with_hash_nodes_all_entries_are_retrievable() {
        common_setup();
        let non_hash_entries: Vec<_> =
            generate_n_random_variable_keys(MASSIVE_TRIE_SIZE, 0).collect();
        let mut trie = PartialTrie::from_iter(non_hash_entries.iter().cloned());

        let extra_hash_entries = generate_n_hash_nodes_entries_for_empty_slots_in_trie(
            &trie,
            MASSIVE_TRIE_SIZE / 10,
            51,
        );
        trie.extend(extra_hash_entries.iter().cloned());

        let all_nodes: HashSet<_> = trie.items().collect();

        // Too much work to make `assert_all_entries_in_trie` work with hash nodes. Do a
        // quick hack for this test.
        assert!(non_hash_entries
            .into_iter()
            .all(|(k, v)| all_nodes.contains(&(k, ValOrHash::Val(v)))));
        assert!(extra_hash_entries
            .into_iter()
            .all(|(k, h)| all_nodes.contains(&(k, ValOrHash::Hash(h)))));
    }

    #[test]
    fn equivalency_check_works() {
        common_setup();

        assert_eq!(PartialTrie::Empty, PartialTrie::Empty);

        let entries = generate_n_random_fixed_trie_entries(MASSIVE_TRIE_SIZE, 0);
        let big_trie_1 = PartialTrie::from_iter(entries);
        assert_eq!(big_trie_1, big_trie_1);

        let entries = generate_n_random_fixed_trie_entries(MASSIVE_TRIE_SIZE, 1);
        let big_trie_2 = PartialTrie::from_iter(entries);

        assert_ne!(big_trie_1, big_trie_2)
    }

    #[test]
    fn two_variable_length_keys_with_overlap_are_queryable() {
        common_setup();

        let entries = [entry_with_value(0x1234, 1), entry_with_value(0x12345678, 2)];
        let trie = PartialTrie::from_iter(entries.iter().cloned());

        assert_eq!(trie.get(0x1234), Some([1].as_slice()));
        assert_eq!(trie.get(0x12345678), Some([2].as_slice()));
    }

    #[test]
    fn get_massive_trie_works() {
        common_setup();

        let random_entries: Vec<_> =
            generate_n_random_fixed_trie_entries(MASSIVE_TRIE_SIZE, 9001).collect();
        let trie = PartialTrie::from_iter(random_entries.iter().cloned());

        for (k, v) in random_entries.into_iter() {
            debug!("Attempting to retrieve {:?}...", (k, &v));
            let res = trie.get(k);

            assert_eq!(res, Some(v.as_slice()));
        }
    }

    #[test]
    fn held_trie_cow_references_do_not_change_as_trie_changes() {
        common_setup();

        let entries = generate_n_random_variable_keys(COW_TEST_TRIE_SIZE, 9002);

        let mut all_nodes_in_trie_after_each_insert = Vec::new();
        let mut root_node_after_each_insert = Vec::new();

        let mut trie = PartialTrie::default();
        for (k, v) in entries {
            trie.insert(k, v);

            all_nodes_in_trie_after_each_insert.push(get_non_hash_values_in_trie(&trie));
            root_node_after_each_insert.push(trie.clone());
        }

        for (old_trie_nodes_truth, old_root_node) in all_nodes_in_trie_after_each_insert
            .into_iter()
            .zip(root_node_after_each_insert.into_iter())
        {
            let nodes_retrieved = get_non_hash_values_in_trie(&old_root_node);
            assert_eq!(old_trie_nodes_truth, nodes_retrieved)
        }
    }

    #[test]
    fn trie_iter_works() {
        common_setup();

        let entries: HashSet<_> =
            generate_n_random_variable_keys(MASSIVE_TRIE_SIZE, 9003).collect();
        let trie = PartialTrie::from_iter(entries.iter().cloned());

        let trie_items: HashSet<_> = trie
            .items()
            .map(|(k, v)| (k, unwrap_iter_item_to_val(v)))
            .collect();

        assert!(entries.iter().all(|e| trie_items.contains(e)));
        assert!(trie_items.iter().all(|item| entries.contains(item)));
    }

    #[test]
    fn deleting_a_non_existent_node_returns_none() {
        common_setup();

        let mut trie = PartialTrie::default();
        trie.insert(0x1234, vec![91]);

        assert!(trie.delete(0x5678).is_none())
    }

    #[test]
    fn deleting_from_an_empty_trie_returns_none() {
        common_setup();

        let mut trie = PartialTrie::default();
        assert!(trie.delete(0x1234).is_none());
    }

    #[test]
    fn deletion_massive_trie() {
        common_setup();

        let entries: Vec<_> = generate_n_random_variable_keys(MASSIVE_TRIE_SIZE, 7).collect();
        let mut trie = PartialTrie::from_iter(entries.iter().cloned());

        // Delete half of the elements
        let half_entries = entries.len() / 2;

        let entries_to_delete = entries.iter().take(half_entries);
        for (k, v) in entries_to_delete {
            let res = trie.delete(*k);

            assert!(trie.get(*k).is_none());
            assert_eq!(res.as_ref(), Some(v));
        }

        let entries_that_still_should_exist = entries.into_iter().skip(half_entries);
        for (k, v) in entries_that_still_should_exist {
            assert_eq!(trie.get(k), Some(v.as_slice()));
        }
    }

    #[test]
    fn replacing_part_of_a_trie_with_a_hash_node_produces_same_hash() {
        let entries = (0..16).flat_map(|i| {
            generate_n_random_variable_keys(NODES_PER_BRANCH_FOR_HASH_REPLACEMENT_TEST, i).map(
                move |(mut k, v)| {
                    // Force all keys to be under a given branch at root.
                    k.truncate_n_nibbles_front_mut(1);
                    k.push_nibble_front(i as Nibble);

                    (k, v)
                },
            )
        });

        let mut trie = PartialTrie::from_iter(entries);
        let orig_hash = trie.calc_hash();

        let root_branch_children = match &mut trie {
            PartialTrie::Branch { children, .. } => children,
            _ => unreachable!(),
        };

        // Replace every even branch node in the root with a hash node.
        for i in (0..16).step_by(2) {
            let child_hash = root_branch_children[i].calc_hash();
            root_branch_children[i] = PartialTrie::Hash(child_hash).into();
        }

        let new_hash = trie.calc_hash();
        assert_eq!(orig_hash, new_hash);
    }
}
