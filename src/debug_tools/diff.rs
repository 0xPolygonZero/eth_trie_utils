use std::fmt::{self, Debug};
use std::{fmt::Display, ops::Deref};

use ethereum_types::H256;

use super::common::{get_segment_from_node_and_key_piece, NodePath};
use crate::{
    nibbles::Nibbles,
    partial_trie::{HashedPartialTrie, Node, PartialTrie},
    utils::TrieNodeType,
};

#[derive(Debug, Eq, PartialEq)]
pub struct TrieDiff {
    pub latest_diff_res: Option<DiffPoint>,
    // TODO: Later add a second pass for finding diffs from the bottom up (`earliest_diff_res`).
}

impl Display for TrieDiff {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(diff) = &self.latest_diff_res {
            write!(f, "{}", diff)?;
        }

        Ok(())
    }
}

#[derive(Copy, Clone, Debug)]
enum DiffDetectionState {
    NodeTypesDiffer, // Also implies that hashes differ.
    HashDiffDetected,
    NoDiffDetected,
}

impl DiffDetectionState {
    fn pick_most_significant_state(&self, other: &Self) -> Self {
        match self.get_int_repr() > other.get_int_repr() {
            false => *other,
            true => *self,
        }
    }

    /// The integer representation also indicates the more "significant" state.
    fn get_int_repr(&self) -> usize {
        match self {
            DiffDetectionState::NodeTypesDiffer => 2,
            DiffDetectionState::HashDiffDetected => 1,
            DiffDetectionState::NoDiffDetected => 0,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DiffPoint {
    pub depth: usize,
    pub path: NodePath,
    pub key: Nibbles,
    pub a_info: NodeInfo,
    pub b_info: NodeInfo,
}

impl DiffPoint {
    fn new(child_a: &HashedPartialTrie, child_b: &HashedPartialTrie, parent_k: Nibbles) -> Self {
        let a_key = parent_k.merge_nibbles(&get_key_piece_from_node(child_a));
        let b_key = parent_k.merge_nibbles(&get_key_piece_from_node(child_b));

        DiffPoint {
            depth: 0,
            path: NodePath::default(),
            key: parent_k,
            a_info: NodeInfo::new(child_a, a_key, get_value_from_node(child_a).cloned()),
            b_info: NodeInfo::new(child_b, b_key, get_value_from_node(child_b).cloned()),
        }
    }
}

impl Display for DiffPoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Point Diff {{depth: {}, ", self.depth)?;
        write!(f, "Path: ({}), ", self.path)?;
        write!(f, "Key: 0x{:x} ", self.key)?;
        write!(f, "A info: {} ", self.a_info)?;
        write!(f, "B info: {}}}", self.b_info)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct NodeInfo {
    key: Nibbles,

    /// The direct value associated with the node (only applicable to `Leaf` &
    /// `Branch` nodes).
    value: Option<Vec<u8>>,
    node_type: TrieNodeType,
    hash: H256,
}

impl Display for NodeInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(key: 0x{:x} ", self.key)?;

        match &self.value {
            Some(v) => write!(f, "Value: 0x{}, ", hex::encode(v))?,
            None => write!(f, "Value: N/A, ")?,
        }

        write!(f, "Node type: {} ", self.node_type)?;
        write!(f, "Trie hash: {:x})", self.hash)
    }
}

impl NodeInfo {
    fn new(n: &HashedPartialTrie, key: Nibbles, value: Option<Vec<u8>>) -> Self {
        Self {
            key,
            value,
            node_type: n.deref().into(),
            hash: n.hash(),
        }
    }
}

pub fn create_diff_between_tries(a: &HashedPartialTrie, b: &HashedPartialTrie) -> TrieDiff {
    TrieDiff {
        latest_diff_res: find_latest_diff_point_where_tries_begin_to_diff(a, b),
    }
}

// Only support `HashedPartialTrie` due to it being significantly faster to
// detect differences due to hash caching.
fn find_latest_diff_point_where_tries_begin_to_diff(
    a: &HashedPartialTrie,
    b: &HashedPartialTrie,
) -> Option<DiffPoint> {
    let state = LatestDiffPerCallState::new(a, b, Nibbles::default(), 0);
    let mut longest_state = LatestNodeDiffState::default();

    find_latest_diff_point_where_tries_begin_to_diff_rec(state, &mut longest_state);

    longest_state
        .longest_key_node_diff
        .or(longest_state.longest_key_hash_diff)
}

#[derive(Debug, Default)]
struct LatestNodeDiffState {
    longest_key_node_diff: Option<DiffPoint>,
    longest_key_hash_diff: Option<DiffPoint>,
}

impl LatestNodeDiffState {
    fn try_update_longest_divergence_key_hash(&mut self, state: &LatestDiffPerCallState) {
        Self::replace_longest_field_if_our_key_is_larger(
            &mut self.longest_key_hash_diff,
            &state.curr_key,
            state.a,
            state.b,
        );
    }

    fn try_update_longest_divergence_key_node(&mut self, state: &LatestDiffPerCallState) {
        Self::replace_longest_field_if_our_key_is_larger(
            &mut self.longest_key_node_diff,
            &state.curr_key,
            state.a,
            state.b,
        );
    }

    fn replace_longest_field_if_our_key_is_larger(
        field: &mut Option<DiffPoint>,
        parent_k: &Nibbles,
        child_a: &HashedPartialTrie,
        child_b: &HashedPartialTrie,
    ) {
        if field
            .as_ref()
            .map_or(true, |d_point| d_point.key.count < parent_k.count)
        {
            *field = Some(DiffPoint::new(child_a, child_b, *parent_k))
        }
    }
}

// State that is copied per recursive call.
#[derive(Clone, Debug)]
struct LatestDiffPerCallState<'a> {
    a: &'a HashedPartialTrie,
    b: &'a HashedPartialTrie,
    curr_key: Nibbles,
    curr_depth: usize,

    // Horribly inefficient, but these are debug tools, so I think we get a pass.
    curr_path: NodePath,
}

impl<'a> LatestDiffPerCallState<'a> {
    /// Exists solely to prevent construction of this type from going over
    /// multiple lines.
    fn new(
        a: &'a HashedPartialTrie,
        b: &'a HashedPartialTrie,
        curr_key: Nibbles,
        curr_depth: usize,
    ) -> Self {
        Self {
            a,
            b,
            curr_key,
            curr_depth,
            curr_path: NodePath::default(),
        }
    }

    /// Note: The assumption here is that `a` and `b` are of the same node type
    /// and have the key.
    fn new_from_parent(
        &self,
        a: &'a HashedPartialTrie,
        b: &'a HashedPartialTrie,
        key_piece: &Nibbles,
    ) -> Self {
        let new_segment = get_segment_from_node_and_key_piece(a, key_piece);
        let new_path = self.curr_path.dup_and_append(new_segment);

        Self {
            a,
            b,
            curr_key: self.curr_key.merge_nibbles(key_piece),
            curr_depth: self.curr_depth + 1,
            curr_path: new_path,
        }
    }
}

fn find_latest_diff_point_where_tries_begin_to_diff_rec(
    state: LatestDiffPerCallState,
    longest_state: &mut LatestNodeDiffState,
) -> DiffDetectionState {
    let a_hash = state.a.hash();
    let b_hash = state.b.hash();

    // We're going to ignore node type differences if they have the same hash (only
    // case I think where this can happen is if one is a hash node?).
    if a_hash == b_hash {
        return DiffDetectionState::NoDiffDetected;
    }

    let a_type: TrieNodeType = state.a.deref().into();
    let b_type: TrieNodeType = state.b.deref().into();

    let a_key_piece = get_key_piece_from_node(state.a);
    let b_key_piece = get_key_piece_from_node(state.b);

    // Note that differences in a node's `value` will be picked up by a hash
    // mismatch.
    match (a_type, a_key_piece) == (b_type, b_key_piece) {
        false => {
            longest_state.try_update_longest_divergence_key_node(&state);
            DiffDetectionState::NodeTypesDiffer
        }
        true => {
            match (&state.a.node, &state.b.node) {
                (Node::Empty, Node::Empty) => DiffDetectionState::NoDiffDetected,
                (Node::Hash(a_hash), Node::Hash(b_hash)) => {
                    create_diff_detection_state_based_from_hashes(
                        a_hash,
                        b_hash,
                        &state.new_from_parent(state.a, state.b, &Nibbles::default()),
                        longest_state,
                    )
                }
                (
                    Node::Branch {
                        children: a_children,
                        value: _a_value,
                    },
                    Node::Branch {
                        children: b_children,
                        value: _b_value,
                    },
                ) => {
                    let mut most_significant_diff_found = DiffDetectionState::NoDiffDetected;

                    for i in 0..16 {
                        let res = find_latest_diff_point_where_tries_begin_to_diff_rec(
                            state.new_from_parent(
                                &a_children[i as usize],
                                &b_children[i as usize],
                                &Nibbles::from_nibble(i as u8),
                            ),
                            longest_state,
                        );
                        most_significant_diff_found =
                            most_significant_diff_found.pick_most_significant_state(&res);
                    }

                    match matches!(
                        most_significant_diff_found,
                        DiffDetectionState::NoDiffDetected
                    ) {
                        false => most_significant_diff_found,
                        true => {
                            // Also run a hash check if we haven't picked anything up yet.
                            create_diff_detection_state_based_from_hash_and_gen_hashes(
                                &state,
                                longest_state,
                            )
                        }
                    }
                }
                (
                    Node::Extension {
                        nibbles: a_nibs,
                        child: a_child,
                    },
                    Node::Extension {
                        nibbles: _b_nibs,
                        child: b_child,
                    },
                ) => find_latest_diff_point_where_tries_begin_to_diff_rec(
                    state.new_from_parent(a_child, b_child, a_nibs),
                    longest_state,
                ),
                (Node::Leaf { .. }, Node::Leaf { .. }) => {
                    create_diff_detection_state_based_from_hash_and_gen_hashes(
                        &state,
                        longest_state,
                    )
                }
                _ => unreachable!(),
            }
        }
    }
}

fn create_diff_detection_state_based_from_hash_and_gen_hashes(
    state: &LatestDiffPerCallState,
    longest_state: &mut LatestNodeDiffState,
) -> DiffDetectionState {
    let a_hash = state.a.hash();
    let b_hash = state.b.hash();

    create_diff_detection_state_based_from_hashes(&a_hash, &b_hash, state, longest_state)
}

fn create_diff_detection_state_based_from_hashes(
    a_hash: &H256,
    b_hash: &H256,
    state: &LatestDiffPerCallState,
    longest_state: &mut LatestNodeDiffState,
) -> DiffDetectionState {
    match a_hash == b_hash {
        false => {
            longest_state.try_update_longest_divergence_key_hash(state);
            DiffDetectionState::HashDiffDetected
        }
        true => DiffDetectionState::NoDiffDetected,
    }
}

// It might seem a bit weird to say a branch has no key piece, but this function
// is used to detect two nodes of the same type that have different keys.
fn get_key_piece_from_node<T: PartialTrie>(n: &Node<T>) -> Nibbles {
    match n {
        Node::Empty | Node::Hash(_) | Node::Branch { .. } => Nibbles::default(),
        Node::Extension { nibbles, child: _ } => *nibbles,
        Node::Leaf { nibbles, value: _ } => *nibbles,
    }
}

/// If the node type contains a value (without looking at the children), then
/// return it.
fn get_value_from_node<T: PartialTrie>(n: &Node<T>) -> Option<&Vec<u8>> {
    match n {
        Node::Empty | Node::Hash(_) | Node::Extension { .. } => None,
        Node::Branch { value, .. } => Some(value),
        Node::Leaf { nibbles: _, value } => Some(value),
    }
}

#[cfg(test)]
mod tests {
    use super::{create_diff_between_tries, DiffPoint, NodeInfo, NodePath};
    use crate::{
        nibbles::Nibbles,
        partial_trie::{HashedPartialTrie, PartialTrie},
        utils::TrieNodeType,
    };

    #[test]
    fn latest_single_node_hash_diffs_work() {
        // TODO: Reduce duplication once we identify common structures across tests...
        let mut a = HashedPartialTrie::default();
        a.insert(0x1234, vec![0]);
        let a_hash = a.hash();

        let mut b = a.clone();
        b.insert(0x1234, vec![1]);
        let b_hash = b.hash();

        let diff = create_diff_between_tries(&a, &b);

        let expected_a = NodeInfo {
            key: 0x1234.into(),
            value: Some(vec![0]),
            node_type: TrieNodeType::Leaf,
            hash: a_hash,
        };

        let expected_b = NodeInfo {
            key: 0x1234.into(),
            value: Some(vec![1]),
            node_type: TrieNodeType::Leaf,
            hash: b_hash,
        };

        let expected = DiffPoint {
            depth: 0,
            path: NodePath(vec![]),
            key: Nibbles::default(),
            a_info: expected_a,
            b_info: expected_b,
        };

        assert_eq!(diff.latest_diff_res, Some(expected));
    }

    #[test]
    #[ignore]
    fn latest_single_node_node_diffs_work() {
        todo!()
    }

    #[test]
    #[ignore]
    fn latest_multi_node_single_node_hash_diffs_work() {
        todo!()
    }

    #[test]
    #[ignore]
    fn latest_multi_node_single_node_node_diffs_work() {
        todo!()
    }

    #[test]
    #[ignore]
    fn latest_massive_single_node_diff_tests() {
        todo!()
    }

    #[test]
    #[ignore]
    fn latest_multi_node_multi_node_hash_diffs_work() {
        todo!()
    }

    #[test]
    #[ignore]
    fn latest_multi_node_multi_node_node_diffs_work() {
        todo!()
    }

    #[test]
    #[ignore]
    fn latest_massive_multi_node_diff_tests() {
        todo!()
    }
}
