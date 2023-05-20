use crate::util;
use std::borrow::BorrowMut;

use radix_trie::{Trie, TrieKey};
use rand::Rng;
use std::fmt::Write;
use tl::NodeHandle;
use tl::{HTMLTag, Node, Parser};

#[derive(Clone)]
pub enum SelectorPart {
    Tag(String),
    Class(String),
    Id(String),
    NthChild(usize),
}

impl ToString for SelectorPart {
    fn to_string(&self) -> String {
        let mut out = String::new();
        match self {
            SelectorPart::Tag(tag) => {
                write!(&mut out, "{tag}")
            }
            SelectorPart::Class(class) => {
                write!(&mut out, ".{class}")
            }
            SelectorPart::Id(id) => {
                write!(&mut out, "#{id}")
            }
            SelectorPart::NthChild(n) => {
                write!(&mut out, ":nth-child({n})")
            }
        }
        .expect("write");
        out
    }
}

impl SelectorPart {
    /// Returns true if this selector part matches the given HTML tag
    fn matches(&self, tag: &HTMLTag) -> bool {
        match self {
            SelectorPart::Tag(tagname) => tag.name() == tagname.as_str(),
            SelectorPart::Class(class) => tag.attributes().is_class_member(class),
            SelectorPart::Id(id) => tag
                .attributes()
                .id()
                .map(|other_id| other_id == id.as_str())
                .unwrap_or(false),
            SelectorPart::NthChild(_) => {
                panic!("cannot match :nth-child selector on its own!")
            }
        }
    }

    /// Tries to find a node matching this SelectorPart by searching all children starting
    /// from `node`. A value will be returned iff exactly one element matched.
    fn try_select(&self, node: NodeHandle, parser: &Parser) -> Option<NodeHandle> {
        let tag = node.get(parser)?.as_tag()?;

        // Handle :nth-child selector
        if let SelectorPart::NthChild(n) = self {
            debug_assert!(*n >= 1);
            return tag
                .children()
                .top()
                .iter()
                .filter(|child| {
                    // Only consider children that are tags
                    util::node_is_tag(child, parser)
                })
                .nth(*n - 1)
                .cloned();
        }

        let results = tag
            .children()
            .all(parser)
            .iter()
            .enumerate()
            .filter(|(_i, child)| matches!(child, Node::Tag(..)))
            .filter(|(_i, child)| self.matches(child.as_tag().unwrap()))
            .take(2)
            .collect::<Vec<_>>();

        if results.is_empty() || results.len() >= 2 {
            None
        } else {
            results
                .get(0)
                .map(|(i, _child)| NodeHandle::new(*i as u32 + 1 + node.get_inner()))
        }
    }

    /// Score of this SelectorPart (lower is better)
    fn score(&self) -> i32 {
        match self {
            SelectorPart::Tag(tag) => tag.len() as i32 + 1,
            SelectorPart::Class(class) => class.len() as i32 + 1,
            SelectorPart::Id(_) => 0,
            SelectorPart::NthChild(n) => 13 + (*n as i32 / 2),
        }
    }
}

#[derive(Clone)]
pub struct Selector {
    parts: Vec<SelectorPart>,
    pub string: String,
    pub score: i32,
}

impl PartialEq for Selector {
    fn eq(&self, other: &Selector) -> bool {
        self.string.eq(&other.string)
    }
}

impl Eq for Selector {}

impl TrieKey for Selector {
    fn encode_bytes(&self) -> Vec<u8> {
        TrieKey::encode_bytes(&self.string)
    }
}

impl Selector {
    /// Create a new selector from multiple SelectorParts.
    /// The parts are interspersed with " > "; this means the element matched by each SelectorPart
    /// must be the *direct parent* of the element matched by the nexted SelectorPart.
    pub fn new_from_parts(parts: Vec<SelectorPart>) -> Self {
        // TODO use intersperse once stabilized
        let string = parts
            .iter()
            .map(|part| part.to_string())
            .collect::<Vec<String>>()
            .join(" > ");
        let score = parts.iter().map(|part| part.score()).sum();
        Selector {
            parts,
            string,
            score,
        }
    }

    pub fn len(&self) -> usize {
        self.parts.len()
    }

    pub fn try_select_with_skip(
        &self,
        handle: NodeHandle,
        parser: &Parser,
        skip: usize,
    ) -> Option<NodeHandle> {
        self.parts
            .iter()
            .skip(skip)
            .fold(Some(handle), |acc, selector| {
                acc.and_then(|node| selector.try_select(node, parser))
            })
    }

    pub fn try_select_with_skip_path(
        &self,
        handle: NodeHandle,
        parser: &Parser,
        skip: usize,
        max_len: usize,
    ) -> Vec<Option<NodeHandle>> {
        self.parts
            .iter()
            .skip(skip)
            .fold(vec![], |mut path, selector| {
                if path.len() >= max_len {
                    return path;
                }

                // Continue from last node or root node
                let last = if path.is_empty() {
                    Some(handle)
                } else {
                    *path.last().unwrap()
                };

                if let Some(last_node) = last {
                    path.push(selector.try_select(last_node, parser));
                } else {
                    path.push(None);
                }

                path
            })
    }

    /// Tries to find a node matching this Selector by searching all nodes below
    /// `handle`. A result will be returned iff exactly one element matched.
    pub fn try_select(&self, handle: NodeHandle, parser: &Parser) -> Option<NodeHandle> {
        self.try_select_with_skip(handle, parser, 0)
    }

    pub fn try_select_path(
        &self,
        handle: NodeHandle,
        parser: &Parser,
        max_len: usize,
    ) -> Vec<Option<NodeHandle>> {
        self.try_select_with_skip_path(handle, parser, 0, max_len)
    }

    pub(crate) fn score(&self) -> i32 {
        self.score
    }

    /// Creates a new selector which is the combination of this selector and `other`.
    /// `other` will be the lower part of the selector.
    fn append(&self, mut other: Selector) -> Self {
        let mut selectors = Vec::with_capacity(other.parts.len() + self.parts.len());
        selectors.append(&mut self.parts.clone());
        selectors.append(&mut other.parts);

        Selector::new_from_parts(selectors)
    }

    /// Creates two new selectors by splitting the parts of this selector at `depth`.
    fn split_at(&self, depth: usize) -> (Self, Self) {
        let mut cloned = self.parts.clone();
        let tail = cloned.split_off(depth);
        (
            Selector::new_from_parts(cloned),
            Selector::new_from_parts(tail),
        )
    }
}

impl std::fmt::Debug for Selector {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "{}", self.string)
    }
}

impl From<SelectorPart> for Selector {
    fn from(value: SelectorPart) -> Self {
        Selector::new_from_parts(vec![value])
    }
}

impl ToString for Selector {
    fn to_string(&self) -> String {
        self.string.clone()
    }
}

pub(crate) struct SelectorCache {
    selector_cache: Trie<Selector, (usize, Option<NodeHandle>)>,
}

impl SelectorCache {
    /// Enable/disable caching
    const ENABLED: bool = true;

    /// Whether we always cache the "leaf node", i.e. the "deepest" result of the selector
    const ALWAYS_CACHE_LEAF: bool = true;

    /// If AGGRESSIVE_ADD_MAX_DEPTH is > 0, cache elements up to a depth of
    /// AGGRESSIVE_ADD_MAX_DEPTH from the root node even if they have not
    /// been explicitely requested.
    const AGGRESSIVE_ADD_MAX_DEPTH: usize = 4;

    pub(crate) fn new() -> Self {
        SelectorCache {
            selector_cache: Default::default(),
        }
    }

    /// Tries to select a target node by applying the selector to root.
    /// Uses a Trie to cache and reuse partial results.
    pub(crate) fn try_select(
        &mut self,
        selector: &Selector,
        root: NodeHandle,
        parser: &Parser,
    ) -> Option<NodeHandle> {
        if let Some((ancestor_length, ancestor_handle)) =
            self.selector_cache.get_ancestor_value(selector)
        {
            if ancestor_handle.is_some() && *ancestor_length < selector.len() {
                let target = selector.try_select_with_skip(
                    ancestor_handle.unwrap(),
                    parser,
                    *ancestor_length,
                );
                if SelectorCache::ENABLED {
                    let len = *ancestor_length;
                    if SelectorCache::AGGRESSIVE_ADD_MAX_DEPTH > len {
                        selector
                            .try_select_with_skip_path(
                                ancestor_handle.unwrap(),
                                parser,
                                len,
                                SelectorCache::AGGRESSIVE_ADD_MAX_DEPTH - len,
                            )
                            .iter()
                            .enumerate()
                            .for_each(|(i, subnode)| {
                                self.selector_cache.insert(
                                    selector.split_at(len + i + 1).0,
                                    (len + i + 1, *subnode),
                                );
                            });
                    }
                    if SelectorCache::ALWAYS_CACHE_LEAF
                        && SelectorCache::AGGRESSIVE_ADD_MAX_DEPTH - len < selector.len()
                    {
                        self.selector_cache
                            .insert(selector.clone(), (selector.len(), target));
                    }
                }
                target
            } else {
                *ancestor_handle
            }
        } else {
            let target = selector.try_select(root, parser);
            if SelectorCache::ENABLED {
                if SelectorCache::AGGRESSIVE_ADD_MAX_DEPTH > 0 {
                    selector
                        .try_select_path(root, parser, SelectorCache::AGGRESSIVE_ADD_MAX_DEPTH)
                        .iter()
                        .enumerate()
                        .for_each(|(i, subnode)| {
                            self.selector_cache
                                .insert(selector.split_at(i + 1).0, (i + 1, *subnode));
                        });
                }
                if SelectorCache::ALWAYS_CACHE_LEAF
                    && SelectorCache::AGGRESSIVE_ADD_MAX_DEPTH < selector.len()
                {
                    self.selector_cache
                        .insert(selector.clone(), (selector.len(), target));
                }
            }
            target
        }
    }
}

pub struct SelectorFuzzer {
    root_selector_cache: SelectorCache,
    pub(crate) retries_used: usize,
}

impl SelectorFuzzer {
    pub fn new() -> Self {
        SelectorFuzzer {
            root_selector_cache: SelectorCache::new(),
            retries_used: 0,
        }
    }

    /// Attempt to create a new selector by mutating the given input selector.
    /// Mutating in this case means we split the selector at a random point and create a new
    /// random selector for the upper part of the selector.
    pub(crate) fn mutate_selector<R: Rng>(
        &mut self,
        selector: &Selector,
        root: NodeHandle,
        parser: &Parser,
        retries: usize,
        rng: &mut R,
    ) -> Option<Selector> {
        if selector.parts.len() <= 1 {
            return None;
        }

        let random_index = rng.borrow_mut().gen_range(1..selector.parts.len());
        let (left, right) = selector.split_at(random_index);
        let left_node = self.root_selector_cache.try_select(&left, root, parser)?;
        let new_left = self.random_selector_for_node(left_node, root, parser, retries, rng)?;
        Some(new_left.append(right))
    }

    /// Recursively generate a random selector for node `handle`. `root` is the root-node
    /// of the subtree.
    pub fn random_selector_for_node<R: Rng>(
        &mut self,
        handle: NodeHandle,
        root: NodeHandle,
        parser: &Parser,
        retries: usize,
        rng: &mut R,
    ) -> Option<Selector> {
        let tag = handle.get(parser)?.as_tag()?;

        if let Some(id) = util::get_id(handle, parser) {
            return Some(Selector::from(SelectorPart::Id(id.to_string())));
        }

        let parent = util::find_parent(handle, parser);
        let has_parent = parent.is_some();
        for tries in 0..retries {
            self.retries_used += 1;
            let typ = rng.gen_range(0..3);

            let selector = match typ {
                0 => Selector::from(SelectorPart::Tag(tag.name().as_utf8_str().to_string())),
                1 => {
                    let classes = tag.attributes().class_iter()?.collect::<Vec<_>>();
                    if classes.is_empty() {
                        continue;
                    }
                    let random_index = rng.gen_range(0..classes.len());
                    Selector::from(SelectorPart::Class(classes[random_index].to_string()))
                }
                2 => {
                    if !has_parent {
                        continue;
                    }
                    let parent = parent.unwrap().get(parser).unwrap();
                    let index = parent
                        .children()
                        .unwrap()
                        .top()
                        .iter()
                        .filter(|child| util::node_is_tag(child, parser))
                        .position(|child| child.get_inner() == handle.get_inner())
                        .expect("child of parent should exists in parent.children()");
                    Selector::from(SelectorPart::NthChild(index + 1))
                }
                _ => unreachable!(),
            };

            let globally_unique = typ != 2
                && matches!(self.root_selector_cache.try_select(&selector, root, parser), Some(h) if h == handle);
            if globally_unique {
                return Some(selector);
            }
            let locally_unique = has_parent
                && matches!(selector.try_select(parent.unwrap(), parser), Some(h) if h == handle);
            if locally_unique {
                let parent_selector = self.random_selector_for_node(
                    parent.unwrap(),
                    root,
                    parser,
                    retries - tries,
                    rng,
                );
                if let Some(parent_selector) = parent_selector {
                    let combined_selector = parent_selector.append(selector);
                    return Some(combined_selector);
                }
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::selectors::*;
    use crate::util;
    use rand::SeedableRng;
    use rand_chacha::ChaCha8Rng;

    use tl::VDom;

    const HTML: &'static str = r#"
        <div class="div_class">
            <div id="div_id">
                <p class="p_class">TARGET</p>
                <p class="other_class">...</p>
            </div>
        </div>
        "#;

    fn get_simple_example() -> VDom<'static> {
        let dom = tl::parse(HTML, tl::ParserOptions::default()).unwrap();
        dom
    }

    #[test]
    fn test_find_node_with_text() {
        let dom = get_simple_example();
        let parser = dom.parser();
        let node = util::find_node_with_text(&dom, "TARGET").unwrap();
        assert_eq!(util::get_classes(node, parser).unwrap(), "p_class")
    }

    #[test]
    fn test_find_parent() {
        let dom = get_simple_example();
        let parser = dom.parser();
        let element: NodeHandle = dom.query_selector("p").unwrap().next().unwrap();
        assert_eq!(util::get_classes(element, parser).unwrap(), "p_class");
        let parent = util::find_parent(element, parser).unwrap();
        assert_eq!(util::get_id(parent, parser).unwrap(), "div_id");
        let parent_parent = util::find_parent(parent, parser).unwrap();
        assert_eq!(
            util::get_classes(parent_parent, parser).unwrap(),
            "div_class"
        );
        let parent_parent_parent = util::find_parent(parent_parent, parser);
        assert_eq!(parent_parent_parent, None)
    }

    #[test]
    fn test_selector() {
        fn test_selector(
            selector: Selector,
            expected_str: &str,
            _parser: &Parser,
        ) -> Option<NodeHandle> {
            assert_eq!(selector.to_string(), expected_str);
            let dom = get_simple_example();
            let parser = dom.parser();
            selector.try_select(/* root node = */ NodeHandle::new(1), parser)
        }

        let dom = get_simple_example();
        let parser = dom.parser();

        let target = test_selector(
            Selector::new_from_parts(vec![SelectorPart::Id("div_id".into())]),
            "#div_id",
            parser,
        );
        assert_eq!(util::get_id(target.unwrap(), parser).unwrap(), "div_id");

        let target = test_selector(
            Selector::new_from_parts(vec![
                SelectorPart::Id("div_id".into()),
                SelectorPart::NthChild(1),
            ]),
            "#div_id > :nth-child(1)",
            parser,
        );
        assert_eq!(
            util::get_classes(target.unwrap(), parser).unwrap(),
            "p_class"
        );

        let target = test_selector(
            Selector::new_from_parts(vec![
                SelectorPart::Id("div_id".into()),
                SelectorPart::NthChild(2),
            ]),
            "#div_id > :nth-child(2)",
            parser,
        );
        assert_eq!(
            util::get_classes(target.unwrap(), parser).unwrap(),
            "other_class"
        );

        let target = test_selector(
            Selector::new_from_parts(vec![
                SelectorPart::NthChild(1),
                SelectorPart::Class("p_class".into()),
            ]),
            ":nth-child(1) > .p_class",
            parser,
        );
        assert_eq!(
            util::get_classes(target.unwrap(), parser).unwrap(),
            "p_class"
        );

        let target = test_selector(
            Selector::new_from_parts(vec![SelectorPart::Class("p_class".into())]),
            ".p_class",
            parser,
        );
        assert_eq!(
            util::get_classes(target.unwrap(), parser).unwrap(),
            "p_class"
        );

        let target = test_selector(
            Selector::new_from_parts(vec![SelectorPart::Tag("div".into())]),
            "div",
            parser,
        ); // Only one div because the outer div is the root node
        assert_eq!(util::get_id(target.unwrap(), parser).unwrap(), "div_id");

        let target = test_selector(
            Selector::new_from_parts(vec![SelectorPart::Tag("p".into())]),
            "p",
            parser,
        );
        assert_eq!(target, None);

        let target = test_selector(
            Selector::new_from_parts(vec![
                SelectorPart::Tag("div".into()),
                SelectorPart::Tag("p".into()),
            ]),
            "div > p",
            parser,
        );
        assert_eq!(target, None);
    }

    #[test]
    fn test_random_selector() {
        const HTML: &'static str = r#"
        <root>
        <div class="div_class">
            <div class="div_id_class">
                <p class="p_class">TARGET</p>
                <p class="p_class">...</p>
            </div>
        </div>
        </root>
        "#;

        let dom = tl::parse(HTML, tl::ParserOptions::default()).unwrap();
        let parser = dom.parser();
        let root = util::find_root(&dom).unwrap();
        let target = dom.query_selector(".p_class").unwrap().next().unwrap();
        let mut rng = ChaCha8Rng::seed_from_u64(1337);
        let mut searcher = SelectorFuzzer::new();
        println!(
            "{:?}",
            searcher
                .random_selector_for_node(target, *root, parser, 10, &mut rng)
                .map(|sel| sel.to_string())
        );
    }
}
