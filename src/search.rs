use std::collections::HashMap;
use std::ops::Deref;

use log::{info, trace, warn};
use rand::Rng;
use std::time::Instant;

use crate::util::{find_root, get_direct_inner_text, get_trimmed_attr_value};
use crate::{util};
use crate::selectors::*;
use tl::{NodeHandle, VDom};

/// Strategy for dealing with missing data (expected attribute value is `None`)
pub enum MissingDataStrategy {
    /// If an expected attribute value is `None`, we do not expect the selector to match any node.
    AllowMissingNode,

    /// If an expected attribute value is `None`, the node must still exist, and it text value
    /// (see [Training::get_node_text]) must be `None` (empty text).
    NodeMustExist,
}

/// Strategy for dealing with multiple nodes matching the expected attribute value
pub enum MultipleMatchesStrategy {
    /// Choose the node which results in the best selector
    PrioritizeBestSelector,

    /// Prefer first matching node
    PrioritizeFirstMatch,
}

pub struct AttributeBuilder<'a> {
    name: String,
    values: Option<Vec<Option<String>>>,
    filter: Option<&'a dyn Fn(&Selector) -> bool>,
}

impl<'a> AttributeBuilder<'a> {
    pub fn new<S: Into<String>>(name: S) -> Self {
        AttributeBuilder {
            name: name.into(),
            values: None,
            filter: None,
        }
    }

    pub fn values(mut self, values: &[Option<&str>]) -> Self {
        self.values = Some(
            values
                .iter()
                .map(|option| option.map(|string| string.to_string()))
                .collect(),
        );
        self
    }

    pub fn filter(mut self, function: &'a dyn Fn(&Selector) -> bool) -> Self {
        self.filter = Some(function);
        self
    }

    pub fn build(self) -> Attribute<'a> {
        Attribute {
            name: self.name,
            values: self.values.unwrap_or(vec![]),
            filter: self.filter,
        }
    }
}

pub struct Attribute<'a> {
    pub(crate) name: String,
    pub(crate) values: Vec<Option<String>>,
    pub(crate) filter: Option<&'a dyn Fn(&Selector) -> bool>,
}

#[derive(Clone)]
struct CheckedSelector {
    selector: Selector,
    checked_on_all_documents: bool,
}

impl CheckedSelector {
    fn new(selector: Selector) -> Self {
        Self {
            selector,
            checked_on_all_documents: false,
        }
    }

    fn new_checked(selector: Selector) -> Self {
        Self {
            selector,
            checked_on_all_documents: true,
        }
    }
}

impl Deref for CheckedSelector {
    type Target = Selector;

    fn deref(&self) -> &Self::Target {
        &self.selector
    }
}

pub struct Training<'a> {
    documents: Vec<VDom<'a>>,
    document_roots: Vec<NodeHandle>,
    attributes: Vec<Attribute<'a>>,
    selector_pool: HashMap<String, Vec<CheckedSelector>>,

    missing_data_strategy: MissingDataStrategy,
    multiple_matches_strategy: MultipleMatchesStrategy,

    /// per document
    random_generation_count: usize,
    random_generation_retries: usize,
    /// per document
    survivor_count: usize,
    /// per document
    random_mutation_count: usize,
}

impl<'a> Training<'a> {
    pub fn documents<'l>(&'l self) -> &'l Vec<VDom<'a>> {
        &self.documents
    }

    pub fn documents_mut<'l>(&'l mut self) -> &'l mut Vec<VDom<'a>> {
        &mut self.documents
    }

    pub fn attributes<'l>(&'l self) -> &'l Vec<Attribute<'a>> {
        &self.attributes
    }

    pub fn new(documents: Vec<VDom<'a>>, attributes: Vec<Attribute<'a>>) -> Option<Self> {
        let document_roots = documents
            .iter()
            .filter_map(find_root)
            .copied()
            .collect::<Vec<_>>();
        if document_roots.len() != documents.len() {
            return None; // Failed to find root in at least one document
        }

        if attributes
            .iter()
            .any(|attr| attr.values.len() != documents.len())
        {
            return None; // At least one attribute has an incorrect number of values
        }

        let training = Training {
            documents,
            document_roots,
            attributes,
            selector_pool: Default::default(),
            missing_data_strategy: MissingDataStrategy::NodeMustExist,
            multiple_matches_strategy: MultipleMatchesStrategy::PrioritizeFirstMatch,
            random_generation_count: 1000,
            random_generation_retries: 100,
            survivor_count: 50,
            random_mutation_count: 50,
        };

        Some(training)
    }

    /// Returns the node's text value, which is either its inner text, the value
    /// of its "title" attribute, or the value of its "alt" attribute (in that order).
    /// TODO Maybe we want a setting later to control what counts as "text"
    fn get_node_text(&self, vdom: &VDom, node: NodeHandle) -> Option<String> {
        node.get(vdom.parser())
            .and_then(|node| node.as_tag())
            .and_then(|tag| {
                let inner_text = get_direct_inner_text(tag, vdom.parser());
                let trimmed_inner_text = inner_text.trim();
                if !trimmed_inner_text.is_empty() {
                    return Some(trimmed_inner_text.to_string());
                }

                let title = get_trimmed_attr_value(tag, "title");
                if title.is_some() {
                    return title;
                }

                let alt = get_trimmed_attr_value(tag, "alt");
                if alt.is_some() {
                    return alt;
                }

                None
            })
    }

    fn find_all_nodes_with_text(&self, vdom: &VDom, text: &str) -> Vec<NodeHandle> {
        let trim = text.trim();
        vdom.nodes()
            .iter()
            .enumerate()
            .map(|(i, _)| NodeHandle::new(i as u32))
            .filter(|node| matches!(self.get_node_text(vdom, *node), Some(text) if trim == text))
            .collect()
    }

    /// Check whether `selector` successfully selects `attribute` on all documents.
    fn check_selector(
        &self,
        selector: &Selector,
        attribute: &Attribute,
        ignore_document: Option<usize>,
    ) -> Result<(), usize> {
        for i in 0..self.documents.len() {
            if matches!(ignore_document, Some(d) if d == i) {
                continue;
            }
            let node = selector.try_select(self.document_roots[i], self.documents[i].parser());
            let node_text_value =
                node.and_then(|node| self.get_node_text(&self.documents[i], node));
            let expected = attribute.values[i].as_ref();

            if expected.is_none() {
                match self.missing_data_strategy {
                    MissingDataStrategy::AllowMissingNode => {
                        let ok = node.is_none() || node_text_value.is_none();
                        if !ok {
                            return Err(i);
                        }
                    }
                    MissingDataStrategy::NodeMustExist => {
                        let ok = node.is_some() && node_text_value.is_none();
                        if !ok {
                            return Err(i);
                        }
                    }
                }
            } else if node_text_value.is_none() || &node_text_value.unwrap() != expected.unwrap() {
                return Err(i);
            }
        }
        Ok(())
    }

    /// Perform one round of generation, mutation, and sorting.
    pub fn do_one_fuzzing_round<R: Rng>(&mut self, rng: &mut R) {
        for attribute in &self.attributes {
            let mut error_vote = vec![0; self.documents.len()];

            // Generate selectors for each document
            let mut document_selectors = self
                .documents
                .iter()
                .enumerate()
                .filter_map(|(i, vdom)| {
                    // Determine target node by looking for node with text matching expected attribute
                    // value.
                    let target_nodes =
                        self.find_all_nodes_with_text(vdom, attribute.values[i].as_ref()?.as_str());
                    let mut random_target_weights = Vec::new();
                    if target_nodes.is_empty() {
                        warn!(
                            "No matching target nodes for attribute {:?} in document {}",
                            attribute.name, i
                        );
                        return None;
                    }
                    if target_nodes.len() > 1 {
                        match self.multiple_matches_strategy {
                            MultipleMatchesStrategy::PrioritizeBestSelector => {
                                // Randomly select between all target_nodes
                                random_target_weights =
                                    vec![1f32 / target_nodes.len() as f32; target_nodes.len()];
                            }
                            MultipleMatchesStrategy::PrioritizeFirstMatch => {
                                // Prioritize first with 90%
                                random_target_weights = vec![
                                    0.1f32
                                        / ((target_nodes.len() - 1) as f32);
                                    target_nodes.len()
                                ];
                                random_target_weights[0] = 0.9f32;
                            }
                        }
                    }

                    let mut searcher = SelectorFuzzer::new();
                    // TODO remove clone
                    let mut selector_pool = self
                        .selector_pool
                        .get(&attribute.name)
                        .cloned()
                        .unwrap_or(Vec::new());
                    trace!(
                        "We have {} selectors for attribute {:?} from the previous iteration",
                        selector_pool.len(),
                        attribute.name
                    );
                    selector_pool.reserve(self.random_generation_count);
                    let start_time = Instant::now();
                    (0..self.random_generation_count)
                        .filter_map(|_| {
                            // Choose random target node based on weights
                            let index = if target_nodes.len() == 1 {
                                0
                            } else {
                                util::random_index_weighted(rng, &random_target_weights)
                            };
                            // Generate random selector for target node
                            searcher
                                .random_selector_for_node(
                                    target_nodes[index],
                                    self.document_roots[i],
                                    vdom.parser(),
                                    self.random_generation_retries,
                                    rng,
                                )
                                .map(CheckedSelector::new)
                        })
                        .for_each(|selector| {
                            selector_pool.push(selector);
                        });
                    let elapsed_ms = start_time.elapsed().as_millis();

                    trace!(
                        "Generation: {} total selectors for attribute {:?} in document {}",
                        selector_pool.len(),
                        attribute.name,
                        i
                    );
                    trace!(
                        "Generation rate: {} in {}ms, {:.2}/s",
                        self.random_generation_count,
                        elapsed_ms,
                        self.random_generation_count as f32 / elapsed_ms as f32 * 1000.
                    );
                    trace!(
                        "Generation retries avg. {:.2} ({} total)",
                        searcher.retries_used as f32 / self.random_generation_count as f32,
                        searcher.retries_used
                    );
                    selector_pool.dedup_by_key(|selector| selector.to_string());
                    trace!("De-dup: {} selectors left", selector_pool.len());
                    if let Some(filter) = attribute.filter {
                        selector_pool.retain(|selector| filter(selector));
                        trace!(
                            "User-defined filter: {} selectors left",
                            selector_pool.len()
                        );
                    }
                    let start_time = Instant::now();
                    selector_pool.retain_mut(|mut selector| {
                        if selector.checked_on_all_documents {
                            return true;
                        }

                        if let Err(index) = self.check_selector(selector, attribute, Some(i)) {
                            error_vote[index] += 1;
                            false
                        } else {
                            selector.checked_on_all_documents = true;
                            true
                        }
                    });
                    let elapsed_ms = start_time.elapsed().as_millis();
                    trace!(
                        "Matching all documents: {} selectors left (check_selector took {}ms)",
                        selector_pool.len(),
                        elapsed_ms
                    );
                    if selector_pool.is_empty() {
                        return None;
                    }
                    selector_pool.sort_by_key(|selector| selector.score());
                    if selector_pool.len() > self.survivor_count {
                        selector_pool.resize_with(self.survivor_count, || unreachable!());
                    }
                    trace!("Survivors: {} selectors left", selector_pool.len());
                    let start_time = Instant::now();
                    for j in 0..usize::min(selector_pool.len(), self.random_mutation_count) {
                        let mutated = searcher.mutate_selector(
                            &selector_pool[j],
                            self.document_roots[i],
                            self.documents[i].parser(),
                            self.random_generation_retries,
                            rng,
                        );
                        if let Some(mutated) = mutated {
                            if let Err(index) = self.check_selector(&mutated, attribute, Some(i)) {
                                error_vote[index] += 1;
                            } else {
                                selector_pool.push(CheckedSelector::new_checked(mutated));
                            }
                        }
                    }
                    let elapsed_ms = start_time.elapsed().as_millis();
                    trace!(
                        "After mutation: {} selectors for attribute {:?} in document {}",
                        selector_pool.len(),
                        attribute.name,
                        i
                    );
                    trace!(
                        "Mutation rate: {} in {}ms, {:.2}/s",
                        self.random_mutation_count,
                        elapsed_ms,
                        self.random_mutation_count as f32 / elapsed_ms as f32 * 1000.
                    );
                    selector_pool.dedup_by_key(|selector| selector.to_string());
                    trace!(
                        "De-dup after mutation: {} selectors left",
                        selector_pool.len()
                    );
                    if let Some(filter) = attribute.filter {
                        selector_pool.retain(|selector| filter(selector));
                        trace!(
                            "User-defined filter: {} selectors left",
                            selector_pool.len()
                        );
                    }

                    Some(selector_pool)
                })
                .flatten()
                .collect::<Vec<_>>();

            if document_selectors.is_empty() {
                warn!(
                    "No selectors for attribute {}! Likely problematic attribute value: {:?}",
                    attribute.name,
                    error_vote
                        .iter()
                        .zip(attribute.values.iter())
                        .max_by_key(|(votes, _)| **votes)
                        .map(|(_, name)| name)
                        .unwrap()
                );
                continue;
            }

            document_selectors.dedup_by_key(|selector| selector.to_string());
            document_selectors.sort_by_key(|selector| selector.score());
            if document_selectors.len() > self.survivor_count {
                document_selectors.resize_with(self.survivor_count, || unreachable!());
            }

            info!(
                "Selector with best score for attribute {:?}:\n{}",
                attribute.name,
                document_selectors[0].to_string()
            );
            self.selector_pool
                .insert(attribute.name.clone(), document_selectors);
        }
    }

    pub fn get_best_selector_for(&self, attribute: &Attribute) -> Option<Selector> {
        self.selector_pool
            .get(&attribute.name)
            .and_then(|selectors| selectors.get(0))
            .map(|selector| selector.selector.clone())
    }
}

#[cfg(test)]
mod tests {
    use crate::search::{Attribute, MissingDataStrategy, Training};
    use crate::selectors::*;
    use crate::*;
    use rand_chacha::ChaCha8Rng;
    use tl::VDom;
    use rand::SeedableRng;

    const HTML: [&'static str; 2] = [
        r#"
        <div id="root" class="root">
            <img id="1" alt="blubb" />
            <img id="2" title="glogg" />
            <p id="3">plapp_before</p>
        </div>
        "#,
        r#"
        <div id="root" class="root">
            <img id="1" alt="" />
            <p id="3">plapp_after</p>
        </div>
        "#,
    ];

    fn get_simple_example() -> (VDom<'static>, VDom<'static>) {
        (
            tl::parse(HTML[0], tl::ParserOptions::default()).unwrap(),
            tl::parse(HTML[1], tl::ParserOptions::default()).unwrap(),
        )
    }

    #[test]
    fn node_text() {
        let (dom0, dom1) = get_simple_example();
        let training = Training::new(vec![dom0, dom1], vec![]).unwrap();
        assert_eq!(
            training.get_node_text(
                &training.documents[0],
                training.documents[0].get_element_by_id("1").unwrap()
            ),
            Some("blubb".into())
        );
        assert_eq!(
            training.get_node_text(
                &training.documents[0],
                training.documents[0].get_element_by_id("2").unwrap()
            ),
            Some("glogg".into())
        );
        assert_eq!(
            training.get_node_text(
                &training.documents[0],
                training.documents[0].get_element_by_id("3").unwrap()
            ),
            Some("plapp_before".into())
        );
        assert_eq!(
            training.get_node_text(
                &training.documents[1],
                training.documents[1].get_element_by_id("1").unwrap()
            ),
            None
        );
        assert_eq!(training.documents[1].get_element_by_id("2"), None);
        assert_eq!(
            training.get_node_text(
                &training.documents[1],
                training.documents[1].get_element_by_id("3").unwrap()
            ),
            Some("plapp_after".into())
        );
        assert_eq!(
            training.get_node_text(
                &training.documents[0],
                training.documents[0].get_element_by_id("root").unwrap()
            ),
            None
        );
    }

    #[test]
    fn find_nodes() {
        let (dom0, dom1) = get_simple_example();
        let training = Training::new(vec![dom0, dom1], vec![]).unwrap();

        assert_eq!(
            training.find_all_nodes_with_text(&training.documents[0], "blubb"),
            vec![training.documents[0].get_element_by_id("1").unwrap()]
        );
        assert_eq!(
            training.find_all_nodes_with_text(&training.documents[0], "glogg"),
            vec![training.documents[0].get_element_by_id("2").unwrap()]
        );
        assert_eq!(
            training.find_all_nodes_with_text(&training.documents[0], "plapp_before"),
            vec![training.documents[0].get_element_by_id("3").unwrap()]
        );
        assert_eq!(
            training.find_all_nodes_with_text(&training.documents[1], "plapp_after"),
            vec![training.documents[1].get_element_by_id("3").unwrap()]
        );
    }

    #[test]
    fn node_matching() {
        let (dom0, dom1) = get_simple_example();
        let mut training = Training::new(
            vec![dom0, dom1],
            vec![
                Attribute {
                    name: "attr1".to_string(),
                    values: vec![Some("blubb".into()), None],
                    filter: None,
                },
                Attribute {
                    name: "attr2".to_string(),
                    values: vec![Some("glogg".into()), None],
                    filter: None,
                },
                Attribute {
                    name: "attr3".to_string(),
                    values: vec![Some("plapp_before".into()), Some("plapp_after".into())],
                    filter: None,
                },
                Attribute {
                    name: "failing_attr1".to_string(),
                    values: vec![Some("blubb".into()), Some("wrong".into())],
                    filter: None,
                },
            ],
        )
        .unwrap();

        let sel1 = Selector::new_from_parts(vec![SelectorPart::Id("1".into())]);
        let sel2 = Selector::new_from_parts(vec![SelectorPart::Id("2".into())]);
        let sel3 = Selector::new_from_parts(vec![SelectorPart::Id("3".into())]);

        training.missing_data_strategy = MissingDataStrategy::AllowMissingNode;

        // Correct values in both documents
        assert!(training
            .check_selector(&sel1, &training.attributes[0], None)
            .is_ok());
        assert!(training
            .check_selector(&sel2, &training.attributes[1], None)
            .is_ok());
        assert!(training
            .check_selector(&sel3, &training.attributes[2], None)
            .is_ok());

        // Correct values in both documents, but node 2 is missing => should error
        training.missing_data_strategy = MissingDataStrategy::NodeMustExist;
        assert!(training
            .check_selector(&sel1, &training.attributes[0], None)
            .is_ok());
        assert!(training
            .check_selector(&sel2, &training.attributes[1], None)
            .is_err());
        assert!(training
            .check_selector(&sel3, &training.attributes[2], None)
            .is_ok());

        // Wrong value in second document
        assert!(training
            .check_selector(&sel1, &training.attributes[3], None)
            .is_err());
        assert!(training
            .check_selector(&sel1, &training.attributes[3], Some(1))
            .is_ok());
    }

    #[test]
    fn fuzzing() {
        let (dom0, dom1) = get_simple_example();
        let mut training = Training::new(
            vec![dom0, dom1],
            vec![
                Attribute {
                    name: "attr1".to_string(),
                    values: vec![Some("blubb".into()), None],
                    filter: None,
                },
                Attribute {
                    name: "attr2".to_string(),
                    values: vec![Some("glogg".into()), None],
                    filter: None,
                },
                Attribute {
                    name: "attr3".to_string(),
                    values: vec![Some("plapp_before".into()), Some("plapp_after".into())],
                    filter: None,
                },
            ],
        )
        .unwrap();

        let mut rng = ChaCha8Rng::seed_from_u64(1337);
        training.do_one_fuzzing_round(&mut rng);
    }
}
