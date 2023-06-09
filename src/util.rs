use crate::selectors::Selector;
use rand::Rng;
use std::borrow::Cow;
use tl::VDom;
use tl::{HTMLTag, Node, NodeHandle, Parser};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Find parent of node using a brute force search in the parser's node table
pub(crate) fn find_parent(handle: NodeHandle, parser: &Parser) -> Option<NodeHandle> {
    let inner = handle.get_inner();
    let mut next_id = inner - 1;
    let mut optional_node = parser.resolve_node_id(next_id);
    while let Some(node) = optional_node {
        let children = node.children();
        if children.is_some()
            && children
                .unwrap()
                .top()
                .iter()
                .any(|child_handle| child_handle.get_inner() == inner)
        {
            return Some(NodeHandle::new(next_id));
        }
        next_id = next_id.checked_sub(1)?;
        optional_node = parser.resolve_node_id(next_id);
    }
    None
}

/// Find root node handle
pub fn find_root<'a>(dom: &'a VDom<'a>) -> Option<&'a NodeHandle> {
    dom.children()
        .iter()
        .find(|node| node_is_tag(node, dom.parser()))
}

/// Returns true if the parser node is a HTML tag
pub(crate) fn node_is_tag(node: &NodeHandle, parser: &Parser) -> bool {
    node.get(parser)
        .map(|node| matches!(node, Node::Tag(..)))
        .unwrap_or(false)
}

/// Returns the inner text of the node, but no text of any child nodes!
pub(crate) fn get_direct_inner_text(tag: &HTMLTag, parser: &Parser) -> String {
    tag.children()
        .top()
        .iter()
        .filter_map(|child| {
            child
                .get(parser)
                .and_then(|node| node.as_raw())
                .map(|raw| raw.as_utf8_str())
        })
        .collect()
}

/// Returns the trimmed value of attribute `attr`, if it exists and is not empty
/// (NOTE <node attr=""/> will thus return `None`.)
pub(crate) fn get_trimmed_attr_value(tag: &HTMLTag, attr: &str) -> Option<String> {
    let attrv = tag.attributes().get(attr).flatten();
    if let Some(attrv) = attrv {
        let attrv = attrv.as_utf8_str();
        let trimmed_attrv = attrv.trim();
        if !trimmed_attrv.is_empty() {
            return Some(trimmed_attrv.to_string());
        }
    }
    None
}

/// Returns the trimmed value of the first attribute whose name starts with `attr_prefix`,
pub(crate) fn get_trimmed_attr_prefix_value(tag: &HTMLTag, attr_prefix: &str) -> Option<String> {
    let attrv = tag
        .attributes()
        .iter()
        .find(|(attr, _)| attr.starts_with(attr_prefix))
        .map(|(_, val)| val)
        .flatten();
    if let Some(attrv) = attrv {
        let trimmed_attrv = attrv.trim();
        if !trimmed_attrv.is_empty() {
            return Some(trimmed_attrv.to_string());
        }
    }
    None
}

/// Searches for a node whose inner text matches the given text.
/// NOTE that this also includes the inner text of any child nodes!
/// Both strings are trimmed before comparison.
pub fn find_node_with_text(dom: &VDom, text: &str) -> Option<NodeHandle> {
    dom.nodes()
        .iter()
        .enumerate()
        .find(|(_, node)| {
            node.as_tag().is_some()
                && node
                    .as_tag()
                    .unwrap()
                    .inner_text(dom.parser())
                    .as_ref()
                    .trim()
                    == text.trim()
        })
        .map(|(i, _)| NodeHandle::new(i as u32))
}

/// Get the id of a node
pub(crate) fn get_id<'p>(handle: NodeHandle, parser: &'p Parser<'p>) -> Option<Cow<'p, str>> {
    Some(
        handle
            .get(parser)?
            .as_tag()?
            .attributes()
            .id()?
            .as_utf8_str(),
    )
}

/// Get all classes of a node as a single string
#[allow(dead_code)] // used in unit tests
pub(crate) fn get_classes<'p>(handle: NodeHandle, parser: &'p Parser<'p>) -> Option<Cow<'p, str>> {
    Some(
        handle
            .get(parser)?
            .as_tag()?
            .attributes()
            .class()?
            .as_utf8_str(),
    )
}

/// Highlight the given selector's selection by adding a red border to it
/// Returns true if successful.
pub(crate) fn style_selected_element(selector: &Selector, dom: &mut VDom) -> bool {
    const HIGHLIGHT_CSS: &'static str = "outline: 1px solid red;";
    if let Some(node) = selector.try_select(*find_root(dom).unwrap(), dom.parser()) {
        let attributes = node
            .get_mut(dom.parser_mut())
            .unwrap()
            .as_tag_mut()
            .unwrap()
            .attributes_mut();
        if let Some(Some(style)) = attributes.get_mut("style") {
            // Add to pre-existing style
            let new_style = format!("{}; {}", style.as_utf8_str(), HIGHLIGHT_CSS).into_bytes();
            style.set(new_style).is_ok()
        } else {
            attributes.insert("style", Some(HIGHLIGHT_CSS));
            true
        }
    } else {
        false
    }
}

pub(crate) fn random_index_weighted<R: Rng>(rng: &mut R, weights: &[f32]) -> usize {
    let random: f32 = rng.gen();
    let mut sum = 0f32;
    for (i, weight) in weights.iter().enumerate() {
        sum += weight;
        if sum >= random {
            return i;
        }
    }
    panic!("this should not happen: {:?} {} {}", weights, random, sum);
}

/// Different options for retrieving text from a node.
/// We generate selectors for every node that yields text that matches the expected attribute value.
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum TextRetrievalOption {
    /// Consider the node's inner text as text.
    InnerText,
    /// Consider the value of the given attribute as text.
    /// For example, Attribute("title"") will consider the value of the "title" attribute as searchable text.
    Attribute(String),
    /// Consider the value of the first attribute whose name starts with the given prefix as text.
    /// For example, AttributeStartsWith("data-") will consider the value of the first attribute whose name
    /// starts with "data-" as searchable text.
    AttributeStartsWith(String),
}

pub type TextRetrievalOptions = Vec<TextRetrievalOption>;

/// Returns the node's text value, as specified by the given [`TextRetrievalOptions`].
pub fn get_node_text(
    vdom: &VDom,
    node: NodeHandle,
    text_retrieval_options: &TextRetrievalOptions,
) -> Option<String> {
    node.get(vdom.parser())
        .and_then(|node| node.as_tag())
        .and_then(|tag| {
            for option in text_retrieval_options {
                match option {
                    TextRetrievalOption::InnerText => {
                        let inner_text = get_direct_inner_text(tag, vdom.parser());
                        let trimmed_inner_text = inner_text.trim();
                        if !trimmed_inner_text.is_empty() {
                            return Some(trimmed_inner_text.to_string());
                        }
                    }
                    TextRetrievalOption::Attribute(name) => {
                        let value = get_trimmed_attr_value(tag, &name);
                        if value.is_some() {
                            return value;
                        }
                    }
                    TextRetrievalOption::AttributeStartsWith(prefix) => {
                        let value = get_trimmed_attr_prefix_value(tag, &prefix);
                        if value.is_some() {
                            return value;
                        }
                    }
                }
            }

            None
        })
}
