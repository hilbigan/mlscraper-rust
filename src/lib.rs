extern crate tl;

pub mod search;
pub mod selectors;
pub mod util;

use crate::search::*;
use crate::selectors::*;
use rand::rngs::SmallRng;
use rand::SeedableRng;

pub fn train<'a, S: Into<&'a str>>(
    mut documents: Vec<S>, 
    attributes: Vec<Attribute<'a>>, 
    settings: FuzzerSettings,
    iterations: usize)
-> Vec<Option<String>> {
    let doms = documents.drain(..)
        .map(|doc| tl::parse(doc.into(), tl::ParserOptions::default())
             .expect("HTML parsing failed"))
        .collect();
    let mut training = Training::with_settings(
        doms,
        attributes,
        settings
    ).expect("TODO error handling");
    let mut rng = SmallRng::from_entropy();

    for _ in 0 .. iterations {
        training.do_one_fuzzing_round(&mut rng);
    }

    training.attributes().iter()
        .map(|attr| training.get_best_selector_for(attr))
        .map(|selector| selector.map(|sel| sel.to_string()))
        .collect()
}
