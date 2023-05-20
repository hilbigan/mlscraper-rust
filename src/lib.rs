extern crate tl;

pub mod search;
pub mod selectors;
pub mod util;

use crate::search::*;
use anyhow::Result;
use rand::rngs::SmallRng;
use rand::Rng;
use rand::SeedableRng;

/// Find suitable selectors for `attributes` in HTML documents `documents`.
///
/// The number of `iterations`
/// is the number of generations the fuzzing algorithm should produce.
/// In our experience, a very low number (1-3) of iterations should be
/// sufficient for most input HTML documents. If a document has a very
/// deep, nested structure, a higher number of iterations may be necessary.
///
/// Further settings can be adjusted with `FuzzerSettings`. If the generated
/// selectors are not satisfactory, you can experiment with increasing the
/// `random_generation_count`, `random_generation_retries` and other settings.
/// Note that this may impact the training time.
///
/// The returned `TrainingResult` can be used to retrieve the generated
/// selectors or to automatically extract information from previously
/// unseen documents.
pub fn train<'a, S: Into<&'a str>>(
    documents: Vec<S>,
    attributes: Vec<Attribute<'a>>,
    settings: FuzzerSettings,
    iterations: usize,
) -> Result<TrainingResult> {
    let mut rng = SmallRng::from_entropy();

    train_with_rng(documents, attributes, settings, iterations, &mut rng)
}

/// Same as [`train`], but with a custom random number generator (Rng).
pub fn train_with_rng<'a, R: Rng, S: Into<&'a str>>(
    mut documents: Vec<S>,
    attributes: Vec<Attribute<'a>>,
    settings: FuzzerSettings,
    iterations: usize,
    rng: &mut R,
) -> Result<TrainingResult> {
    let doms = documents
        .drain(..)
        .map(|doc| {
            tl::parse(doc.into(), tl::ParserOptions::default()).expect("HTML parsing failed")
        })
        .collect();
    let mut training = Training::with_settings(doms, attributes, settings)?;

    for _ in 0..iterations {
        training.do_one_fuzzing_round(rng);
    }

    Ok(training.to_result())
}
