extern crate mlscraper_rust;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand_chacha::ChaCha8Rng;
use rand_chacha::rand_core::SeedableRng;
use criterion::BenchmarkId;
use std::fs;
use mlscraper_rust::util;
use mlscraper_rust::selectors::{Selector, SelectorPart, SelectorFuzzer};
use tl::{NodeHandle, Parser};
use rand::Rng;


fn generate<R: Rng>(target: NodeHandle, root: NodeHandle, parser: &Parser, retries: usize, rng: &mut R) -> Option<Selector> {
    let mut fuzzer = SelectorFuzzer::new();
    fuzzer.random_selector_for_node(
        target,
        root,
        parser,
        retries,
        rng
    )
}

fn criterion_benchmark(c: &mut Criterion) {
    let input_html = fs::read_to_string("benches/match6481506591.html").unwrap();
    let dom = tl::parse(&input_html, tl::ParserOptions::default()).unwrap();
    let root = *util::find_root(&dom).unwrap();
    let target = Selector::new_from_parts(vec![
        SelectorPart::Class("matchTable".into()),
        SelectorPart::NthChild(4),
        SelectorPart::Class("text-left".into()),
        SelectorPart::NthChild(1),
        SelectorPart::Class("txt".into()),
        SelectorPart::Tag("a".into()),
        SelectorPart::Class("name".into()),
    ]).try_select(root, dom.parser()).unwrap();

    let args = (target, root, dom.parser(), 100);

    c.bench_with_input(BenchmarkId::new("generate", "benches/match6481506591.html"), &args, |b, input| {
        let mut rng = ChaCha8Rng::seed_from_u64(1337);
        b.iter(|| generate(input.0, input.1, input.2, input.3, &mut rng))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
