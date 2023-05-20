extern crate mlscraper_rust;

use mlscraper_rust::train;

use mlscraper_rust::search::{AttributeBuilder, FuzzerSettings};
use mlscraper_rust::util::TextRetrievalOption;

use std::fs;
use std::time::Instant;

fn main() {
    let galaxy = fs::read_to_string("./python_comparison/amazon_galaxy.html").unwrap();
    let iphone = fs::read_to_string("./python_comparison/amazon_iphone.html").unwrap();

    // Enable matching of data-attributes (attributes starting with "data-")
    let mut settings = FuzzerSettings::default();
    settings
        .text_retrieval_options
        .push(TextRetrievalOption::AttributeStartsWith("data-".into()));

    let start_time = Instant::now();
    let result = train(
        vec![galaxy.as_str(), iphone.as_str()],
        vec![
            AttributeBuilder::new("product")
                .values(&[
                        Some("Samsung Galaxy S21 5G, US Version, 128GB, Phantom Gray - Unlocked (Renewed)"),
                        Some("Apple iPhone 11, 64GB, Black - Unlocked (Renewed)"),
                ])
                .build(),
            AttributeBuilder::new("price")
                .values(&[
                        Some("$244.95"),
                        Some("$293.00"),
                ])
                .build(),
            //AttributeBuilder::new("add-to-cart")
            //    .values(&[
            //            Some("Add to Cart"),
            //            Some("Add to Cart"),
            //    ])
            //    .build(),
        ],
        settings,
        1
    ).expect("training");
    println!(
        "Elapsed training time: {} ms",
        start_time.elapsed().as_millis()
    );

    // Print best selectors
    println!("{:?}", result.selectors());
}
