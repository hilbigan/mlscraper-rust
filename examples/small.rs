extern crate mlscraper_rust;

use mlscraper_rust::search::AttributeBuilder;
use mlscraper_rust::train;

fn main() {
    let html = reqwest::blocking::get("http://quotes.toscrape.com/author/Albert-Einstein/")
        .expect("request")
        .text()
        .expect("text");

    let result = train(
        vec![html.as_str()],
        vec![
            AttributeBuilder::new("name")
                .values(&[Some("Albert Einstein")])
                .build(),
            AttributeBuilder::new("born")
                .values(&[Some("March 14, 1879")])
                .build(),
        ],
        Default::default(),
        1,
    )
    .expect("training");

    // Print best selectors
    println!("{:?}", result.selectors());

    let html = reqwest::blocking::get("http://quotes.toscrape.com/author/J-K-Rowling")
        .expect("request")
        .text()
        .expect("text");

    let dom = result.parse(&html).expect("parse");

    result
        .attributes()
        .for_each(|attr| println!("{attr}: {:?}", result.get_value(&dom, attr).ok().flatten()))
}
