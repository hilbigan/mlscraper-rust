extern crate mlscraper_rust;

use mlscraper_rust::train;
use mlscraper_rust::selectors::Selector;
use mlscraper_rust::search::{AttributeBuilder, FuzzerSettings};
use env_logger::Env;

fn main() {
    env_logger::Builder::from_env(Env::default().default_filter_or("info"))
        .init();
    const USER_AGENT: &str = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:85.0) Gecko/20100101 Firefox/85.0;";

    let pages = &[
        "https://www.leagueofgraphs.com/match/euw/6375150547",
        "https://www.leagueofgraphs.com/match/euw/6379043327",
        "https://www.leagueofgraphs.com/match/euw/6378924500",
    ];
    let client = reqwest::blocking::Client::new();
    let htmls = pages.iter()
        .map(|url| {
            client.get(*url)
                .header("User-Agent", USER_AGENT)
                .send()
                .expect("request")
                .text()
                .expect("text")
        })
        .collect::<Vec<_>>();


    let filter = |selector: &Selector| {
        !selector.string.contains(".no-padding-right") 
            && !selector.string.contains("#graph")
    };

    let filter_matchtable = |selector: &Selector| {
        !selector.string.contains(".no-padding-right")
            && !selector.string.contains("#graph")
            && selector.string.starts_with(".matchTable")
    };

    let mut fuzzer_settings = FuzzerSettings::default();
    fuzzer_settings.random_generation_count = 500;
    fuzzer_settings.survivor_count = 100;
    fuzzer_settings.random_mutation_count = 50;

    let result = train(
        htmls.iter().map(|s| s.as_ref()).collect(),
        vec![
            AttributeBuilder::new("team0result")
                .values(&[Some("Defeat"), Some("Victory"), Some("Victory")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("game_duration")
                .values(&[Some("(32:16)"), Some("(23:18)"), Some("(28:27)")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("game_type")
                .values(&[
                    Some("Ranked Solo/Duo"),
                    Some("Normal (Draft Pick)"),
                    Some("Ranked Solo/Duo"),
                ])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner0")
                .values(&[
                    Some("FranciscoFrasc0"),
                    Some("kthxinator"),
                    Some("kthxinator"),
                ])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner1")
                .values(&[
                    Some("Filthy Gibbin"),
                    Some("LordKrullzuPenny"),
                    Some("npvimwvk"),
                ])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner2")
                .values(&[Some("scott pilgriim"), Some("Egrassa"), Some("Madcrow999")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner3")
                .values(&[Some("H BO5S"), Some("Zentaa"), Some("k1llu4h")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner4")
                .values(&[Some("PizzaNightmare"), Some("spixieo"), Some("RonnyMcFly")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner5")
                .values(&[
                    Some("kthxinator"),
                    Some("xanimetal"),
                    Some("Aki Hayakawa13"),
                ])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner6")
                .values(&[
                    Some("jakkittu 1v9"),
                    Some("mondsegen"),
                    Some("Better Whuffle"),
                ])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner7")
                .values(&[
                    Some("InAtlantideUrbis"),
                    Some("Kata go brr"),
                    Some("Pleisteision"),
                ])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner8")
                .values(&[Some("BomberMan35"), Some("UnfreshA"), Some("kintitus")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner9")
                .values(&[Some("Bob de Bertus"), Some("Zentaru"), Some("Oligor")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner0rank")
                .values(&[Some("Bronze I"), Some("Bronze I"), Some("Bronze II")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner1rank")
                .values(&[Some("Unranked"), Some("Unranked"), Some("Bronze IV")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner2rank")
                .values(&[Some("Bronze I"), Some("Silver IV"), Some("Bronze IV")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner3rank")
                .values(&[Some("Silver IV"), Some("Silver IV"), Some("Bronze II")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner4rank")
                .values(&[Some("Bronze IV"), Some("Unranked"), Some("Bronze IV")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner5rank")
                .values(&[Some("Bronze II"), Some("Unranked"), Some("Bronze IV")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner6rank")
                .values(&[Some("Silver IV"), Some("Unranked"), Some("Bronze II")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner7rank")
                .values(&[Some("Bronze II"), Some("Bronze III"), Some("Bronze IV")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner8rank")
                .values(&[Some("Silver IV"), Some("Bronze II"), Some("Unranked")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner9rank")
                .values(&[Some("Bronze IV"), Some("Unranked"), Some("Bronze III")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner0champion")
                .values(&[Some("Yorick"), Some("Tryndamere"), Some("Jax")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner1champion")
                .values(&[Some("Viego"), Some("Xin Zhao"), Some("Master Yi")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner2champion")
                .values(&[Some("Annie"), Some("Lux"), Some("Veigar")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner3champion")
                .values(&[Some("Miss Fortune"), Some("Nilah"), Some("Ezreal")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner4champion")
                .values(&[Some("Lux"), Some("Senna"), Some("Soraka")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner5champion")
                .values(&[Some("Jax"), Some("Riven"), Some("Garen")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner6champion")
                .values(&[Some("Gragas"), Some("Sona"), Some("Vi")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner7champion")
                .values(&[Some("Vladimir"), Some("Katarina"), Some("Katarina")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner8champion")
                .values(&[Some("Jinx"), Some("Miss Fortune"), Some("Ashe")])
                .filter(&filter)
                .build(),
            AttributeBuilder::new("summoner9champion")
                .values(&[Some("Anivia"), Some("Rek'Sai"), Some("Lulu")])
                .filter(&filter)
                .build(),
            //AttributeBuilder::new("summoner0item0")
            //    .values(&[
            //        Some("Dead Man's Plate"),
            //        Some("Navori Quickblades"),
            //        Some("Ravenous Hydra"),
            //    ])
            //    .filter(&filter_matchtable)
            //    .build(),
            //AttributeBuilder::new("summoner0item3")
            //    .values(&[
            //        Some("Thornmail"),
            //        Some("Long Sword"),
            //        Some("Ionian Boots of Lucidity"),
            //    ])
            //    .filter(&filter_matchtable)
            //    .build(),
            //AttributeBuilder::new("summoner0item4")
            //    .values(&[Some("Stealth Ward"), Some("Long Sword"), Some("Kindlegem")])
            //    .filter(&filter_matchtable)
            //    .build(),
            //AttributeBuilder::new("summoner0item6")
            //    .values(&[None, Some("Stealth Ward"), Some("Stealth Ward")])
            //    .filter(&filter_matchtable)
            //    .build(),
            //AttributeBuilder::new("summoner5item0")
            //    .values(&[
            //        Some("Sterak's Gage"),
            //        Some("Ravenous Hydra"),
            //        Some("Plated Steelcaps"),
            //    ])
            //    .filter(&filter_matchtable)
            //    .build(),
            //AttributeBuilder::new("summoner5item3")
            //    .values(&[
            //        Some("Ravenous Hydra"),
            //        Some("Long Sword"),
            //        Some("Chain Vest"),
            //    ])
            //    .filter(&filter_matchtable)
            //    .build(),
            //AttributeBuilder::new("summoner5item4")
            //    .values(&[
            //        Some("Glacial Buckler"),
            //        Some("Long Sword"),
            //        Some("Stealth Ward"),
            //    ])
            //    .filter(&filter_matchtable)
            //    .build(),
            //AttributeBuilder::new("summoner5item6")
            //    .values(&[Some("Stealth Ward"), None, None])
            //    .filter(&filter_matchtable)
            //    .build(),
            AttributeBuilder::new("summoner0kills")
                .values(&[Some("0"), Some("5"), Some("5")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner0deaths")
                .values(&[Some("9"), Some("4"), Some("2")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner0assists")
                .values(&[Some("2"), Some("2"), Some("2")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner1kills")
                .values(&[Some("9"), Some("9"), Some("7")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner1deaths")
                .values(&[Some("3"), Some("1"), Some("3")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner1assists")
                .values(&[Some("4"), Some("1"), Some("4")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner2kills")
                .values(&[Some("4"), Some("2"), Some("6")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner2deaths")
                .values(&[Some("3"), Some("2"), Some("4")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner2assists")
                .values(&[Some("5"), Some("5"), Some("3")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner3kills")
                .values(&[Some("13"), Some("1"), Some("10")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner3deaths")
                .values(&[Some("7"), Some("5"), Some("4")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner3assists")
                .values(&[Some("5"), Some("1"), Some("7")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner4kills")
                .values(&[Some("0"), Some("3"), Some("2")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner4deaths")
                .values(&[Some("9"), Some("3"), Some("3")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner4assists")
                .values(&[Some("12"), Some("4"), Some("17")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner5kills")
                .values(&[Some("7"), Some("3"), Some("2")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner5deaths")
                .values(&[Some("3"), Some("5"), Some("2")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner5assists")
                .values(&[Some("0"), Some("1"), Some("3")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner6kills")
                .values(&[Some("15"), Some("2"), Some("6")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner6deaths")
                .values(&[Some("3"), Some("4"), Some("3")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner6assists")
                .values(&[Some("7"), Some("7"), Some("2")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner7kills")
                .values(&[Some("4"), Some("4"), Some("5")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner7deaths")
                .values(&[Some("6"), Some("2"), Some("9")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner7assists")
                .values(&[Some("7"), Some("1"), Some("2")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner8kills")
                .values(&[Some("2"), Some("4"), Some("2")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner8deaths")
                .values(&[Some("7"), Some("4"), Some("10")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner8assists")
                .values(&[Some("9"), Some("4"), Some("5")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner9kills")
                .values(&[Some("3"), Some("2"), Some("1")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner9deaths")
                .values(&[Some("7"), Some("5"), Some("6")])
                .filter(&filter_matchtable)
                .build(),
            AttributeBuilder::new("summoner9assists")
                .values(&[Some("3"), Some("3"), Some("3")])
                .filter(&filter_matchtable)
                .build(),
        ],
        fuzzer_settings,
        3,
    ).expect("training");

    let new_page = client.get("https://www.leagueofgraphs.com/match/kr/6481506591")
        .header("User-Agent", USER_AGENT)
        .send()
        .expect("request")
        .text()
        .expect("text");

    let new_dom = result.parse(&new_page)
        .expect("parse");

    println!("Game type: {:?}", result.get_value(&new_dom, "game_type").ok().flatten());
    println!("Game duration: {:?}", result.get_value(&new_dom, "game_duration").ok().flatten());
    println!("Game result: {:?}", result.get_value(&new_dom, "team0result").ok().flatten());

    for i in 0 .. 10 {
        println!(
            "Player {i}: {:?} ({:?}, {:?}), K/D/A: {:?}/{:?}/{:?}",
            result.get_value(&new_dom, &format!("summoner{i}")).ok().flatten().unwrap_or("N/A".into()),
            result.get_value(&new_dom, &format!("summoner{i}champion")).ok().flatten().unwrap_or("N/A".into()),
            result.get_value(&new_dom, &format!("summoner{i}rank")).ok().flatten().unwrap_or("N/A".into()),
            result.get_value(&new_dom, &format!("summoner{i}kills")).ok().flatten().unwrap_or("N/A".into()),
            result.get_value(&new_dom, &format!("summoner{i}deaths")).ok().flatten().unwrap_or("N/A".into()),
            result.get_value(&new_dom, &format!("summoner{i}assists")).ok().flatten().unwrap_or("N/A".into())
        )
    }
}
