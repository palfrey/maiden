use pest_derive::Parser as DeriveParser;
use pest::Parser;
use clap::{App, Arg};
use std::fs::File;
use std::io::Read;

#[derive(DeriveParser)]
#[grammar = "rockstar.peg"]
pub struct Rockstar;

mod common;

fn main() -> common::Result<()> {
    pretty_env_logger::try_init().unwrap_or(());
    let matches = App::new("Maiden")
        .version("1.0")
        .author("Tom Parker <palfrey@tevp.net>")
        .about("Rockstar interpreter")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .get_matches();
    let mut f = File::open(matches.value_of("INPUT").unwrap())?;
    let mut buffer = String::new();
    f.read_to_string(&mut buffer)?;
    let parsed = Rockstar::parse(Rule::program, &buffer).unwrap_or_else(|e| panic!("{}", e));
    println!("{:?}", parsed);
    return Ok(());
}