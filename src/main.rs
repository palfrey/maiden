use pest_derive::Parser;
//use pest::Parser;

#[derive(Parser)]
#[grammar = "../spec/satriani/rockstar.peg"]
pub struct INIParser;

fn main() {
}