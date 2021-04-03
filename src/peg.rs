use pest_derive::Parser as DeriveParser;

#[allow(clippy::upper_case_acronyms)]
#[derive(DeriveParser)]
#[grammar = "rockstar.peg"]
pub struct Rockstar;
