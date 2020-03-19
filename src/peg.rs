use pest_derive::Parser as DeriveParser;

#[derive(DeriveParser)]
#[grammar = "rockstar.peg"]
pub struct Rockstar;
