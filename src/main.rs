#![allow(warnings)]

use pest::Parser;
use clap::{App, Arg};
use std::fs::File;
use std::io::{self, Read};
use std::collections::HashMap;

mod common;
mod peg;
mod runner;

use crate::common::{Expression, SymbolType, Command, Program, CommandLine};
use crate::peg::{Rockstar, Rule};

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
    let mut parsed = Rockstar::parse(Rule::program, &buffer).unwrap_or_else(|e| panic!("{}", e));
    let program = depair_program(&mut parsed);
    println!("{:?}", program);
    runner::run(&program, &mut io::stdout())?;
    return Ok(());
}

#[derive(Debug, Clone)]
enum Item {
    Expression(Expression),
    Symbol(SymbolType),
    Command(Command)
}

fn depair_program<'i, I>(pairs: &'i mut I) -> Program
where
    I: Iterator<Item = pest::iterators::Pair<'i, Rule>>
{
    let pair = pairs.next().expect("one pair");
    if pair.as_rule() != Rule::program {
        panic!("Bad rule: {:?}", pair.as_rule());
    }
    let mut commands = vec![];
    for (current_line, line) in pair.into_inner().enumerate() {
        if line.as_rule() != Rule::line {
            panic!("Bad rule: {:?}", line.as_rule());
        }
        let depaired = depair(&mut line.into_inner(), 0);
        match depaired {
            Item::Command(command) => {
                commands.push(CommandLine{cmd: command, line: current_line + 1});
            }
            Item::Symbol(SymbolType::Empty) => {},
            item => {
                println!("Something else {:?}", item);
            }
        }
    }
    common::Program {
        commands,
        functions: HashMap::new()
    }
}

impl From<Expression> for Item {
    fn from(exp: Expression) -> Item {
        Item::Expression(exp)
    }
}

impl From<SymbolType> for Item {
    fn from(sym: SymbolType) -> Item {
        Item::Symbol(sym)
    }
}

impl From<Command> for Item {
    fn from(command: Command) -> Item {
        Item::Command(command)
    }
}

fn depair<'i, I>(pairs: &'i mut I, level: usize) -> Item
where
    I: Iterator<Item = pest::iterators::Pair<'i, Rule>>
{
    let mut items = vec![];
    let level_string = format!("({}){}", level, "  ".repeat(level));
    for pair in pairs {
        let rule = pair.as_rule();
        match rule {
            Rule::common_variable => {
                items.push(Expression::Variable(pair.as_span().as_str().to_string()).into());
            }
            Rule::true_kw => {
                items.push(Expression::True.into());
            }
            Rule::false_kw => {
                items.push(Expression::False.into());
            }
            Rule::is_kw => {
                items.push(SymbolType::Is.into());
            }
            Rule::output => {
                let value = depair(&mut pair.into_inner(), level + 1);
                if let Item::Expression(expr) = value {
                    items.push(Command::Say {
                        value: expr
                    }.into());
                } else {
                    panic!("Bad say {:?}", value);
                }
            }
            rule => {
                let original = pair.clone();
                let inner = pair.into_inner();
                let count = inner.count();
                if count == 0 {
                    println!("{}Empty pair: {:?}", level_string, original);
                }
                else if count == 1 {
                    println!("{}Depairing {:?}", level_string, rule);
                    items.push(depair(&mut original.into_inner(), level + 1));
                } else {
                    println!("{}List rule: {:?}", level_string, rule);
                    items.push(depair(&mut original.into_inner(), level + 1));
                }
            }
        }
    }
    match items.as_slice() {
        [Item::Expression(Expression::Variable(target)), Item::Symbol(SymbolType::Is), Item::Expression(value)] => {
            return Command::Assignment {
                target: target.to_string(),
                value: value.clone(),
            }.into();
        }
        _ => {}
    }
    match items.len() {
        0 => SymbolType::Empty.into(),
        1 => items.get(0).unwrap().clone(),
        _ => {
            panic!("{}Many! {:?}", level_string, items);
        }
    }
}