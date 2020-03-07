#![allow(warnings)]

use pest::Parser;
use clap::{App, Arg};
use std::fs::File;
use std::io::{self, Read};
use std::collections::HashMap;
use pest::iterators::Pair;

mod common;
mod peg;
mod runner;

use crate::common::{Block, Expression, SymbolType, Command, Program, CommandLine};
use crate::peg::{Rockstar, Rule};

fn main() -> common::Result<()> {
    pretty_env_logger::try_init().unwrap_or(());
    let matches = App::new("Maiden")
        .version("1.0")
        .author("Tom Parker-Shemilt <palfrey@tevp.net>")
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
    eprintln!("{:#?}", program);
    runner::run(&program, &mut io::stdout())?;
    return Ok(());
}

#[derive(Debug, Clone, PartialEq)]
enum Item {
    Expression(Expression),
    Symbol(SymbolType),
    Command(Command),
    Block(Block)
}

impl Item {
    fn expr(self) -> Expression {
        if let Item::Expression(e) = self { e } else { panic!("Not an expression: {:?}", self)}
    }
    fn symbol(self) -> SymbolType {
        if let Item::Symbol(e) = self { e } else { panic!("Not a symboltype: {:?}", self)}
    }
    fn command(self) -> Command {
        if let Item::Command(e) = self { e } else { panic!("Not a command: {:?}", self)}
    }
    fn block(self) -> Block {
        if let Item::Block(e) = self { e } else { panic!("Not a block: {:?}", self)}
    }
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
    for line in pair.into_inner() {
        if line.as_rule() != Rule::line {
            panic!("Bad rule: {:?}", line.as_rule());
        }
        let (line_no, _) = line.as_span().start_pos().line_col();
        let depaired = depair(&mut line.into_inner(), 0);
        match depaired {
            Item::Command(command) => {
                commands.push(CommandLine{cmd: command, line: line_no});
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

impl From<Block> for Item {
    fn from(block: Block) -> Item {
        Item::Block(block)
    }
}

fn depair_core<'i>(pair: Pair<'i, Rule>, level: usize) -> Item {
    let rule = pair.as_rule();
    let level_string = format!("({}){}", level, "  ".repeat(level));
    match rule {
        Rule::EOI => {
            SymbolType::Empty.into()
        }
        Rule::common_variable | Rule::proper_variable => {
            Expression::Variable(pair.as_span().as_str().to_string()).into()
        }
        Rule::true_kw => {
            Expression::True.into()
        }
        Rule::false_kw => {
            Expression::False.into()
        }
        Rule::is_kw | Rule::is => {
            SymbolType::Is.into()
        }
        Rule::output => {
            let value = depair(&mut pair.into_inner(), level + 1).expr();
            Command::Say {
                value
            }.into()
        }
        Rule::string => {
            let mut value = pair.as_str();
            value = &value[1..value.len()-1];
            Expression::String(value.to_string()).into()
        }
        Rule::number => {
            let mut value = pair.as_str();
            Expression::Floating(value.parse::<f64>().unwrap()).into()
        }
        Rule::conditional => {
            let mut pairs: Vec<_> = pair.into_inner().collect();
            let expression = depair_core(pairs.remove(0), level + 1).expr();
            let first = pairs.remove(0);
            let consequent;
            let mut alternate;
            match first.as_rule() {
                Rule::consequent => {
                    consequent = Some(depair_core(first, level + 1).block());
                    alternate = if pairs.is_empty() { None } else { Some(depair_core(pairs.remove(0), level + 1).block()) };
                }
                Rule::alternate => {
                    consequent = None;
                    alternate = Some(depair_core(first, level + 1).block());
                }
                rule => {
                    panic!("Bad rule: {:?}", rule);
                }
            }
            Command::If {
                expression,
                then: consequent,
                otherwise: alternate
            }.into()
        }
        Rule::block => {
            eprintln!("{}Depairing Block", level_string);
            let items = depair_seq(&mut pair.into_inner(), level + 1);
            let mut commands = vec![];
            for item in items {
                commands.push(CommandLine {cmd: item.command(), line: 0});
            }
            Block { commands }.into()
        }
        Rule::equality_check => {
            eprintln!("{}Depairing equality_check", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1);
            if items.len() == 1 {
                return items.remove(0);
            }
            let is = items.remove(1);
            if is != Item::Symbol(SymbolType::Is) {
                panic!("Not is: {:?}", is);
            }
            Expression::Is(Box::new(items.remove(0).expr()), Box::new(items.remove(0).expr())).into()
        }
        Rule::statement => {
            let item = depair(&mut pair.into_inner(), level + 1);
            if let Item::Expression(Expression::Is(target, value)) = item {
                return Command::Assignment{target: *target, value: *value}.into();
            }
            item
        }
        Rule::assignment => {
            eprintln!("{}Depairing assignment", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1);
            if items.len() == 0 {
                panic!("Empty assignment!");
            }
            let target = items.remove(0).expr();
            match items.len() {
                1 => {
                    Command::Assignment{target, value: items.remove(0).expr()}.into()
                }
                2 => {
                    let operator = items.remove(0).symbol();
                    let first = Box::new(target.clone());
                    let second = items.remove(0).expr();
                    match operator {
                        SymbolType::Add => {
                            let expr = Expression::Add(
                                first,
                                Box::new(second)
                            );
                            Command::Assignment{target, value: expr}.into()
                        }
                        SymbolType::Subtract => {
                            let expr = Expression::Subtract(
                                first,
                                Box::new(second)
                            );
                            Command::Assignment{target, value: expr}.into()
                        }
                        SymbolType::Divide => {
                            let expr = Expression::Divide(
                                first,
                                Box::new(second)
                            );
                            Command::Assignment{target, value: expr}.into()
                        }
                        SymbolType::Is => {
                            Command::Assignment{target, value: second}.into()
                        }
                        _ => {
                            panic!("Bad assignment operator: {:?}", operator);
                        }
                    }
                }
                _ => {
                    panic!("Bad assignment: {:?}", items);
                }
            }
        }
        Rule::add => {
            SymbolType::Add.into()
        }
        Rule::subtract => {
            SymbolType::Subtract.into()
        }
        Rule::divide => {
            SymbolType::Divide.into()
        }
        Rule::pronoun => {
            Expression::Pronoun.into()
        }
        Rule::poetic_digits => {
            let value = pair.as_str();
            let mut number = 0;
            for word in value.split_whitespace() {
                if number > 0 {
                    number *= 10;
                }
                number += word.len();
            }
            Expression::Floating(number as f64).into()
        }
        rule => {
            let original = pair.clone();
            let inner = pair.into_inner();
            let count = inner.count();
            if count == 0 {
                if rule == Rule::alternate {
                    return Block { commands: vec![] }.into()
                }
                let (line_no, col_no) = original.as_span().start_pos().line_col();
                panic!("{}Empty pair at {}, {}: {:?}", level_string, line_no, col_no, original);
            }
            else if count == 1 {
                eprintln!("{}Depairing {:?}", level_string, rule);
                depair(&mut original.into_inner(), level + 1)
            } else {
                eprintln!("{}List rule: {:?}", level_string, rule);
                depair(&mut original.into_inner(), level + 1)
            }
        }
    }
}


fn depair<'i, I>(pairs: &'i mut I, level: usize) -> Item
where
    I: Iterator<Item = pest::iterators::Pair<'i, Rule>>
{
    let mut items = vec![];
    let level_string = format!("({}){}", level, "  ".repeat(level));
    for pair in pairs {
        let item = depair_core(pair, level);
        if item == SymbolType::Empty.into() {
            continue;
        }
        items.push(item);
    }
    match items.len() {
        0 => {
            eprintln!("{}Empty", level_string);
            SymbolType::Empty.into()
        }
        1 => items.get(0).unwrap().clone(),
        _ => {
            panic!("{}Many! {:?}", level_string, items);
        }
    }
}

fn depair_seq<'i, I>(pairs: &'i mut I, level: usize) -> Vec<Item>
where
    I: Iterator<Item = pest::iterators::Pair<'i, Rule>>
{
    let mut items = vec![];
    for pair in pairs {
        items.push(depair_core(pair, level));
    }
    return items;
}