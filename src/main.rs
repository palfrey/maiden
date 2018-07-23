#![allow(non_shorthand_field_patterns)]

#[macro_use]
extern crate nom;
#[macro_use]
extern crate log;
extern crate pretty_env_logger;

use nom::types::CompleteStr;
use std::io::{Cursor, Write, self};
use std::collections::HashMap;
use std::ops::IndexMut;

fn main() {
    println!("Hello, world!");
}

pub fn is_space(chr: char) -> bool {
  chr == ' ' || chr == '\t' || chr == ','
}

pub fn is_newline(chr: char) -> bool {
  chr == '\r' || chr == '\n'
}

pub fn is_alphabetic(chr: char) -> bool {
  (chr >= 'A' && chr <= 'Z') || (chr >= 'a' && chr <= 'z')
}

pub fn is_digit(chr: char) -> bool {
  chr >= '0' && chr <= '9'
}

pub fn is_alphanumeric(chr: char) -> bool {
  is_alphabetic(chr) || is_digit(chr)
}

named!(word<CompleteStr, String>,
    do_parse!(
        word: take_while!(is_alphanumeric) >>
        take_while!(is_space) >>
        (word.to_lowercase())
    ));

named!(line<CompleteStr, Vec<String>>, many0!(word));
named!(lines<CompleteStr, Vec<Vec<String>>>, many0!(
    do_parse!(
        a_line: line >>
        take_while!(is_newline) >>
        (a_line)
    )));

#[derive(Debug)]
enum SymbolType {
    Is,
    Build,
    Up,
    Until,
    Next,
    Words(String)
}

#[derive(Debug)]
enum Command {
    Assignment { target: String, value: String },
    UntilIs { target: String, value: String, loop_end: Option<usize> },
    Increment { target: String },
    Next { loop_start: usize}
}

macro_rules! push_symbol {
    ($symbol:ident, $words:ident, $symbols:ident) => {
        {
            if !$words.trim().is_empty() {
                $symbols.push(SymbolType::Words($words.trim().to_string()));
            }
            $symbols.push(SymbolType::$symbol);
            $words = String::new();
        }
    }
}

fn parse(input: &str) -> Vec<Command> {
    let raw_lines = lines(CompleteStr(input)).unwrap().1;
    debug!("{:?}", raw_lines);
    let mut commands: Vec<Command> = Vec::new();
    let mut loop_starts: Vec<usize> = Vec::new();
    for line in raw_lines {
        let mut symbols: Vec<SymbolType> = Vec::new();
        let mut words = String::new();
        match line.iter().map(|x| x.as_str()).collect::<Vec<&str>>().as_slice() {
            ["and", "around", "we", "go"] => {
                push_symbol!(Next, words, symbols)
            },
            _ => {
                for word in line {
                    match word.as_str() {
                        "is" => push_symbol!(Is, words, symbols),
                        "build" => push_symbol!(Build, words, symbols),
                        "up" => push_symbol!(Up, words, symbols),
                        "until" => push_symbol!(Until, words, symbols),
                        "end" => push_symbol!(Next, words, symbols),
                        other => {
                            words += " ";
                            words += &other;
                        }
                    }
                }
            }
        };
        if !words.is_empty() {
            symbols.push(SymbolType::Words(words.trim().to_string()));
        }
        debug!("{:?}", symbols);

        match symbols.as_slice() {
            [SymbolType::Words(target), SymbolType::Is, SymbolType::Words(value)] => {
                commands.push(Command::Assignment { target: target.to_string(), value: value.to_string()});
            }
            [SymbolType::Build, SymbolType::Words(target), SymbolType::Up] => {
                commands.push(Command::Increment { target: target.to_string()});
            }
            [SymbolType::Next] => {
                let loop_start = loop_starts.pop().unwrap();
                let loop_len = commands.len();
                match commands.index_mut(loop_start) {
                    Command::UntilIs {loop_end: ref mut loop_end, target: _, value: _ } => {
                        loop_end.get_or_insert(loop_len);
                    }
                    _ => {
                        panic!("loop to non-loop command");
                    }
                }
                commands.push(Command::Next {loop_start: loop_start});
            }
            [SymbolType::Until, SymbolType::Words(target), SymbolType::Is, SymbolType::Words(value)] => {
                loop_starts.push(commands.len());
                commands.push(Command::UntilIs { target: target.to_string(), value: value.to_string(), loop_end: None});
            }
            _ => {
                error!("Don't recognise command sequence {:?}", symbols);
            }
        }
    }
    return commands;
}

#[derive(Debug, PartialEq, Clone)]
enum Variable {
    Integer(i32),
    String(String)
}

fn evaluate(variables: &HashMap<String, Variable>, value: &str) -> Variable {
    if value == "nothing" {
        return Variable::Integer(0);
    }
    let as_int = value.parse::<i32>();
    if let Ok(int) = as_int {
        return Variable::Integer(int);
    }
    if let Some(val) = variables.get(value) {
        return val.clone();
    }
    let words = line(CompleteStr(value)).unwrap().1;
    let mut number = 0i32;
    for word in words.iter() {
        number *= 10;
        let len: i32 = (word.len() % 10) as i32;
        number += len;
    }
    return Variable::Integer(number);
}

fn run(commands: Vec<Command>, writer: &mut Write) -> Result<HashMap<String, Variable>, io::Error> {
    let mut pc = 0;
    let mut variables: HashMap<String, Variable> = HashMap::new();
    let mut total_instr = 0;
    loop {
        total_instr +=1;
        if total_instr > 1000 {
            panic!("Ran out of instr");
        }
        let command = match commands.get(pc) {
            Some(c) => c,
            None => break
        };
        info!("command: {:?}", command);
        match command {
            Command::Assignment {target, value} => {
                let val = evaluate(&variables, value);
                variables.insert(target.to_string(), val);
            }
            Command::Increment {target} => {
                let val = variables[target.as_str()].clone();
                info!("Value of {} is {:?}", target, val);
                match val {
                    Variable::Integer(x) => {
                        variables.insert(target.to_string(), Variable::Integer(x+1));
                    }
                    _ => {
                        error!("Attempt to increment non-integer '{}'", target);
                    }
                };
            }
            Command::UntilIs {target, value, loop_end} => {
                let target = variables[target.as_str()].clone();
                let value = evaluate(&variables, value);
                if target == value {
                    pc = loop_end.expect("loop_end");
                }
            }
            Command::Next {loop_start} => {
                pc = loop_start-1;
            }
        }
        pc +=1;
    }
    return Ok(variables);
}

fn test_program(program: &str, end_variables: HashMap<String, Variable>) {
    pretty_env_logger::try_init().unwrap_or(());
    let commands = parse(program);
    let mut writer = Cursor::new(Vec::new());
    let variables = run(commands, &mut writer).unwrap();
    writer.set_position(0);
    let res = std::str::from_utf8(writer.get_ref()).unwrap();
    if res != "" {
        println!("{}", res);
    }
    assert_eq!(end_variables, variables);
}

// https://gist.github.com/DmitrySoshnikov/8439eac0a09d9fafe55a83c88d049117
macro_rules! hashmap(
    { $($key:expr => $value:expr),+, } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key.to_string(), $value);
            )+
            m
        }
     };
);

#[test]
fn test_counting() {
    let program = "Limit is 100
Counter is 0
Fizz is 3
Buzz is 5
Until Counter is Limit
	Build Counter up
End";
    let end_variables = hashmap! {
        "buzz" => Variable::Integer(5),
        "limit" => Variable::Integer(100),
        "counter" => Variable::Integer(100),
        "fizz" => Variable::Integer(3),
    };
    test_program(program, end_variables);
}

#[test]
fn test_rocking_counting() {
    let program = "Desire is a lovestruck ladykiller
My world is nothing
Fire is ice
Hate is water
Until my world is Desire,
Build my world up
And around we go";
    let end_variables = hashmap! {
        "my world" => Variable::Integer(100),
        "fire" => Variable::Integer(3),
        "hate" => Variable::Integer(5),
        "desire" => Variable::Integer(100),
    };
    test_program(program, end_variables);
}

#[test]
fn check_evaluate() {
    pretty_env_logger::try_init().unwrap_or(());
    let variables: HashMap<String, Variable> = HashMap::new();
    assert_eq!(evaluate(&variables, "a lovestruck ladykiller"), Variable::Integer(100));
    assert_eq!(evaluate(&variables, "nothing"), Variable::Integer(0));
}