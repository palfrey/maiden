#[macro_use]
extern crate nom;
#[macro_use]
extern crate log;
extern crate pretty_env_logger;

use nom::types::CompleteStr;
use std::io::{Cursor, Write, self};

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
    UntilIs { target: String, value: String },
    Increment { target: String },
    Next
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
                commands.push(Command::Next);
            }
            [SymbolType::Until, SymbolType::Words(target), SymbolType::Is, SymbolType::Words(value)] => {
                commands.push(Command::UntilIs { target: target.to_string(), value: value.to_string()});
            }
            _ => {
                error!("Don't recognise command sequence {:?}", symbols);
            }
        }
    }
    return commands;
}

fn run(commands: Vec<Command>, writer: &mut Write) -> Result<(), io::Error> {
    writeln!(writer, "{:?}", commands)?;
    return Ok(());
}

fn test_program(program: &str) {
    pretty_env_logger::try_init().unwrap_or(());
    let commands = parse(program);
    let mut writer = Cursor::new(Vec::new());
    run(commands, &mut writer).unwrap();
    writer.set_position(0);
    println!("{}", std::str::from_utf8(writer.get_ref()).unwrap());
}

#[test]
fn test_counting() {
    let program = "Limit is 100
Counter is 0
Fizz is 3
Buzz is 5
Until Counter is Limit
	Build Counter up
End";
    test_program(program);
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
    test_program(program);
}