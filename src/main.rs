#[macro_use]
extern crate nom;
#[macro_use]
extern crate log;
extern crate pretty_env_logger;

use nom::types::CompleteStr;

fn main() {
    println!("Hello, world!");
}

pub fn is_space(chr: char) -> bool {
  chr == ' ' || chr == '\t'
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

named!(word<CompleteStr, &str>,
    do_parse!(
        word: take_while!(is_alphanumeric) >>
        take_while!(is_space) >>
        (*word)
    ));

named!(line<CompleteStr, Vec<&str>>, many0!(word));
named!(lines<CompleteStr, Vec<Vec<&str>>>, many0!(
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
    Words(String)
}

#[derive(Debug)]
enum Command {
    Assignment { target: String, value: String },
    Increment { target: String }
}

fn parse(input: &str) {
    let raw_lines = lines(CompleteStr(input)).unwrap().1;
    println!("{:?}", raw_lines);
    let mut commands: Vec<Command> = Vec::new();
    for line in raw_lines {
        let mut symbols: Vec<SymbolType> = Vec::new();
        let mut words = String::new();
        for word in line {
            match word.to_lowercase().as_str() {
                "is" => {
                    if !words.is_empty() {
                        symbols.push(SymbolType::Words(words.trim().to_string()));
                    }
                    symbols.push(SymbolType::Is);
                    words = String::new();
                },
                "build" => {
                    if !words.trim().is_empty() {
                        symbols.push(SymbolType::Words(words.trim().to_string()));
                    }
                    symbols.push(SymbolType::Build);
                    words = String::new();
                },
                "up" => {
                    if !words.is_empty() {
                        symbols.push(SymbolType::Words(words.trim().to_string()));
                    }
                    symbols.push(SymbolType::Up);
                    words = String::new();
                },
                other => {
                    words += " ";
                    words += &other;
                }
            }
        }
        if !words.is_empty() {
            symbols.push(SymbolType::Words(words.trim().to_string()));
        }
        println!("{:?}", symbols);

        match symbols.as_slice() {
            [SymbolType::Words(target), SymbolType::Is, SymbolType::Words(value)] => {
                commands.push(Command::Assignment { target: target.to_string(), value: value.to_string()});
            }
            [SymbolType::Build, SymbolType::Words(target), SymbolType::Up] => {
                commands.push(Command::Increment { target: target.to_string()});
            }
            _ => {
                error!("Don't recognise command sequence {:?}", symbols);
            }
        }
    }
    println!("{:?}", commands);
}

#[test]
fn test_counting() {
    pretty_env_logger::init();
    let program = "Limit is 100
Counter is 0
Fizz is 3
Buzz is 5
Until Counter is Limit
	Build Counter up
End";
    parse(program);
}