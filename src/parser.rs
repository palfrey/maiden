use common::Command;
use nom::types::CompleteStr;
use std::ops::IndexMut;

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

named!(pub line<CompleteStr, Vec<String>>, many0!(word));
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

pub fn parse(input: &str) -> Vec<Command> {
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