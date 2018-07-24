use nom::types::CompleteStr;
use std::ops::IndexMut;
use std::ops::Deref;
use common::*;

fn is_space(chr: char) -> bool {
  chr == ' ' || chr == '\t' || chr == ','
}

fn is_newline(chr: char) -> bool {
  chr == '\r' || chr == '\n'
}

fn is_word_character(chr: char) -> bool {
  !is_space(chr) && !is_newline(chr)
}

fn is_quote(chr: char) -> bool {
    chr == '\"' || chr == '\''
}

fn string_character(chr: char) -> bool {
  !is_newline(chr) && !is_quote(chr)
}

named!(word<CompleteStr, SymbolType>,
    alt_complete!(
        tag_no_case!("is") => {|_| SymbolType::Is} |
        tag_no_case!("if") => {|_| SymbolType::If} |
        tag_no_case!("build") => {|_| SymbolType::Build} |
        tag_no_case!("up") => {|_| SymbolType::Up} |
        tag_no_case!("say") => {|_| SymbolType::Say} |
        tag_no_case!("shout") => {|_| SymbolType::Say} |
        tag_no_case!("whisper") => {|_| SymbolType::Say} |
        tag_no_case!("and") => {|_| SymbolType::And} |
        tag_no_case!("while") => {|_| SymbolType::While} |
        tag_no_case!("until") => {|_| SymbolType::Until} |
        tag_no_case!("end") => {|_| SymbolType::Next} |
        tag_no_case!("around we go") => {|_| SymbolType::Next} |
        tag_no_case!("take it to the top") => {|_| SymbolType::Next} |
        tag_no_case!("taking") => {|_| SymbolType::Taking} |
        tag_no_case!("give back") => {|_| SymbolType::Return} |
        tag_no_case!("takes") => {|_| SymbolType::Takes} |
        do_parse!(
            tag!("\"") >> 
            phrase: take_while!(string_character) >>
            tag!("\"") >>
            (phrase)
        ) => {|p: CompleteStr| SymbolType::String(p.to_string())} |
        take_while!(is_word_character) => {|word: CompleteStr| SymbolType::Words(word.to_lowercase())}
    ));

named!(pub line<CompleteStr, Vec<SymbolType>>, many0!(
    do_parse!(
        word: word >>
        take_while!(is_space) >>
        (word)
    )));

named!(lines<CompleteStr, Vec<Vec<SymbolType>>>, many0!(
    do_parse!(
        a_line: line >>
        take_while!(is_newline) >>
        (a_line)
    )));

fn compact_words(line: Vec<SymbolType>) -> Vec<SymbolType> {
    let mut symbols: Vec<SymbolType> = Vec::new();
    let mut words = String::new();
    for word in line {
        match word {
            SymbolType::Words(other) => {
                words += " ";
                words += &other;
            }
            symbol => {
                if !words.trim().is_empty() {
                    symbols.push(SymbolType::Words(words.trim().to_string()));
                    words = String::new();
                }
                symbols.push(symbol);
            }
        }
    }
    if !words.is_empty() {
        symbols.push(SymbolType::Words(words.trim().to_string()));
    }
    debug!("{:?}", symbols);
    return symbols;
}

pub fn parse(input: &str) -> Result<Vec<Command>> {
    let raw_lines = lines(CompleteStr(input)).unwrap();
    if raw_lines.0.len() > 0 {
        bail!(ErrorKind::UnparsedText(raw_lines.0.deref().to_string()));
    }
    debug!("{:?}", raw_lines);
    let mut commands: Vec<Command> = Vec::new();
    let mut loop_starts: Vec<usize> = Vec::new();
    for raw_symbols in raw_lines.1 {
        let mut symbols = compact_words(raw_symbols);

        match symbols.as_slice() {
            [SymbolType::Words(target), SymbolType::Is, SymbolType::Words(value)] => {
                commands.push(Command::Assignment { target: target.to_string(), value: SymbolType::Words(value.to_string())});
            }
            [SymbolType::Build, SymbolType::Words(target), SymbolType::Up] => {
                commands.push(Command::Increment { target: target.to_string()});
            }
            [SymbolType::And, SymbolType::Next] | [SymbolType::Next] => {
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
                commands.push(Command::UntilIs { target: target.to_string(), value: SymbolType::Words(value.to_string()), loop_end: None});
            }
            [SymbolType::Say, SymbolType::String(value)] => {
                commands.push(Command::Say{value: SymbolType::String(value.to_string())});
            }
            _ => {
                error!("Don't recognise command sequence {:?}", symbols);
            }
        }
    }
    return Ok(commands);
}

#[test]
fn multi_word_quote_parse() {
    assert_eq!(
        (CompleteStr(""), vec![SymbolType::Say, SymbolType::String("shout let it all out".to_string())]),
        line(CompleteStr("say \"shout let it all out\"")).unwrap());
}