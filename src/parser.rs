use nom::types::CompleteStr;
use std::ops::IndexMut;
use std::ops::Deref;
use common::*;
use pretty_env_logger;
use nom;

fn is_space(chr: char) -> bool {
  chr == ' ' || chr == '\t'
}

fn is_newline(chr: char) -> bool {
  chr == '\r' || chr == '\n'
}

fn is_quote(chr: char) -> bool {
    chr == '\"' || chr == '\''
}

fn string_character(chr: char) -> bool {
  !is_newline(chr) && !is_quote(chr)
}

fn is_alphabetic(chr: char) -> bool {
  (chr >= 'A' && chr <= 'Z') || (chr >= 'a' && chr <= 'z')
}

named!(proper_variable<CompleteStr, SymbolType>,
    do_parse!( // FIXME: Expand to multi-word Proper variables
        first: take_while_m_n!(1, 1, char::is_uppercase) >>
        rest: take_while1!(char::is_lowercase) >>
        (SymbolType::Variable(format!("{}{}", first, rest)))
    )
);

named!(word<CompleteStr, SymbolType>,
    alt_complete!(
        tag_no_case!("is") => {|_| SymbolType::Is} |
        tag_no_case!("if") => {|_| SymbolType::If} |
        tag_no_case!("build") => {|_| SymbolType::Build} |
        tag_no_case!("up") => {|_| SymbolType::Up} |
        alt_complete!(
            tag_no_case!("say") | tag_no_case!("shout") | tag_no_case!("whisper")
        ) => {|_| SymbolType::Say} |
        tag_no_case!("and") => {|_| SymbolType::And} |
        tag_no_case!("while") => {|_| SymbolType::While} |
        tag_no_case!("until") => {|_| SymbolType::Until} |
        alt_complete!(
            tag_no_case!("end") | tag_no_case!("around we go") | tag_no_case!("take it to the top")
        ) => {|_| SymbolType::Next} |
        tag_no_case!("taking") => {|_| SymbolType::Taking} |
        tag_no_case!("give back") => {|_| SymbolType::Return} |
        tag_no_case!("takes") => {|_| SymbolType::Takes} |
        tag_no_case!("without") => {|_| SymbolType::Subtract} |
        tag_no_case!("into") => {|_| SymbolType::Where} |
        tag_no_case!("put") => {|_| SymbolType::Put} |
        tag_no_case!("as high as") => {|_| SymbolType::GreaterThanOrEqual} |
        tag!("nothing") => {|_| SymbolType::Integer(0) } |
        take_while1!(char::is_numeric) => {|n: CompleteStr| SymbolType::Integer(n.parse::<u32>().unwrap())} |
        do_parse!(
            keyword: alt_complete!(
                tag_no_case!("a") |
                tag_no_case!("an") |
                tag_no_case!("the") |
                tag_no_case!("my") |
                tag_no_case!("your")
            ) >>
            take_while1!(is_space) >>
            word: take_while1!(char::is_lowercase) >>
            (SymbolType::Variable(format!("{} {}", keyword, word)))
        ) => {|s| s } |
        proper_variable => {|s| s } |
        tag!(",") => {|_| SymbolType::Comma} |
        do_parse!(
            tag!("\"") >> 
            phrase: take_while!(string_character) >>
            tag!("\"") >>
            (phrase)
        ) => {|p: CompleteStr| SymbolType::String(p.to_string())} |
        take_while1!(is_alphabetic) => {|word: CompleteStr| SymbolType::Words(vec![word.to_string()])}
    ));

named!(poetic_number_literal_core<CompleteStr, (SymbolType, Vec<CompleteStr>)>,
    do_parse!(
        pv: proper_variable >>
        take_while1!(is_space) >>
        tag_no_case!("is") >>
        words: many1!(
            do_parse!(
                take_while1!(is_space) >>
                word: take_while1!(is_alphabetic) >>
                (word)
            )
        ) >>
        (pv, words)
    )
);

fn poetic_number_literal(input: CompleteStr) -> nom::IResult<CompleteStr, Vec<SymbolType>> {
    let (rest, (target, words)) = poetic_number_literal_core(input)?;
    let literal = SymbolType::Words(words.iter().map(|s| s.to_string()).collect());
    return Ok((rest, vec![target, SymbolType::Is, literal]));
}

named!(pub line<CompleteStr, Vec<SymbolType>>, alt_complete!(
    poetic_number_literal => {|s| s } |
    many0!(do_parse!(
        word: word >>
        take_while!(is_space) >>
        (word)
    )) => {|s| s }
));

named!(lines<CompleteStr, Vec<Vec<SymbolType>>>, many0!(
    do_parse!(
        a_line: line >>
        take_while!(is_newline) >>
        take_while!(is_space) >>
        (a_line)
    )));

fn compact_words(line: Vec<SymbolType>) -> Vec<SymbolType> {
    let mut symbols: Vec<SymbolType> = Vec::new();
    let mut words = Vec::new();
    for word in line {
        match word {
            SymbolType::Words(other) => {
                words.extend_from_slice(&other);
            }
            symbol => {
                if !words.is_empty() {
                    symbols.push(SymbolType::Words(words));
                    words = Vec::new();
                }
                symbols.push(symbol);
            }
        }
    }
    if !words.is_empty() {
        symbols.push(SymbolType::Words(words));
    }
    debug!("{:?}", symbols);
    return symbols;
}

fn evaluate(value: &SymbolType) -> Result<Expression> {
    match value {
        SymbolType::Words(words) => {
            if words.len() == 1 {
                if words[0] == "nothing" {
                    return Ok(Expression::Integer(0));
                }
                let as_int = words[0].parse::<i32>();
                if let Ok(int) = as_int {
                    return Ok(Expression::Integer(int));
                }
            }
            let mut number = 0i32;
            for word in words {
                number *= 10;
                let len: i32 = (word.len() % 10) as i32;
                number += len;
            }
            return Ok(Expression::Integer(number));
        }
        SymbolType::String(phrase) => {
            return Ok(Expression::String(phrase.to_string()));
        }
        _ => {
            warn!("Evaluate: '{:?}'", value);
            unimplemented!();
        }
    }
}

fn parse_expression(items: Vec<&SymbolType>) -> Result<Expression> {
    // based off of https://en.wikipedia.org/wiki/Shunting-yard_algorithm
    let mut output: Vec<Expression> = Vec::new();
    let mut operators: Vec<&SymbolType> = Vec::new();
    let describe = format!("{:?}", items);
    for item in items {
        match item {
            &SymbolType::Words(ref phrase) => {
                 output.push(evaluate(item)?);
            },
            &SymbolType::Is => {
                operators.push(item);
            }
            &SymbolType::Variable(ref name) => {
                output.push(Expression::Variable(name.clone()));
            }
            &SymbolType::Integer(ref val) => {
                output.push(Expression::Integer(*val as i32));
            }
            _ => {
                warn!("No parse for {:?}", item);
            }
        }
    }
    operators.reverse();
    debug!("operators: {:?}", operators);
    debug!("output: {:?}", output);
    for op in operators {
        match op {
            &SymbolType::Is => {
                let second = output.pop().expect("second");
                let first = output.pop().expect("first");
                output.push(Expression::Is(Box::new(first), Box::new(second)));
            }
            _ => {
                warn!("No operation for {:?}", op);
            }
        }
    }
    if output.len() == 1 {
        return Ok(output[0].clone());
    }
    else {
        bail!(ErrorKind::UnbalancedExpression(describe));
    }
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
        let section = {
            let mut first = 0;
            if symbols[first] == SymbolType::And {
                first = 1;
            }
            if symbols[symbols.len()-1] == SymbolType::Comma {
                &symbols[first..symbols.len()-1]
            }
            else {
                &symbols[first..]
            }
        };
        match section {
            [SymbolType::Build, SymbolType::Variable(target), SymbolType::Up] => {
                commands.push(Command::Increment { target: target.to_string()});
            }
            [SymbolType::Next] => {
                let loop_start = loop_starts.pop().expect("loop_starts");
                let loop_len = commands.len();
                match commands.index_mut(loop_start) {
                    Command::Until {loop_end: ref mut loop_end, expression: _} => {
                        loop_end.get_or_insert(loop_len);
                    }
                    _ => {
                        panic!("loop to non-loop command");
                    }
                }
                commands.push(Command::Next {loop_start: loop_start});
            }
            [SymbolType::Say, SymbolType::String(value)] => {
                commands.push(Command::Say{value: Expression::String(value.to_string())});
            }
            _ => {
                // Better done with slice patterns once they stablise (see https://github.com/rust-lang/rust/issues/23121)
                if section[1] == SymbolType::Is {
                    if let SymbolType::Variable(ref target) = section[0] {
                        let expression_seq: Vec<&SymbolType> = section.iter().skip(2).collect();
                        let expression = parse_expression(expression_seq)?;
                        commands.push(Command::Assignment { target: target.to_string(), value: expression});
                    }
                }
                else if section[0] == SymbolType::Until && section.len() > 1 {
                    loop_starts.push(commands.len());
                    let expression_seq: Vec<&SymbolType> = section.iter().skip(1).collect();
                    let expression = parse_expression(expression_seq)?;
                    commands.push(Command::Until { expression: expression, loop_end: None});
                }
                else {
                    error!("Don't recognise command sequence {:?}", section);
                }
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

#[test]
fn check_evaluate() {
    pretty_env_logger::try_init().unwrap_or(());
    assert_eq!(evaluate(&SymbolType::Words(vec!["a".to_string(), "lovestruck".to_string(), "ladykiller".to_string()])).unwrap(), Expression::Integer(100));
    assert_eq!(evaluate(&SymbolType::Words(vec!["nothing".to_string()])).unwrap(), Expression::Integer(0));
} 