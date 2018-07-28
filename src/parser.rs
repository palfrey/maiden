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

named!(proper_variable<CompleteStr, String>,
    do_parse!( // FIXME: Expand to multi-word Proper variables
        first: take_while_m_n!(1, 1, char::is_uppercase) >>
        rest: take_while1!(char::is_lowercase) >>
        (format!("{}{}", first, rest))
    )
);

named!(variable<CompleteStr, String>, alt_complete!(
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
        (format!("{} {}", keyword, word))
    ) => {|s| s } |
    proper_variable => {|s| s }
));

named!(word<CompleteStr, SymbolType>,
    alt_complete!(
        do_parse!(
            target: variable >>
            take_while1!(is_space) >>
            tag_no_case!("taking") >>
            take_while1!(is_space) >>
            first_arg: variable >>
            other_args: many0!(
                do_parse!(
                    take_while!(is_space) >>
                    tag!(",") >>
                    take_while1!(is_space) >>
                    var: variable >>
                    (var)
                )) >>
            (target, first_arg, other_args)
        ) => {|(target, first_arg, mut other_args): (String, String, Vec<String>)| {
            other_args.insert(0, first_arg);
            SymbolType::Taking{target, args: other_args}
        }} |
        tag_no_case!("is as high as") => {|_| SymbolType::GreaterThanOrEqual} |
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
            tag_no_case!("end") | tag_no_case!("around we go")
        ) => {|_| SymbolType::Next} |
        tag_no_case!("take it to the top") => {|_| SymbolType::Continue} |
        tag_no_case!("give back") => {|_| SymbolType::Return} |
        tag_no_case!("takes") => {|_| SymbolType::Takes} |
        tag_no_case!("without") => {|_| SymbolType::Subtract} |
        tag_no_case!("into") => {|_| SymbolType::Where} |
        tag_no_case!("put") => {|_| SymbolType::Put} |
        tag!("nothing") => {|_| SymbolType::Integer(0) } |
        take_while1!(char::is_numeric) => {|n: CompleteStr| SymbolType::Integer(n.parse::<u32>().unwrap())} |
        variable => {|s| SymbolType::Variable(s) } |
        tag!(",") => {|_| SymbolType::Comma} |
        do_parse!(
            tag!("\"") >> 
            phrase: take_while!(string_character) >>
            tag!("\"") >>
            (phrase)
        ) => {|p: CompleteStr| SymbolType::String(p.to_string())} |
        take_while1!(is_alphabetic) => {|word: CompleteStr| SymbolType::Words(vec![word.to_string()])}
    ));

named!(poetic_number_literal_core<CompleteStr, (String, Vec<CompleteStr>)>,
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
    return Ok((rest, vec![SymbolType::Variable(target), SymbolType::Is, literal]));
}

named!(pub line<CompleteStr, Vec<SymbolType>>, alt_complete!(
    poetic_number_literal => {|s| s } |
    many1!(do_parse!(
        word: word >>
        take_while!(is_space) >>
        (word)
    )) => {|s| s }
));

named!(lines<CompleteStr, Vec<Vec<SymbolType>>>, many0!(
    alt!(
        take_while1!(is_newline) => {|_| vec![SymbolType::Newline]} |
        do_parse!(
            a_line: line >>
            take_while_m_n!(0, 1, is_newline) >>
            take_while!(is_space) >>
            (a_line)
        ) => {|l| l }
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

fn build_binop(output: &mut Vec<Expression>, describe: &str) -> Result<(Box<Expression>, Box<Expression>)> {
    if output.len() < 2 {
        bail!(ErrorKind::UnbalancedExpression(describe.to_string()));
    }
    let second = output.pop().expect("second");
    let first = output.pop().expect("first");
    return Ok((Box::new(first), Box::new(second)));
}

fn parse_expression(items: Vec<&SymbolType>) -> Result<Expression> {
    // based off of https://en.wikipedia.org/wiki/Shunting-yard_algorithm
    let mut output: Vec<Expression> = Vec::new();
    let mut operators: Vec<&SymbolType> = Vec::new();
    let describe = format!("{:?}", items);
    for item in items {
        match item {
            &SymbolType::Words(_) => {
                 output.push(evaluate(item)?);
            },
            &SymbolType::Is | &SymbolType::GreaterThanOrEqual | &SymbolType::Subtract | &SymbolType::And => {
                operators.push(item);
            }
            &SymbolType::Variable(ref name) => {
                output.push(Expression::Variable(name.clone()));
            }
            &SymbolType::String(ref phrase) => {
                output.push(Expression::String(phrase.clone()));
            }
            &SymbolType::Integer(ref val) => {
                output.push(Expression::Integer(*val as i32));
            }
            &SymbolType::Taking{ref target, ref args} => {
                output.push(Expression::Call(target.to_string(), args.iter().map(|s| Expression::Variable(s.to_string())).collect()));
            }
            _ => {
                error!("No parse for {:?}", item);
            }
        }
    }
    operators.reverse();
    debug!("operators: {:?}", operators);
    debug!("output: {:?}", output);
    for op in operators {
        match op {
            &SymbolType::Is => {
                let (first, second) = build_binop(&mut output, &describe)?;
                output.push(Expression::Is(first, second));
            }
            &SymbolType::GreaterThanOrEqual => {
                let (first, second) = build_binop(&mut output, &describe)?;
                output.push(Expression::GreaterThanOrEqual(first, second));
            }
            &SymbolType::Subtract => {
                let (first, second) = build_binop(&mut output, &describe)?;
                output.push(Expression::Subtract(first, second));
            }
            &SymbolType::And => {
                let (first, second) = build_binop(&mut output, &describe)?;
                output.push(Expression::And(first, second));
            }
            _ => {
                error!("No operation for {:?}", op);
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

fn build_next(commands: &mut Vec<Command>, loop_starts: &mut Vec<usize>) {
    let loop_start = loop_starts.pop().expect("loop_starts");
    let loop_len = commands.len();
    match commands.index_mut(loop_start) {
        Command::Until {loop_end: ref mut loop_end, expression: _} => {
            loop_end.get_or_insert(loop_len);
        }
        Command::While {loop_end: ref mut loop_end, expression: _} => {
            loop_end.get_or_insert(loop_len);
        }
        _ => {
            panic!("loop to non-loop command");
        }
    }
    commands.push(Command::Next {loop_start: loop_start});
}

fn build_return(commands: &mut Vec<Command>, loop_starts: &mut Vec<usize>) {
    let loop_start = loop_starts.pop().expect("loop_starts");
    let loop_len = commands.len();
    match commands.index_mut(loop_start) {
        Command::Until {loop_end: ref mut loop_end, expression: _} => {
            loop_end.get_or_insert(loop_len);
        }
        Command::While {loop_end: ref mut loop_end, expression: _} => {
            loop_end.get_or_insert(loop_len);
        }
        _ => {
            panic!("loop to non-loop command");
        }
    }
    commands.push(Command::Next {loop_start: loop_start});
}

pub fn parse(input: &str) -> Result<Vec<Command>> {
    let raw_lines = lines(CompleteStr(input)).unwrap();
    if raw_lines.0.len() > 0 {
        bail!(ErrorKind::UnparsedText(raw_lines.0.deref().to_string()));
    }
    debug!("{:?}", raw_lines);
    let mut commands: Vec<Command> = Vec::new();
    let mut loop_starts: Vec<usize> = Vec::new();
    let mut func_starts: Vec<usize> = Vec::new();
    let mut if_starts: Vec<usize> = Vec::new();
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
                build_next(&mut commands, &mut loop_starts);
            },
            [SymbolType::Continue] => {
                let loop_start = loop_starts.last().expect("loop_starts");
                commands.push(Command::Next {loop_start: *loop_start});
            }
            [SymbolType::Newline] => {
                if !if_starts.is_empty() {
                    let if_start = if_starts.pop().expect("if_starts");
                    let if_len = commands.len();
                    match commands.index_mut(if_start) {
                        Command::If {expression: _, if_end: ref mut if_end} => {
                            if_end.get_or_insert(if_len);
                        }
                        _ => {
                            panic!("return to non-if command");
                        }
                    }
                }
                else if !loop_starts.is_empty() {
                    build_next(&mut commands, &mut loop_starts);
                }
                else if !func_starts.is_empty() {
                    let func_start = func_starts.pop().expect("func_starts");
                    let func_len = commands.len();
                    match commands.index_mut(func_start) {
                        Command::FunctionDeclaration {name: _, args: _, func_end: ref mut func_end} => {
                            func_end.get_or_insert(func_len);
                        }
                        _ => {
                            panic!("return to non-func command");
                        }
                    }
                    commands.push(Command::EndFunction);
                }
                else {
                    error!("Bad double-newline");
                }
            }
            _ => {
                // Better done with slice patterns once they stablise (see https://github.com/rust-lang/rust/issues/23121)
                if section[0] == SymbolType::Say && section.len() > 1 {
                    let expression_seq: Vec<&SymbolType> = section.iter().skip(1).collect();
                    let expression = parse_expression(expression_seq)?;
                    commands.push(Command::Say{value: expression});
                }
                else if section.len() > 1 && section[1] == SymbolType::Is {
                    if let SymbolType::Variable(ref target) = section[0] {
                        let expression_seq: Vec<&SymbolType> = section.iter().skip(2).collect();
                        let expression = parse_expression(expression_seq)?;
                        commands.push(Command::Assignment { target: target.to_string(), value: expression});
                    }
                    else {
                        error!("Bad 'is' section: {:?}", section);
                    }
                }
                else if section[0] == SymbolType::Until && section.len() > 1 {
                    loop_starts.push(commands.len());
                    let expression_seq: Vec<&SymbolType> = section.iter().skip(1).collect();
                    let expression = parse_expression(expression_seq)?;
                    commands.push(Command::Until { expression: expression, loop_end: None});
                }
                else if section[0] == SymbolType::While && section.len() > 1 {
                    loop_starts.push(commands.len());
                    let expression_seq: Vec<&SymbolType> = section.iter().skip(1).collect();
                    let expression = parse_expression(expression_seq)?;
                    commands.push(Command::While { expression: expression, loop_end: None});
                }
                else if section[0] == SymbolType::If && section.len() > 1 {
                    if_starts.push(commands.len());
                    let expression_seq: Vec<&SymbolType> = section.iter().skip(1).collect();
                    let expression = parse_expression(expression_seq)?;
                    commands.push(Command::If { expression: expression, if_end: None});
                }
                else if section.len() > 3 && section[0] == SymbolType::Put && section[section.len()-2] == SymbolType::Where {
                    if let SymbolType::Variable(ref target) = section[section.len()-1] {
                        let expression_seq: Vec<&SymbolType> = section.iter().skip(1).take(section.len()-3).collect();
                        let expression = parse_expression(expression_seq)?;
                        commands.push(Command::Assignment { target: target.to_string(), value: expression});
                    }
                    else {
                        error!("Bad 'put' section: {:?}", section);
                    }
                }
                else if section.len() > 2 && section[1] == SymbolType::Takes {
                    if let SymbolType::Variable(ref name) = section[0] {
                        let mut var_seq = section.iter().skip(2);
                        let mut args = vec![];
                        loop {
                            if let Some(SymbolType::Variable(ref arg)) = var_seq.next() {
                                args.push(Expression::Variable(arg.to_string()));
                                match var_seq.next() {
                                    Some(sym) => {
                                        if sym != &SymbolType::And {
                                            error!("Bad 'function declaration' section: {:?} {:?}", sym, section);
                                        }
                                    }
                                    None => {
                                        break;
                                    }
                                }
                            }
                            else {
                                error!("Bad 'function declaration' section: {:?}", section);
                            }
                        }
                        func_starts.push(commands.len());
                        commands.push(Command::FunctionDeclaration{name:name.to_string(), args, func_end: None});
                    }
                    else {
                        error!("Bad 'function declaration' section: {:?}", section);
                    }
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