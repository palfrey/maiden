// because nom macros
#![cfg_attr(feature = "cargo-clippy", allow(double_parens))]
#![cfg_attr(feature = "cargo-clippy", allow(double_comparisons))]

use common::*;
use nom;
use nom::types::CompleteStr;
use std::collections::HashMap;
use std::ops::IndexMut;

fn is_space(chr: char) -> bool {
    chr == ' ' || chr == '\t'
}

fn is_literal_spacing_character(chr: char) -> bool {
    !is_newline(chr) && !word_character(chr)
}

fn is_newline(chr: char) -> bool {
    chr == '\r' || chr == '\n'
}

fn is_not_newline(chr: char) -> bool {
    !is_newline(chr)
}

fn is_quote(chr: char) -> bool {
    chr == '\"' || chr == '\''
}

fn string_character(chr: char) -> bool {
    !is_newline(chr) && !is_quote(chr)
}

fn variable_character(chr: char) -> bool {
    chr.is_alphabetic()
}

fn word_character(chr: char) -> bool {
    variable_character(chr) || char::is_numeric(chr) || chr == '\'' || chr == '.'
}

fn is_digit(chr: char) -> bool {
    chr.is_digit(10)
}

named!(title_case<Span, String>,
    do_parse!(
        not!(keyword) >> // to shortcut the "Until Counter" case
        first: verify!(take!(1), |val: Span| val.fragment.chars().all(|c| c.is_uppercase())) >>
        rest: take_while!(variable_character) >>
        (format!("{}{}", first.fragment, rest.fragment))
    ));

named!(proper_variable<Span, String>,
    do_parse!(
        first: title_case >>
        rest: many0!(do_parse!(
            take_while1!(is_space) >>
            word: title_case >>
            (word)
        )) >>
        (format!("{}{}{}", first, if rest.is_empty() {""} else {" "}, rest.join(" ")))
    ));

named!(variable<Span, String>, alt_complete!(
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
        (format!("{} {}", keyword.fragment, word.fragment))
    ) => {|s| s } |
    proper_variable => {|s| s }
));

named!(keyword<Span, Span>, // single-words only
    alt_complete!(
        tag_no_case!("and") |
        tag_no_case!("build") |
        tag_no_case!("end") |
        tag_no_case!("else") |
        tag_no_case!("if") |
        tag_no_case!("into") |
        tag_no_case!("is") |
        tag_no_case!("minus") |
        tag_no_case!("put") |
        tag_no_case!("scream") |
        tag_no_case!("shout") |
        tag_no_case!("takes") |
        tag_no_case!("until") |
        tag_no_case!("up") |
        tag_no_case!("was") |
        tag_no_case!("while") |
        tag_no_case!("whisper") |
        tag_no_case!("with") |
        tag_no_case!("without")
    )
);

named!(null<Span, Span>,
    alt_complete!(
        tag_no_case!("nothing") |
        tag_no_case!("null") |
        tag_no_case!("gone") |
        tag_no_case!("nobody") |
        tag_no_case!("nowhere") |
        tag_no_case!("empty")
    )
);

named!(literal_word<Span, Span>,
    alt_complete!(
        null
    )
);

named!(compare(Span) -> SymbolType,
    alt_complete!(
        do_parse!(
            tag_no_case!("as") >>
            take_while1!(is_space) >>
            alt_complete!(
                tag_no_case!("high") | tag_no_case!("strong") | tag_no_case!("big")
            ) >>
            take_while1!(is_space) >>
            tag_no_case!("as") >>
            (())
        ) => {|_| SymbolType::GreaterThanOrEqual} |
        do_parse!(
            alt_complete!(
                tag_no_case!("less") | tag_no_case!("weaker") | tag_no_case!("lower") | tag_no_case!("smaller")
            ) >>
            take_while1!(is_space) >>
            tag_no_case!("than") >>
            (())
        ) => {|_| SymbolType::LessThan} |
        do_parse!(
            tag_no_case!("as") >>
            take_while1!(is_space) >>
            alt_complete!(
                tag_no_case!("low") | tag_no_case!("little") | tag_no_case!("small") | tag_no_case!("weak")
            ) >>
            take_while1!(is_space) >>
            tag_no_case!("as") >>
            (())
        ) => {|_| SymbolType::LessThanOrEqual} |
        do_parse!(
            alt_complete!(
                tag_no_case!("higher") | tag_no_case!("stronger") | tag_no_case!("bigger") | tag_no_case!("greater")
            ) >>
            take_while1!(is_space) >>
            tag_no_case!("than") >>
            (())
        ) => {|_| SymbolType::GreaterThan}
    )
);

named!(pronoun(Span) -> SymbolType,
    value!(SymbolType::Pronoun,
        alt_complete!(
            tag_no_case!("it") |
            tag_no_case!("he") | tag_no_case!("she") | tag_no_case!("him") | tag_no_case!("her") | tag_no_case!("they") | tag_no_case!("them") | 
            tag_no_case!("ze") | tag_no_case!("hir") | tag_no_case!("zie") | tag_no_case!("zir") | tag_no_case!("xe") | tag_no_case!("xem") | 
            tag_no_case!("ve") | tag_no_case!("ver")
        )
    )
);

named!(literal(Span) -> SymbolType,
    alt_complete!(
        tag_no_case!("mysterious") => {|_| SymbolType::Mysterious} |
        null => {|_| SymbolType::Null} |
        alt_complete!(
            tag_no_case!("true") |
            tag_no_case!("right") |
            tag_no_case!("yes") |
            tag_no_case!("ok")
        ) => {|_| SymbolType::True} |
        alt_complete!(
            tag_no_case!("false") |
            tag_no_case!("lies") |
            tag_no_case!("wrong") |
            tag_no_case!("no")
        ) => {|_| SymbolType::False} |
        do_parse!(
            minus: opt!(tag!("-")) >>
            before: take_while!(is_digit) >>
            tag!(".") >>
            after: take_while1!(is_digit) >>
            (minus, before, after)
        ) => {|(minus, before, after):(Option<Span>, Span, Span)|
            if minus.is_some() {
                SymbolType::Floating(format!("-{}.{}", before.fragment, after.fragment))
            } else {
                SymbolType::Floating(format!("{}.{}", before.fragment, after.fragment))
            }}|
        do_parse!(
            minus: opt!(tag!("-")) >>
            val: take_while1!(is_digit) >>
            (minus, val)
        ) => {|(minus, val):(Option<Span>, Span)|
            if minus.is_some() {
                SymbolType::Integer(format!("-{}", val.fragment))
            } else {
                SymbolType::Integer(val.fragment.to_string())
            }}|
        do_parse!(
            tag!("\"") >>
            phrase: take_while!(string_character) >>
            tag!("\"") >>
            (phrase)
        ) => {|p: Span| SymbolType::String(p.to_string())}
    )
);

named!(value_provider(Span) -> SymbolType,
    alt!(
        do_parse!(
            l: literal >>
            peek!(alt!(tag!("'n'") | tag!(",") | take_while1!(is_space) | take_while1!(is_newline) | eof!())) >>
            (l)
        ) => {|s| s} |
        pronoun => {|s| s} |
        variable => {|s| SymbolType::Variable(s)}
    )
);

named!(function_call_expr(Span) -> SymbolType,
    alt_complete!(
        do_parse!(
            target: variable >>
            take_while1!(is_space) >>
            tag_no_case!("taking") >>
            take_while1!(is_space) >>
            first: value_provider >>
            rest: many0!(do_parse!(
                alt!(
                    tag!("'n'") => {|_|()} |
                    tuple!(
                        alt!(
                            tuple!(
                                tag!(","),
                                opt!(tuple!(take_while1!(is_space), tag!("and")))
                            ) => {|_|()} |
                            tuple!(take_while1!(is_space), tag!("&")) => {|_|()}
                        ),
                        take_while1!(is_space)
                    ) => {|_|()}
                ) >>
                value: value_provider >>
                (value)
            )) >>
            (target, first, rest)
        ) => {|(name, first, mut rest): (String, SymbolType, Vec<SymbolType>)| {
            rest.insert(0, first);
            SymbolType::Taking{target: name, args: rest}
        }} |
        value_provider => {|s| s}
    )
);

named!(function_signature(Span) -> SymbolType,
    alt_complete!(
        do_parse!(
            target: variable >>
            take_while1!(is_space) >>
            tag_no_case!("takes") >>
            take_while1!(is_space) >>
            first: variable >>
            rest: many0!(do_parse!(
                alt!(
                    tag!("'n'") => {|_|()} |
                    tuple!(
                        alt!(
                            tuple!(
                                tag!(","),
                                opt!(tuple!(take_while1!(is_space), tag!("and")))
                            ) => {|_|()} |
                            tuple!(take_while1!(is_space), tag!("and")) => {|_|()} |
                            tuple!(take_while1!(is_space), tag!("&")) => {|_|()}
                        ),
                        take_while1!(is_space)
                    ) => {|_|()}
                ) >>
                value: variable >>
                (value)
            )) >>
            (target, first, rest)
        ) => {|(name, first, mut rest): (String, String, Vec<String>)| {
            rest.insert(0, first);
            SymbolType::Takes{name, args: rest}
        }}
    )
);

named!(not_expr(Span) -> SymbolType,
    alt_complete!(
        do_parse!(
            tag_no_case!("not") >>
            take_while1!(is_space) >>
            func: not_expr >>
            (func)
        ) => {|f| SymbolType::Not(Box::new(f))} |
        function_call_expr => {|s| s} |
        value_provider => {|s| s}
    )
);

#[cfg_attr(feature = "cargo-clippy", allow(cyclomatic_complexity))] // FIXME: break this up a bit
fn multiply_expr(input: Span) -> nom::IResult<Span, Vec<SymbolType>> {
    let (rest, (mut res, mut times)) = do_parse!(
        input,
        ne: not_expr
            >> times:
                many0!(do_parse!(
                    take_while1!(is_space)
                        >> op: alt_complete!(
                            alt_complete!(
                                tag_no_case!("times") | tag_no_case!("of")
                            ) => {|_| SymbolType::Times} |
                            tag_no_case!("over") => {|_| SymbolType::Divide}
                        )
                        >> take_while1!(is_space)
                        >> other_ne: not_expr
                        >> (vec![op, other_ne])
                ))
            >> (vec![ne], times)
    )?;
    for item in &mut times {
        res.append(item);
    }
    return Ok((rest, res));
}

#[cfg_attr(feature = "cargo-clippy", allow(cyclomatic_complexity))] // FIXME: break this up a bit
fn add_expr(input: Span) -> nom::IResult<Span, Vec<SymbolType>> {
    let (rest, (mut res, mut adds)) = do_parse!(
        input,
        me: multiply_expr
            >> adds: many0!(do_parse!(
                take_while1!(is_space)
                    >> op: alt_complete!(
                    alt_complete!(
                        tag_no_case!("without") | tag_no_case!("minus")
                    ) => {|_| SymbolType::Subtract} |
                    alt_complete!(
                        tag_no_case!("with") | tag_no_case!("plus")
                    ) => {|_| SymbolType::Add}
                ) >> take_while1!(is_space)
                    >> other_me: multiply_expr
                    >> (op, other_me)
            ))
            >> (me, adds)
    )?;
    for (op, item) in &mut adds {
        res.push(op.clone());
        res.append(item);
    }
    return Ok((rest, res));
}

#[cfg_attr(feature = "cargo-clippy", allow(cyclomatic_complexity))] // FIXME: break this up a bit
fn inequality_expr(input: Span) -> nom::IResult<Span, Vec<SymbolType>> {
    let (rest, (mut res, mut ineqs)) = do_parse!(
        input,
        ae: add_expr
            >> ineqs:
                many0!(do_parse!(
                    take_while1!(is_space)
                        >> tag!("is")
                        >> take_while1!(is_space)
                        >> comp: compare
                        >> take_while1!(is_space)
                        >> other_add: add_expr
                        >> (comp, other_add)
                ))
            >> (ae, ineqs)
    )?;
    for (comp, item) in &mut ineqs {
        res.push(comp.clone());
        res.append(item);
    }
    return Ok((rest, res));
}

#[cfg_attr(feature = "cargo-clippy", allow(cyclomatic_complexity))] // FIXME: break this up a bit
fn equality_expr(input: Span) -> nom::IResult<Span, Vec<SymbolType>> {
    let (rest, (mut res, mut eqs)) = do_parse!(
        input,
        ie: inequality_expr
            >> eqs: many0!(do_parse!(
                take_while1!(is_space)
                    >> kind: alt_complete!(
                    alt_complete!(
                        tag!("is") | tag!("was") | tag!("are") | tag!("were")
                    ) => {|_| SymbolType::Is} |
                    alt_complete!(
                        tag_no_case!("ain't") | tag_no_case!("aint")
                    ) => {|_| SymbolType::Aint}
                ) >> take_while1!(is_space)
                    >> other_ie: inequality_expr
                    >> (kind, other_ie)
            ))
            >> (ie, eqs)
    )?;
    for (kind, item) in &mut eqs {
        res.push(kind.clone());
        res.append(item);
    }
    return Ok((rest, res));
}

fn boolean_expr(input: Span) -> nom::IResult<Span, Vec<SymbolType>> {
    let (rest, (mut res, mut bqs)) = do_parse!(
        input,
        ee: equality_expr
            >> bqs: many0!(do_parse!(
                take_while1!(is_space)
                    >> kind: alt_complete!(
                    tag_no_case!("and") => {|_| SymbolType::And} |
                    tag_no_case!("or") => {|_| SymbolType::Or} |
                    tag_no_case!("nor") => {|_| SymbolType::Nor}
                ) >> take_while1!(is_space)
                    >> other_ee: equality_expr
                    >> (kind, other_ee)
            ))
            >> (ee, bqs)
    )?;
    for (kind, item) in &mut bqs {
        res.push(kind.clone());
        res.append(item);
    }
    return Ok((rest, res));
}

named!(statement(Span) -> SymbolType,
    do_parse!(
        val: alt_complete!(
            function_signature => {|s| s} |
            tag_no_case!("if") => {|_| SymbolType::If} |
            do_parse!(
                tag_no_case!("build") >>
                take_while1!(is_space) >>
                target: variable >>
                take_while1!(is_space) >>
                tag_no_case!("up") >>
                rest: many0!(do_parse!(
                    opt!(tag!(",")) >>
                    take_while1!(is_space) >>
                    tag_no_case!("up") >>
                    (())
                )) >>
                (target, rest.len())
            ) => {|(target, rest)| SymbolType::Build { target, count: rest + 1}} |
            do_parse!(
                tag_no_case!("knock") >>
                take_while1!(is_space) >>
                target: variable >>
                take_while1!(is_space) >>
                tag_no_case!("down") >>
                rest: many0!(do_parse!(
                    opt!(tag!(",")) >>
                    take_while1!(is_space) >>
                    tag_no_case!("down") >>
                    (())
                )) >>
                (target, rest.len())
            ) => {|(target, rest)| SymbolType::Knock { target, count: rest + 1}} |
            alt_complete!(
                tag_no_case!("say") | tag_no_case!("shout") | tag_no_case!("whisper") | tag_no_case!("scream")
            ) => {|_| SymbolType::Say} |
            tag_no_case!("while") => {|_| SymbolType::While} |
            tag_no_case!("until") => {|_| SymbolType::Until} |
            alt_complete!(
                tag_no_case!("end") | tag_no_case!("around we go")
            ) => {|_| SymbolType::Next} |
            alt_complete!(
                tag_no_case!("take it to the top") | tag_no_case!("continue")
            ) => {|_| SymbolType::Continue} |
            tag_no_case!("give back") => {|_| SymbolType::Return} |
            tag_no_case!("into") => {|_| SymbolType::Where} |
            tag_no_case!("put") => {|_| SymbolType::Put} |
            tag_no_case!("else") => {|_| SymbolType::Else} |
            tag_no_case!("listen to") => {|_| SymbolType::Listen} |
            tag_no_case!("listen") => {|_| SymbolType::Listen} |
            tag_no_case!("break it down") => {|_| SymbolType::Break} |
            tag_no_case!("break") => {|_| SymbolType::Break}
        ) >>
        peek!(alt!(take_while1!(is_space) | take_while1!(is_newline) | eof!())) >>
        (val)
    )
);

named!(word(Span) -> Vec<SymbolType>,
    alt_complete!(
        statement => {|s: SymbolType| vec![s]} |
        do_parse!(
            pv: variable >>
            tag!("'s") >>
            take_while1!(is_space) >>
            comp: compare >>
            (pv, comp)
        ) => {|(pv, comp):(String, SymbolType)| vec![SymbolType::Variable(pv), comp]} |
        do_parse!(
            tag!("(") >>
            take_until!(")") >>
            tag!(")") >>
            ()
        ) => {|_| vec![SymbolType::Comment] } |
        boolean_expr => {|e| e}
    )
);

named!(poetic_number_literal_core<Span, (u32, String, Vec<Span>)>,
    do_parse!(
        pv: variable >>
        alt!(
            tag!("'s") => {|_| (())} |
            do_parse!(
                take_while1!(is_space) >>
                alt!(tag!("is") | tag!("are") | tag!("was") | tag!("were")) >>
                (())
            )
        ) >>
        position: position!() >>
        peek!(not!(tuple!(
            take_while1!(is_space),
            literal_word,
            alt!(take_while1!(is_space) | take_while1!(is_newline) | eof!()))
        )) >> // number literals cannot start with a literal word
        peek!(tuple!(take_while1!(is_space), take_while1!(word_character))) >> // make sure it starts with a word character
        words: many1!(
            do_parse!(
                take_while1!(is_literal_spacing_character) >>
                word: take_while1!(word_character) >>
                (word)
            )
        ) >>
        (position.line, pv, words)
    )
);

fn poetic_number_literal(input: Span) -> nom::IResult<Span, (u32, Vec<SymbolType>)> {
    let (rest, (line, target, words)) = poetic_number_literal_core(input)?;
    let literal = SymbolType::Words(words.iter().map(|s| s.to_string()).collect());
    return Ok((
        rest,
        (
            line,
            vec![SymbolType::Variable(target), SymbolType::Is, literal],
        ),
    ));
}

named!(poetic_string_literal_core<Span, (u32, String, Span)>,
    do_parse!(
        pv: variable >>
        take_while1!(is_space) >>
        tag!("says") >>
        take_while1!(is_space) >>
        position: position!() >>
        words: take_while1!(is_not_newline) >>
        (position.line, pv, words)
    )
);

fn poetic_string_literal(input: Span) -> nom::IResult<Span, (u32, Vec<SymbolType>)> {
    let (rest, (line, target, words)) = poetic_string_literal_core(input)?;
    let literal = SymbolType::String(words.to_string());
    return Ok((
        rest,
        (
            line,
            vec![SymbolType::Variable(target), SymbolType::Is, literal],
        ),
    ));
}

named!(pub line_core<Span, (u32, Vec<Vec<SymbolType>>)>, alt_complete!(
    do_parse!(
        position: position!() >>
        variable: variable >>
        take_while1!(is_space) >>
        tag!("is") >>
        take_while1!(is_space) >>
        kind: alt!(
            alt!(tag_no_case!("true") | tag_no_case!("yes")) => {|_| SymbolType::True } |
            alt!(tag_no_case!("false") | tag_no_case!("lies")) => {|_| SymbolType::False } |
            tag_no_case!("mysterious") => {|_| SymbolType::Mysterious }
        ) >>
        (position, variable, kind)
    ) => {|(p,v,k): (Span, String, SymbolType)|
        (p.line, vec![vec![
            SymbolType::Variable(v),
            SymbolType::Is,
            k]])} |
    poetic_number_literal => {|(line, s)| (line, vec![s]) } |
    poetic_string_literal => {|(line, s)| (line, vec![s]) } |
    do_parse!(
        position: position!() >>
        first_words: word >>
        other_words: many0!(
            alt_complete!(
                tag!(",") => {|_| vec![SymbolType::Comma]}|
                do_parse!(
                    take_while1!(is_space) >>
                    words: word >>
                    (words)
                ) => {|t| t}
        )) >>
        (position, first_words, other_words)
    ) => {|(pos, first, mut other):(Span, Vec<SymbolType>, Vec<Vec<SymbolType>>)| {
        other.insert(0, first);
        (pos.line, other)
         }}
));

fn line(input: Span) -> nom::IResult<Span, Vec<Token>> {
    let (rest, (line, words)) = line_core(input)?;
    let mut res = vec![];
    for wordset in words {
        for word in wordset {
            res.push(Token { line, symbol: word });
        }
    }
    return Ok((rest, res));
}

named!(blank_line<Span, Vec<Token>>,
    do_parse!(
        pos: position!() >>
        take_while!(is_space) >>
        alt!(tag!("\n") | tag!("\r")) >>
        take_while!(is_space) >>
        (vec![Token{line: pos.line, symbol: SymbolType::Newline}])
    )
);

named!(lines_core<Span, (Vec<Token>, Vec<Vec<Token>>)>,
    do_parse!(
        many0!(blank_line) >>
        first_line: line >>
        other_lines: many0!(
            alt_complete!(
                do_parse!(
                    take_while!(is_space) >>
                    alt!(tag!("\n") | tag!("\r")) >>
                    take_while!(is_space) >>
                    a_line: line >>
                    (a_line)
                ) => {|l| l } |
                blank_line => {|b| b }
            )
        ) >>
        (first_line, other_lines)
    )
);

fn lines(input: &str) -> nom::IResult<Span, Vec<Vec<Token>>> {
    let cs = CompleteStr(&input);
    let complete: Span = Span::new(cs);
    return match lines_core(complete) {
        Ok((rest, (first, mut others))) => {
            others.insert(0, first);
            Ok((rest, others))
        }
        Err(err) => Err(err),
    };
}

fn compact_words(line: Vec<Token>) -> Vec<Token> {
    let mut symbols: Vec<Token> = Vec::new();
    let mut words = Vec::new();
    let pos = line[0].line;
    for word in line {
        match word.symbol {
            SymbolType::Words(other) => {
                words.extend_from_slice(&other);
            }
            SymbolType::Comment => {
                // strip these
            }
            _ => {
                if !words.is_empty() {
                    symbols.push(Token {
                        line: word.line,
                        symbol: SymbolType::Words(words),
                    });
                    words = Vec::new();
                }
                symbols.push(word);
            }
        }
    }
    if !words.is_empty() {
        symbols.push(Token {
            line: pos,
            symbol: SymbolType::Words(words),
        });
    }
    return symbols;
}

fn parse_words(words: &[String]) -> usize {
    let mut number = 0;
    for word in words {
        let len = word.chars().filter(|c| char::is_alphabetic(*c)).count();
        if len > 0 {
            number *= 10;
            number += len % 10;
        }
    }
    return number;
}

fn evaluate(value: &SymbolType, line: u32) -> Result<Expression> {
    match value {
        SymbolType::Words(words) => {
            if words.len() == 1 {
                let as_float = words[0].parse::<f64>();
                if let Ok(float) = as_float {
                    return Ok(Expression::Floating(float));
                }
            }
            let fullstop = words.iter().position(|s| s.contains('.'));
            let number = if let Some(fullstop_pos) = fullstop {
                let mut editable_words = words.clone();
                let after = editable_words.split_off(fullstop_pos + 1);
                let first = parse_words(&editable_words) as f64;
                let second = parse_words(&after) as f64;
                let divisor = 10f64.powf(second.log10().ceil());
                debug!("first: {}, second: {}, divisor: {}", first, second, divisor);
                if second == 0.0 && divisor == 0.0 {
                    first
                } else {
                    first + (second / divisor)
                }
            } else {
                parse_words(&words) as f64
            };
            return Ok(Expression::Floating(number));
        }
        SymbolType::String(phrase) => {
            return Ok(Expression::String(phrase.to_string()));
        }
        _ => {
            bail!(ErrorKind::Unimplemented(
                format!("Evaluate: '{:?}'", value),
                line
            ));
        }
    }
}

fn next_operator<'a>(
    items: &[&'a SymbolType],
    mut index: usize,
) -> Option<(&'a SymbolType, usize)> {
    loop {
        let item_poss = items.get(index);
        let item = item_poss?;
        match *item {
            SymbolType::Is
            | SymbolType::Aint
            | SymbolType::GreaterThanOrEqual
            | SymbolType::GreaterThan
            | SymbolType::LessThan
            | SymbolType::LessThanOrEqual
            | SymbolType::Add
            | SymbolType::Subtract
            | SymbolType::Times
            | SymbolType::Divide
            | SymbolType::Or
            | SymbolType::Nor
            | SymbolType::And => {
                return Some((item, index));
            }
            _ => {}
        }
        index += 1;
    }
}

fn single_symbol_to_expression(sym: &SymbolType, line: u32) -> Result<Expression> {
    return match *sym {
        SymbolType::Taking {
            ref target,
            ref args,
        } => Ok(Expression::Call(
            target.clone(),
            args.iter()
                .map(|arg| single_symbol_to_expression(arg, line).unwrap())
                .collect::<Vec<Expression>>(),
        )),
        SymbolType::Words(_) => evaluate(sym, line),
        SymbolType::Variable(ref name) => Ok(Expression::Variable(name.clone())),
        SymbolType::String(ref phrase) => Ok(Expression::String(phrase.clone())),
        SymbolType::Integer(ref val) => {
            return match val.parse::<f64>() {
                Ok(i) => Ok(Expression::Floating(i)),
                Err(_) => bail!(ErrorKind::ParseNumberError(val.to_string(), line)),
            };
        }
        SymbolType::Floating(ref val) => {
            return match val.parse::<f64>() {
                Ok(i) => Ok(Expression::Floating(i)),
                Err(_) => bail!(ErrorKind::ParseNumberError(val.to_string(), line)),
            };
        }
        SymbolType::True => Ok(Expression::True),
        SymbolType::False => Ok(Expression::False),
        SymbolType::Null => Ok(Expression::Null),
        SymbolType::Mysterious => Ok(Expression::Mysterious),
        SymbolType::Pronoun => Ok(Expression::Pronoun),
        SymbolType::Not(ref arg) => Ok(Expression::Not(Box::new(single_symbol_to_expression(
            &arg, line,
        )?))),
        _ => {
            bail!(ErrorKind::Unimplemented(
                format!("Single symbol to expression: {:?}", sym),
                line
            ));
        }
    };
}

fn parse_expression(items: &[&SymbolType], line: u32) -> Result<Expression> {
    // based off of https://en.wikipedia.org/wiki/Operator-precedence_parser#Pseudo-code
    let describe = format!("{:?}", items);
    if items.is_empty() {
        bail!(ErrorKind::UnbalancedExpression(describe, line));
    }
    debug!("Begin parse: {}", describe);
    let lhs = single_symbol_to_expression(items[0], line)?;
    let res = parse_expression_1(&items, 0, lhs, &LOWEST_PRECDENCE, line)?;
    if res.1 != items.len() - 1 {
        bail!(ErrorKind::UnbalancedExpression(describe, line));
    }
    return Ok(res.0);
}

fn parse_expression_1(
    items: &[&SymbolType],
    mut index: usize,
    mut lhs: Expression,
    precedence: &SymbolType,
    line: u32,
) -> Result<(Expression, usize)> {
    debug!(
        "index: {}, lhs: {:?} precedence: {:?}",
        index, lhs, precedence
    );
    let mut lookahead = next_operator(items, index);
    while lookahead.is_some() && lookahead.unwrap().0 >= precedence {
        debug!("lookahead: {:?}", lookahead.unwrap());
        let op = lookahead.unwrap().0;
        index = if lookahead.is_some() {
            lookahead.unwrap().1 + 1
        } else {
            index
        };
        if index >= items.len() {
            bail!(ErrorKind::UnbalancedExpression(
                format!("{:?}", items),
                line
            ));
        }
        let mut rhs = single_symbol_to_expression(items[index], line)?;
        lookahead = next_operator(items, index);
        while lookahead.is_some() && lookahead.unwrap().0 > op {
            let l = lookahead.unwrap().1;
            if l >= items.len() {
                bail!(ErrorKind::UnbalancedExpression(
                    format!("{:?}", items),
                    line
                ));
            }
            let res = parse_expression_1(items, index, rhs, &items[l], line)?;
            rhs = res.0;
            index = res.1;
            lookahead = next_operator(items, index);
        }
        lhs = match *op {
            SymbolType::Is => Expression::Is(Box::new(lhs.clone()), Box::new(rhs)),
            SymbolType::Aint => Expression::Aint(Box::new(lhs.clone()), Box::new(rhs)),
            SymbolType::GreaterThanOrEqual => {
                Expression::GreaterThanOrEqual(Box::new(lhs.clone()), Box::new(rhs))
            }
            SymbolType::GreaterThan => {
                Expression::GreaterThan(Box::new(lhs.clone()), Box::new(rhs))
            }
            SymbolType::LessThan => Expression::LessThan(Box::new(lhs.clone()), Box::new(rhs)),
            SymbolType::LessThanOrEqual => {
                Expression::LessThanOrEqual(Box::new(lhs.clone()), Box::new(rhs))
            }
            SymbolType::Add => Expression::Add(Box::new(lhs.clone()), Box::new(rhs)),
            SymbolType::Subtract => Expression::Subtract(Box::new(lhs.clone()), Box::new(rhs)),
            SymbolType::Times => Expression::Times(Box::new(lhs.clone()), Box::new(rhs)),
            SymbolType::Divide => Expression::Divide(Box::new(lhs.clone()), Box::new(rhs)),
            SymbolType::And => Expression::And(Box::new(lhs.clone()), Box::new(rhs)),
            SymbolType::Or => Expression::Or(Box::new(lhs.clone()), Box::new(rhs)),
            SymbolType::Nor => Expression::Nor(Box::new(lhs.clone()), Box::new(rhs)),
            _ => {
                bail!(ErrorKind::Unimplemented(
                    format!("No operation for {:?}", op),
                    line
                ));
            }
        }
    }
    return Ok((lhs.clone(), index));
}

fn build_next(commands: &mut Vec<CommandLine>, loop_starts: &mut Vec<usize>) -> Command {
    let loop_start = loop_starts.pop().expect("loop_starts");
    let loop_len = commands.len();
    match commands.index_mut(loop_start).cmd {
        Command::Until {
            ref mut loop_end, ..
        } => {
            loop_end.get_or_insert(loop_len);
        }
        Command::While {
            ref mut loop_end, ..
        } => {
            loop_end.get_or_insert(loop_len);
        }
        _ => {
            panic!("loop to non-loop command");
        }
    }
    return Command::Next { loop_start };
}

#[cfg_attr(feature = "cargo-clippy", allow(cyclomatic_complexity))] // FIXME: break this up a bit
pub fn parse(input: &str) -> Result<Program> {
    let raw_lines = lines(&input)?;
    if !raw_lines.0.fragment.is_empty() && raw_lines.0.fragment.chars().any(|c| !c.is_whitespace())
    {
        // ignore empty and all-whitespace blocks
        let pos = raw_lines.0;
        bail!(ErrorKind::UnparsedText(pos.fragment.to_string(), pos.line));
    }
    debug!("{:?}", raw_lines);
    let mut functions: HashMap<String, Function> = HashMap::new();
    let mut commands: Vec<CommandLine> = Vec::new();
    let mut loop_starts: Vec<usize> = Vec::new();
    let mut func_starts: Vec<usize> = Vec::new();
    let mut if_starts: Vec<usize> = Vec::new();
    let mut last_line = 0;
    let mut pronoun: Option<String> = None;
    for raw_symbols in raw_lines.1 {
        debug!("raw_symbols: {:?}", raw_symbols);
        let mut symbols = compact_words(raw_symbols);
        if !symbols.is_empty() {
            if symbols[0].symbol == SymbolType::And {
                symbols.remove(0);
            }
            if symbols[symbols.len() - 1].symbol == SymbolType::Comma {
                symbols.pop();
            }
        }
        debug!("symbols: {:?}", symbols);
        let current_line = if symbols.is_empty() {
            last_line
        } else {
            symbols.first().unwrap().line
        };
        last_line = current_line;
        let symbols: Vec<SymbolType> = symbols.into_iter().map(|t| t.symbol).collect();
        match symbols.as_slice() {
            [SymbolType::Next] => {
                let command = build_next(&mut commands, &mut loop_starts);
                commands.push(CommandLine {
                    cmd: command,
                    line: current_line,
                });
            }
            [SymbolType::Continue] => {
                let loop_start = loop_starts.last().expect("loop_starts");
                commands.push(CommandLine {
                    cmd: Command::Continue {
                        loop_start: *loop_start,
                    },
                    line: current_line,
                });
            }
            [SymbolType::Newline] | [] => {
                // Comment on it's own is newline-equivalent
                if !if_starts.is_empty() {
                    let if_start = if_starts.pop().expect("if_starts");
                    let if_len = commands.len();
                    match commands.index_mut(if_start) {
                        CommandLine {
                            cmd: Command::If { ref mut if_end, .. },
                            ..
                        } => {
                            if_end.get_or_insert(if_len);
                        }
                        _ => {
                            panic!("return to non-if command");
                        }
                    }
                    commands.push(CommandLine {
                        cmd: Command::EndIf,
                        line: if !symbols.is_empty() && symbols[0] == SymbolType::Newline {
                            current_line + 1 // Newline line is the one before this
                        } else {
                            current_line
                        },
                    });
                } else if !loop_starts.is_empty() {
                    let command = build_next(&mut commands, &mut loop_starts);
                    commands.push(CommandLine {
                        cmd: command,
                        line: if !symbols.is_empty() && symbols[0] == SymbolType::Newline {
                            current_line + 1 // Newline line is the one before this
                        } else {
                            current_line
                        },
                    });
                } else if !func_starts.is_empty() {
                    let func_start = func_starts.pop().expect("func_starts");
                    let func_len = commands.len();
                    match commands.index_mut(func_start) {
                        CommandLine {
                            cmd:
                                Command::FunctionDeclaration {
                                    ref mut func_end, ..
                                },
                            ..
                        } => {
                            func_end.get_or_insert(func_len);
                        }
                        _ => {
                            panic!("return to non-func command");
                        }
                    }
                    commands.push(CommandLine {
                        cmd: Command::EndFunction,
                        line: if !symbols.is_empty() && symbols[0] == SymbolType::Newline {
                            current_line + 1 // Newline line is the one before this
                        } else {
                            current_line
                        },
                    });
                } else {
                    debug!("Double newline that doesn't end anything");
                }
            }
            [SymbolType::Listen] => {
                commands.push(CommandLine {
                    cmd: Command::Listen { target: None },
                    line: current_line,
                });
            }
            [SymbolType::Listen, SymbolType::Variable(target)] => {
                commands.push(CommandLine {
                    cmd: Command::Listen {
                        target: Some(target.to_string()),
                    },
                    line: current_line,
                });
            }
            [SymbolType::Else] => {
                if if_starts.is_empty() {
                    bail!(ErrorKind::ElseWithNoIf(current_line));
                }
                let if_start = if_starts.last().expect("if_starts");
                let if_len = commands.len();
                match commands.index_mut(*if_start) {
                    CommandLine {
                        cmd:
                            Command::If {
                                ref mut else_loc, ..
                            },
                        ..
                    } => {
                        else_loc.get_or_insert(if_len);
                    }
                    _ => {
                        panic!("return to non-if command");
                    }
                }
                commands.push(CommandLine {
                    cmd: Command::Else {
                        if_start: *if_start,
                    },
                    line: if !symbols.is_empty() && symbols[0] == SymbolType::Newline {
                        current_line + 1 // Newline line is the one before this
                    } else {
                        current_line
                    },
                });
            }
            [SymbolType::Break] => {
                let loop_start = loop_starts.last().expect("loop_starts");
                commands.push(CommandLine {
                    cmd: Command::Break {
                        loop_start: *loop_start,
                    },
                    line: current_line,
                });
            }
            [SymbolType::Taking {
                ref target,
                ref args,
            }] => {
                commands.push(CommandLine {
                    cmd: Command::Call {
                        name: target.to_string(),
                        args: args
                            .iter()
                            .map(|arg| single_symbol_to_expression(&arg, current_line).unwrap())
                            .collect::<Vec<Expression>>(),
                    },
                    line: current_line,
                });
            }
            [SymbolType::Takes { ref name, ref args }] => {
                func_starts.push(commands.len());
                functions.insert(
                    name.to_string(),
                    Function {
                        location: commands.len(),
                        args: args.clone(),
                    },
                );
                commands.push(CommandLine {
                    cmd: Command::FunctionDeclaration {
                        name: name.to_string(),
                        args: args.clone(),
                        func_end: None,
                    },
                    line: current_line,
                });
            }
            [SymbolType::Build {
                ref target,
                ref count,
            }] => {
                commands.push(CommandLine {
                    cmd: Command::Increment {
                        target: target.to_string(),
                        count: *count as f64,
                    },
                    line: current_line,
                });
            }
            [SymbolType::Knock {
                ref target,
                ref count,
            }] => {
                commands.push(CommandLine {
                    cmd: Command::Decrement {
                        target: target.to_string(),
                        count: *count as f64,
                    },
                    line: current_line,
                });
            }
            _ => {
                // Better done with slice patterns once they stabilise
                // (see https://github.com/rust-lang/rust/issues/23121)
                if symbols[0] == SymbolType::Say && symbols.len() > 1 {
                    let expression_seq: Vec<&SymbolType> = symbols.iter().skip(1).collect();
                    let expression = parse_expression(&expression_seq, current_line)?;
                    commands.push(CommandLine {
                        cmd: Command::Say { value: expression },
                        line: current_line,
                    });
                } else if symbols.len() > 1 && symbols[1] == SymbolType::Is {
                    let target = match symbols[0] {
                        SymbolType::Variable(ref target) => target.to_string(),
                        SymbolType::Pronoun => {
                            if pronoun.is_none() {
                                bail!(ErrorKind::UndefinedPronoun(current_line));
                            }
                            pronoun.clone().unwrap()
                        }
                        _ => {
                            bail!(ErrorKind::BadIs(symbols.to_vec(), current_line));
                        }
                    };
                    let expression_seq: Vec<&SymbolType> = symbols.iter().skip(2).collect();
                    let expression = parse_expression(&expression_seq, current_line)?;
                    pronoun = Some(target.clone());
                    commands.push(CommandLine {
                        cmd: Command::Assignment {
                            target,
                            value: expression,
                        },
                        line: current_line,
                    });
                } else if symbols[0] == SymbolType::Until && symbols.len() > 1 {
                    loop_starts.push(commands.len());
                    let expression_seq: Vec<&SymbolType> = symbols.iter().skip(1).collect();
                    let expression = parse_expression(&expression_seq, current_line)?;
                    commands.push(CommandLine {
                        cmd: Command::Until {
                            expression,
                            loop_end: None,
                        },
                        line: current_line,
                    });
                } else if symbols[0] == SymbolType::While && symbols.len() > 1 {
                    loop_starts.push(commands.len());
                    let expression_seq: Vec<&SymbolType> = symbols.iter().skip(1).collect();
                    let expression = parse_expression(&expression_seq, current_line)?;
                    commands.push(CommandLine {
                        cmd: Command::While {
                            expression,
                            loop_end: None,
                        },
                        line: current_line,
                    });
                } else if symbols[0] == SymbolType::If && symbols.len() > 1 {
                    if_starts.push(commands.len());
                    let expression_seq: Vec<&SymbolType> = symbols.iter().skip(1).collect();
                    let expression = parse_expression(&expression_seq, current_line)?;
                    commands.push(CommandLine {
                        cmd: Command::If {
                            expression,
                            if_end: None,
                            else_loc: None,
                        },
                        line: current_line,
                    });
                } else if symbols.len() > 3
                    && symbols[0] == SymbolType::Put
                    && symbols[symbols.len() - 2] == SymbolType::Where
                {
                    let target = match symbols[symbols.len() - 1] {
                        SymbolType::Variable(ref target) => target.to_string(),
                        SymbolType::Pronoun => {
                            if pronoun.is_none() {
                                bail!(ErrorKind::UndefinedPronoun(current_line));
                            }
                            pronoun.clone().unwrap()
                        }
                        _ => {
                            bail!(ErrorKind::BadPut(symbols.to_vec(), current_line));
                        }
                    };
                    let expression_seq: Vec<&SymbolType> =
                        symbols.iter().skip(1).take(symbols.len() - 3).collect();
                    let expression = parse_expression(&expression_seq, current_line)?;
                    pronoun = Some(target.clone());
                    commands.push(CommandLine {
                        cmd: Command::Assignment {
                            target,
                            value: expression,
                        },
                        line: current_line,
                    });
                } else if symbols[0] == SymbolType::Return && symbols.len() > 1 {
                    let expression_seq: Vec<&SymbolType> = symbols.iter().skip(1).collect();
                    let expression = parse_expression(&expression_seq, current_line)?;
                    commands.push(CommandLine {
                        cmd: Command::Return {
                            return_value: expression,
                        },
                        line: current_line,
                    });
                } else {
                    bail!(ErrorKind::BadCommandSequence(
                        symbols.to_vec(),
                        current_line
                    ));
                }
            }
        }
    }
    return Ok(Program {
        commands,
        functions,
    });
}

#[cfg(any(target_arch = "wasm32", test))]
fn print_command(command: &Command) -> String {
    format!("{:?}", command)
}

#[cfg(any(target_arch = "wasm32", test))]
pub fn print_program(program: &Program) -> String {
    let mut res = String::new();
    let mut indent = 0;
    let mut last_line = 0;
    for command in &program.commands {
        match command.cmd {
            Command::EndFunction | Command::Else { .. } | Command::EndIf | Command::Next { .. } => {
                indent -= 1;
            }
            _ => {}
        }
        while last_line < command.line - 1 {
            last_line += 1;
            res += &format!("{}:\n", last_line);
        }
        last_line = command.line;
        res += &format!("{}: ", command.line);
        for _ in 0..indent {
            res += "  ";
        }
        res += &(print_command(&command.cmd) + "\n");
        match command.cmd {
            Command::FunctionDeclaration { .. }
            | Command::If { .. }
            | Command::While { .. }
            | Command::Else { .. }
            | Command::Until { .. } => {
                indent += 1;
            }
            _ => {}
        }
    }
    return res;
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_env_logger;

    #[test]
    fn multi_word_quote_parse() {
        let (span, tokens) = line(Span::new(CompleteStr("say \"shout let it all out\""))).unwrap();
        assert_eq!(CompleteStr(""), span.fragment);
        assert_eq!(
            vec![
                SymbolType::Say,
                SymbolType::String("shout let it all out".to_string())
            ],
            tokens.into_iter().map(|t| t.symbol).collect::<Vec<_>>()
        );
    }

    #[test]
    fn check_evaluate() {
        pretty_env_logger::try_init().unwrap_or(());
        assert_eq!(
            evaluate(
                &SymbolType::Words(vec![
                    "a".to_string(),
                    "lovestruck".to_string(),
                    "ladykiller".to_string()
                ]),
                0
            ).unwrap(),
            Expression::Floating(100f64)
        );
    }

    #[test]
    fn check_full_expression_parse() {
        pretty_env_logger::try_init().unwrap_or(());
        let expression = Expression::And(
            Box::new(Expression::Is(
                Box::new(Expression::Call(
                    "Midnight".to_string(),
                    vec![
                        Expression::Variable("my world".to_string()),
                        Expression::Variable("Fire".to_string()),
                    ],
                )),
                Box::new(Expression::Null),
            )),
            Box::new(Expression::Is(
                Box::new(Expression::Call(
                    "Midnight".to_string(),
                    vec![
                        Expression::Variable("my world".to_string()),
                        Expression::Variable("Hate".to_string()),
                    ],
                )),
                Box::new(Expression::Null),
            )),
        );
        let commands = vec![CommandLine {
            cmd: Command::If {
                expression: expression,
                if_end: None,
                else_loc: None,
            },
            line: 1,
        }];
        let functions = HashMap::new();
        assert_eq!(
            parse("If Midnight taking my world, Fire is nothing and Midnight taking my world, Hate is nothing")
                .unwrap(),
            Program { commands, functions }
        );
    }

    fn lines_tokens_check(input: &str, tokens: Vec<SymbolType>) {
        pretty_env_logger::try_init().unwrap_or(());
        let mut raw_lines = lines(input).unwrap();
        assert_eq!(raw_lines.0.fragment, CompleteStr(""), "{:?}", raw_lines);
        assert_eq!(raw_lines.1.len(), 1, "{:?}", raw_lines.1);
        assert_eq!(
            raw_lines
                .1
                .remove(0)
                .into_iter()
                .map(|t| t.symbol)
                .collect::<Vec<_>>(),
            tokens
        );
    }

    #[test]
    fn check_expression_parse() {
        lines_tokens_check(
            "If Midnight taking my world, Fire is nothing and Midnight taking my world, Hate is nothing",
            vec![
                SymbolType::If,
                SymbolType::Taking{target:"Midnight".to_string(), args: vec![SymbolType::Variable("my world".to_string()), SymbolType::Variable("Fire".to_string())]},
                SymbolType::Is,
                SymbolType::Null,
                SymbolType::And,
                SymbolType::Taking{target:"Midnight".to_string(), args: vec![SymbolType::Variable("my world".to_string()), SymbolType::Variable("Hate".to_string())]},
                SymbolType::Is,
                SymbolType::Null,
            ],
        );
    }

    #[test]
    fn negative_numbers() {
        lines_tokens_check("-3", vec![SymbolType::Integer("-3".to_string())]);
    }

    #[test]
    fn floating_point_numbers() {
        lines_tokens_check(
            "say .5",
            vec![SymbolType::Say, SymbolType::Floating(".5".to_string())],
        );
    }

    #[test]
    fn comment_parsing() {
        lines_tokens_check("(foo bar baz)", vec![SymbolType::Comment]);
    }

    #[test]
    fn apostrophe_parsing() {
        let commands = vec![CommandLine {
            cmd: Command::Assignment {
                target: "Bar".to_string(),
                value: Expression::Floating(4f64),
            },
            line: 1,
        }];
        let functions = HashMap::new();
        assert_eq!(
            parse("Bar is foo'd").unwrap(),
            Program {
                commands,
                functions
            }
        );
    }

    #[test]
    fn multi_word_proper_variable() {
        lines_tokens_check(
            "Liftin High takes the spirit and Foo",
            vec![SymbolType::Takes {
                name: "Liftin High".to_string(),
                args: vec!["the spirit".to_string(), "Foo".to_string()],
            }],
        );
    }

    #[test]
    fn not_proper_variable() {
        lines_tokens_check(
            "Until Counter is Limit",
            vec![
                SymbolType::Until,
                SymbolType::Variable("Counter".to_string()),
                SymbolType::Is,
                SymbolType::Variable("Limit".to_string()),
            ],
        );
    }

    #[test]
    fn split_nothing() {
        lines_tokens_check(
            "If a thought is greater than Nothinggggggggg",
            vec![
                SymbolType::If,
                SymbolType::Variable("a thought".to_string()),
                SymbolType::GreaterThan,
                SymbolType::Variable("Nothinggggggggg".to_string()),
            ],
        );
    }

    #[test]
    fn literal_words() {
        lines_tokens_check(
            "My world is nothing without your love",
            vec![
                SymbolType::Variable("My world".to_string()),
                SymbolType::Is,
                SymbolType::Null,
                SymbolType::Subtract,
                SymbolType::Variable("your love".to_string()),
            ],
        );
    }

    #[test]
    fn non_alphabetic_literal() {
        lines_tokens_check(
            "A nightmare is decimated, destroyed; sparkling, sinuously perfected",
            vec![
                SymbolType::Variable("A nightmare".to_string()),
                SymbolType::Is,
                SymbolType::Words(vec![
                    "decimated".to_string(),
                    "destroyed".to_string(),
                    "sparkling".to_string(),
                    "sinuously".to_string(),
                    "perfected".to_string(),
                ]),
            ],
        );
    }

    #[test]
    fn keyword_named_func() {
        lines_tokens_check(
            "TrueFunc takes Ignored",
            vec![SymbolType::Takes {
                name: "TrueFunc".to_string(),
                args: vec!["Ignored".to_string()],
            }],
        );
    }

    #[test]
    fn nor_usage() {
        lines_tokens_check(
            "Say true nor true",
            vec![
                SymbolType::Say,
                SymbolType::True,
                SymbolType::Nor,
                SymbolType::True,
            ],
        );
    }

    #[test]
    fn great_davy() {
        pretty_env_logger::try_init().unwrap_or(());
        let expression = Expression::Aint(
            Box::new(Expression::Variable("Davy".to_string())),
            Box::new(Expression::Variable("Greatness".to_string())),
        );
        let commands = vec![CommandLine {
            cmd: Command::While {
                expression,
                loop_end: None,
            },
            line: 1,
        }];
        let functions = HashMap::new();
        assert_eq!(
            parse("While Davy ain't Greatness").unwrap(),
            Program {
                commands,
                functions
            }
        );
    }

    #[test]
    fn everyone_taking_the_bait() {
        pretty_env_logger::try_init().unwrap_or(());
        let commands = vec![CommandLine {
            cmd: Command::Assignment {
                target: "Foo".to_string(),
                value: Expression::Subtract(
                    Box::new(Expression::Call(
                        "Everyone".to_string(),
                        vec![Expression::Variable("the bait".to_string())],
                    )),
                    Box::new(Expression::Floating(1f64)),
                ),
            },
            line: 1,
        }];
        let functions = HashMap::new();
        assert_eq!(
            parse("Put Everyone taking the bait without 1 into Foo").unwrap(),
            Program {
                commands,
                functions
            }
        );
    }

    #[test]
    fn pretty_print() {
        assert_eq!(
            print_program(&parse("Absolute takes a thought").unwrap()),
            "1: FunctionDeclaration { name: \"Absolute\", args: [\"a thought\"], func_end: None }\n"
        )
    }

    #[test]
    fn bad_fragment() {
        pretty_env_logger::try_init().unwrap_or(());
        let err = parse("2 is 1").err().unwrap().0;
        if let ErrorKind::BadIs(symbols, line) = err {
            assert_eq!(
                symbols,
                vec![
                    SymbolType::Integer("2".to_string()),
                    SymbolType::Is,
                    SymbolType::Integer("1".to_string()),
                ]
            );
            assert_eq!(line, 1);
        } else {
            assert!(false, format!("{:?}", err));
        }
    }

    #[test]
    fn bad_expression() {
        pretty_env_logger::try_init().unwrap_or(());
        let err = parse("if 1 is").err().unwrap().0;
        if let ErrorKind::UnparsedText(content, line) = err {
            assert_eq!(content, " is");
            assert_eq!(line, 1);
        } else {
            assert!(false, format!("{:?}", err));
        }
    }

    #[test]
    fn bad_put() {
        pretty_env_logger::try_init().unwrap_or(());
        let err = parse("put 1 into 3").err().unwrap().0;
        if let ErrorKind::BadPut(expression, line) = err {
            assert_eq!(
                expression,
                vec![
                    SymbolType::Put,
                    SymbolType::Integer("1".to_string()),
                    SymbolType::Where,
                    SymbolType::Integer("3".to_string())
                ]
            );
            assert_eq!(line, 1);
        } else {
            assert!(false, format!("{:?}", err));
        }
    }
}
