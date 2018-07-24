use nom::types::CompleteStr;
use common::*;
use std::io::Write;
use std::collections::HashMap;
use pretty_env_logger;
use parser;

pub fn is_alphabetic(chr: char) -> bool {
  (chr >= 'A' && chr <= 'Z') || (chr >= 'a' && chr <= 'z')
}

fn evaluate(variables: &HashMap<String, Expression>, value: &SymbolType) -> Result<Expression> {
    match value {
        SymbolType::Words(words) => {
            if words == "nothing" {
                return Ok(Expression::Integer(0));
            }
            let as_int = words.parse::<i32>();
            if let Ok(int) = as_int {
                return Ok(Expression::Integer(int));
            }
            if let Some(val) = variables.get(words) {
                return Ok(val.clone());
            }
            let symbols = parser::line(CompleteStr(words)).unwrap().1;
            if symbols.len() == 1 {
                if let SymbolType::Words(word) = &symbols[0] {
                    if word.chars().next().unwrap() == '\"' && word.chars().last().unwrap() == '\"' {
                        return Ok(Expression::String(word[1..word.len()-1].to_string()));
                    }
                }
                else {
                    unimplemented!();
                }
            }
            let mut number = 0i32;
            for sym in symbols.iter() {
                if let SymbolType::Words(word) = sym {
                    word.chars().try_for_each(|c| if !is_alphabetic(c) {
                            Err(ErrorKind::NonAlphabeticWord(word.to_string()))
                        }
                        else {
                            Ok(())
                        })?;
                    number *= 10;
                    let len: i32 = (word.len() % 10) as i32;
                    number += len;
                }
                else {
                    panic!("Need to cope with '{:?}'", sym);
                }
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

pub fn run(commands: Vec<Command>, writer: &mut Write) -> Result<HashMap<String, Expression>> {
    let mut pc = 0;
    let mut variables: HashMap<String, Expression> = HashMap::new();
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
                let val = evaluate(&variables, value)?;
                variables.insert(target.to_string(), val);
            }
            Command::Increment {target} => {
                let val = variables.get(target.as_str()).expect(&format!("Can't find '{}'", target)).clone();
                info!("Value of {} is {:?}", target, val);
                match val {
                    Expression::Integer(x) => {
                        variables.insert(target.to_string(), Expression::Integer(x+1));
                    }
                    _ => {
                        error!("Attempt to increment non-integer '{}'", target);
                    }
                };
            }
            Command::UntilIs {target, value, loop_end} => {
                let target = variables.get(target.as_str()).expect(&format!("Can't find '{}'", target)).clone();
                let value = evaluate(&variables, value)?;
                if target == value {
                    pc = loop_end.expect("loop_end");
                }
            }
            Command::Next {loop_start} => {
                pc = loop_start-1;
            }
            Command::Say {value} => {
                let value = evaluate(&variables, value)?;
                match value {
                    Expression::Integer(x) => writeln!(writer, "{}", x)?,
                    Expression::String(s) => writeln!(writer, "{}", s)?
                };
            }
        }
        pc +=1;
    }
    return Ok(variables);
}

#[test]
fn check_evaluate() {
    pretty_env_logger::try_init().unwrap_or(());
    let variables: HashMap<String, Expression> = HashMap::new();
    assert_eq!(evaluate(&variables, &SymbolType::Words("a lovestruck ladykiller".to_string())).unwrap(), Expression::Integer(100));
    assert_eq!(evaluate(&variables, &SymbolType::Words("nothing".to_string())).unwrap(), Expression::Integer(0));
}