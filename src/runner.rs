use nom::types::CompleteStr;
use common::*;
use std::io::Write;
use std::collections::HashMap;
use pretty_env_logger;
use parser;

#[derive(Debug, PartialEq, Clone)]
pub enum Variable {
    Integer(i32),
    String(String)
}

pub fn is_alphabetic(chr: char) -> bool {
  (chr >= 'A' && chr <= 'Z') || (chr >= 'a' && chr <= 'z')
}

fn evaluate(variables: &HashMap<String, Variable>, value: &str) -> Result<Variable> {
    if value == "nothing" {
        return Ok(Variable::Integer(0));
    }
    let as_int = value.parse::<i32>();
    if let Ok(int) = as_int {
        return Ok(Variable::Integer(int));
    }
    if let Some(val) = variables.get(value) {
        return Ok(val.clone());
    }
    let words = parser::line(CompleteStr(value)).unwrap().1;
    if words.len() == 1 {
        let word = &words[0];
        if word.chars().next().unwrap() == '\"' && word.chars().last().unwrap() == '\"' {
            return Ok(Variable::String(word[1..word.len()-1].to_string()));
        }
    }
    let mut number = 0i32;
    for word in words.iter() {
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
    return Ok(Variable::Integer(number));
}

pub fn run(commands: Vec<Command>, writer: &mut Write) -> Result<HashMap<String, Variable>> {
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
                let val = evaluate(&variables, value)?;
                variables.insert(target.to_string(), val);
            }
            Command::Increment {target} => {
                let val = variables.get(target.as_str()).expect(&format!("Can't find '{}'", target)).clone();
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
                    Variable::Integer(x) => writeln!(writer, "{}", x)?,
                    Variable::String(s) => writeln!(writer, "{}", s)?
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
    let variables: HashMap<String, Variable> = HashMap::new();
    assert_eq!(evaluate(&variables, "a lovestruck ladykiller").unwrap(), Variable::Integer(100));
    assert_eq!(evaluate(&variables, "nothing").unwrap(), Variable::Integer(0));
}