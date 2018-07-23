use nom::types::CompleteStr;
use common::Command;
use std::io::{Write, self};
use std::collections::HashMap;
use pretty_env_logger;
use parser;

#[derive(Debug, PartialEq, Clone)]
pub enum Variable {
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
    let words = parser::line(CompleteStr(value)).unwrap().1;
    let mut number = 0i32;
    for word in words.iter() {
        number *= 10;
        let len: i32 = (word.len() % 10) as i32;
        number += len;
    }
    return Variable::Integer(number);
}

pub fn run(commands: Vec<Command>, writer: &mut Write) -> Result<HashMap<String, Variable>, io::Error> {
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

#[test]
fn check_evaluate() {
    pretty_env_logger::try_init().unwrap_or(());
    let variables: HashMap<String, Variable> = HashMap::new();
    assert_eq!(evaluate(&variables, "a lovestruck ladykiller"), Variable::Integer(100));
    assert_eq!(evaluate(&variables, "nothing"), Variable::Integer(0));
}