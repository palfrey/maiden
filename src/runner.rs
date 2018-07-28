use common::*;
use std::io::Write;
use std::collections::HashMap;
use std::ops::Deref;

fn run_binop(variables: &HashMap<String, Expression>, first: &Expression, second: &Expression, f: fn(&Expression, &Expression) -> bool) -> Result<Expression> {
    let res_first = run_expression(variables, first.deref())?;
    let res_second = run_expression(variables, second.deref())?;
    debug!("first: {:?} second: {:?}", res_first, res_second);
    if f(&res_first, &res_second) {
        Ok(Expression::True)
    } else {
        Ok(Expression::False)
    }
}

fn run_expression(variables: &HashMap<String, Expression>, expression: &Expression) -> Result<Expression> {
    return match expression {
        &Expression::Is(ref first, ref second) => {
            return run_binop(variables, first, second, |f, s| {f == s});
        }
        &Expression::GreaterThanOrEqual(ref first, ref second) => {
            return run_binop(variables, first, second, |f, s| {f >= s});
        }
        &Expression::String(_) | &Expression::Integer(_) => {
            Ok(expression.clone())
        }
        &Expression::Variable(ref name) => {
            match variables.get(&name.to_lowercase()) {
                Some(exp) => Ok(exp.clone()),
                None => {
                    bail!(ErrorKind::MissingVariable(name.clone()));
                }
            }
        }
        _ => {
            warn!("No runner for {:?}", expression);
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
                let val = run_expression(&variables, value)?;
                variables.insert(target.to_lowercase(), val);
            }
            Command::Increment {target} => {
                let val = variables.get(&target.to_lowercase()).expect(&format!("Can't find '{}'", target)).clone();
                info!("Value of {} is {:?}", target, val);
                match val {
                    Expression::Integer(x) => {
                        variables.insert(target.to_lowercase(), Expression::Integer(x+1));
                    }
                    _ => {
                        error!("Attempt to increment non-integer '{}'", target);
                    }
                };
            }
            Command::Until {expression, loop_end} => {
                let resolve = run_expression(&variables, expression);
                if let Ok(Expression::True) = resolve {
                    pc = loop_end.expect("loop_end");
                }
                else if let Ok(Expression::False) = resolve {
                    // all fine
                }
                else {
                    //warn!("Bad expression resolve: {:?}", resolve);
                    panic!("Bad expression resolve: {:?}", resolve);
                }
            }
            Command::While {expression, loop_end} => {
                let resolve = run_expression(&variables, expression);
                if let Ok(Expression::False) = resolve {
                    pc = loop_end.expect("loop_end");
                }
                else if let Ok(Expression::True) = resolve {
                    // all fine
                }
                else {
                    panic!("Bad expression resolve: {:?}", resolve);
                }
            }
            Command::Next {loop_start} => {
                pc = loop_start-1;
            }
            Command::Say {value} => {
                match value {
                    Expression::Integer(x) => writeln!(writer, "{}", x)?,
                    Expression::String(s) => writeln!(writer, "{}", s)?,
                    _ => {
                        warn!("{:?}", value);
                        unimplemented!();
                    }
                };
            }
            Command::FunctionDeclaration {name, args, func_end} => {
                //warn!("Command: {:?}", command);
                //unimplemented!();
                pc = func_end.expect("func_end");
            }
            Command::EndFunction => {
                warn!("Command: {:?}", command);
                unimplemented!();
            }
        }
        pc +=1;
    }
    return Ok(variables);
}