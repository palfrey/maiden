use common::*;
use std::collections::HashMap;
use std::io::{self, Write};
use std::ops::Deref;

struct State<'a> {
    writer: &'a mut Write,
    variables: &'a mut HashMap<String, Expression>,
    current_line: u32,
    depth: u32,
    pronoun: Option<String>,
}

fn run_binop(
    state: &mut State,
    program: &Program,
    first: &Expression,
    second: &Expression,
    f: fn(&State, &Expression, &Expression) -> Result<bool>,
) -> Result<Expression> {
    let res_first = run_expression(state, program, first.deref())?;
    let res_second = run_expression(state, program, second.deref())?;
    debug!("first: {:?} second: {:?}", res_first, res_second);
    if f(state, &res_first, &res_second)? {
        Ok(Expression::True)
    } else {
        Ok(Expression::False)
    }
}

fn run_binop_shortcut(
    state: &mut State,
    program: &Program,
    first: &Expression,
    second: &Expression,
    f: fn(&State, &Expression, &Expression) -> Result<bool>,
) -> Result<bool> {
    let res_first = run_expression(state, program, first.deref())?;
    if let Expression::True = res_first {
        return Ok(true);
    }
    let res_second = run_expression(state, program, second.deref())?;
    debug!("first: {:?} second: {:?}", res_first, res_second);
    return Ok(f(state, &res_first, &res_second)?);
}

fn run_mathbinop(
    state: &mut State,
    program: &Program,
    first: &Expression,
    second: &Expression,
    f: fn(f64, f64) -> f64,
) -> Result<Expression> {
    let res_first = run_expression(state, program, first.deref())?;
    let res_second = run_expression(state, program, second.deref())?;
    debug!("first: {:?} second: {:?}", res_first, res_second);
    let first_value: f64 = match res_first {
        Expression::Floating(ref i) => *i,
        _ => {
            bail!(ErrorKind::Unimplemented(format!("Math op on non-number: {:?} {:?}", res_first, res_second), state.current_line));
        }
    };
    let second_value: f64 = match res_second {
        Expression::Floating(ref i) => *i,
        _ => {
            bail!(ErrorKind::Unimplemented(format!("Math op on non-number: {:?} {:?}", res_first, res_second), state.current_line));
        }
    };
    return Ok(Expression::Floating(f(first_value, second_value)));
}

fn to_boolean(state: &State, expression: &Expression) -> Result<bool> {
    if let Expression::False = *expression {
        Ok(false)
    } else if let Expression::True = *expression {
        Ok(true)
    } else if let Expression::Floating(ref val) = *expression {
        if *val == 0f64 {
            Ok(false)
        } else {
            Ok(true)
        }
    } else if let Expression::String(ref val) = *expression {
        if val.is_empty() {
            Ok(false)
        } else {
            Ok(true)
        }
    } else {
        bail!(ErrorKind::BadBooleanResolve(
            format!("{:?}", expression),
            state.current_line,
        ));
    }
}

fn call_function(
    state: &mut State,
    program: &Program,
    target: &str,
    args: &[Expression],
) -> Result<Expression> {
    let func_wrap = program.functions.get(target);
    if func_wrap.is_none() {
        bail!(ErrorKind::MissingFunction(
            target.to_string(),
            state.current_line,
        ));
    }
    let func = func_wrap.unwrap();
    if args.len() != func.args.len() {
        bail!(ErrorKind::WrongArgCount(
            func.args.len(),
            args.len(),
            state.current_line,
        ))
    }

    let mut new_variables = state.variables.clone();
    if state.depth == 100 {
        bail!(ErrorKind::StackOverflow(state.depth, state.current_line));
    }
    let mut new_state = State {
        writer: state.writer,
        variables: &mut new_variables,
        current_line: state.current_line,
        depth: state.depth + 1,
        pronoun: None
    };
    for (i, arg) in args.iter().enumerate() {
        let value = run_expression(&mut new_state, program, &arg)?;
        new_state
            .variables
            .insert(func.args[i].to_lowercase(), value);
    }
    return run_core(&mut new_state, program, func.location + 1);
}

fn run_expression(
    state: &mut State,
    program: &Program,
    expression: &Expression,
) -> Result<Expression> {
    debug!("Expression: {:?}", expression);
    return match *expression {
        Expression::Is(ref first, ref second) => {
            return run_binop(state, program, first, second, |_, f, s| Ok(f == s));
        }
        Expression::Aint(ref first, ref second) => {
            return run_binop(state, program, first, second, |_, f, s| Ok(f != s));
        }
        Expression::And(ref first, ref second) => {
            return run_binop(state, program, first, second, |st, f, s| {
                return Ok(to_boolean(st, f)? && to_boolean(st, s)?);
            });
        }
        Expression::Or(ref first, ref second) => {
            if run_binop_shortcut(state, program, first, second, |st, f, s| {
                return Ok(to_boolean(st, f)? || to_boolean(st, s)?);
            })? {
                return Ok(Expression::True);
            } else {
                return Ok(Expression::False);
            };
        }
        Expression::Nor(ref first, ref second) => {
            if run_binop_shortcut(state, program, first, second, |st, f, s| {
                return Ok(to_boolean(st, f)? || to_boolean(st, s)?);
            })? {
                return Ok(Expression::False);
            } else {
                return Ok(Expression::True);
            };
        }
        Expression::GreaterThanOrEqual(ref first, ref second) => {
            return run_binop(state, program, first, second, |_, f, s| Ok(f >= s));
        }
        Expression::GreaterThan(ref first, ref second) => {
            return run_binop(state, program, first, second, |_, f, s| Ok(f > s));
        }
        Expression::LessThanOrEqual(ref first, ref second) => {
            return run_binop(state, program, first, second, |_, f, s| Ok(f <= s));
        }
        Expression::LessThan(ref first, ref second) => {
            return run_binop(state, program, first, second, |_, f, s| Ok(f < s));
        }
        Expression::Subtract(ref first, ref second) => {
            return run_mathbinop(state, program, first, second, |f, s| f - s);
        }
        Expression::Add(ref first, ref second) => {
            return run_mathbinop(state, program, first, second, |f, s| f + s);
        }
        Expression::Times(ref first, ref second) => {
            return run_mathbinop(state, program, first, second, |f, s| f * s);
        }
        Expression::Divide(ref first, ref second) => {
            return run_mathbinop(state, program, first, second, |f, s| f / s);
        }
        Expression::Variable(ref name) => match state.variables.get(&name.to_lowercase()) {
            Some(exp) => {
                debug!("Got variable {} with value {:?}", &name, exp);
                Ok(exp.clone())
            }
            None => {
                bail!(ErrorKind::MissingVariable(name.clone(), state.current_line));
            }
        },
        Expression::Call(ref target, ref args) => call_function(state, program, target, args),
        Expression::Pronoun => {
            match state.pronoun {
                Some(ref pronoun) => {
                    match state.variables.get(&pronoun.to_lowercase()) {
                        Some(exp) => {
                            debug!("Got variable {} with value {:?}", &pronoun, exp);
                            Ok(exp.clone())
                        }
                        None => {
                            bail!(ErrorKind::MissingVariable(pronoun.clone(), state.current_line));
                        }
                    }
                }
                None => {
                    bail!(ErrorKind::UndefinedPronoun(state.current_line));
                }
            }
        },
        _ => Ok(expression.clone()),
    };
}

pub fn run(program: &Program, writer: &mut Write) -> Result<HashMap<String, Expression>> {
    let pc = 0;
    let mut variables: HashMap<String, Expression> = HashMap::new();
    {
        let mut state = State {
            variables: &mut variables,
            writer,
            current_line: 0,
            depth: 0,
            pronoun: None,
        };
        run_core(&mut state, program, pc)?;
    } // FIXME: Drop once NLL is merged
    return Ok(variables);
}

fn get_printable(value: &Expression, state: &mut State) -> Result<String> {
    match *value {
        Expression::Floating(ref x) => Ok(format!("{}", x)),
        Expression::String(ref s) => Ok(s.to_string()),
        Expression::Variable(ref x) => {
            let v = {
                let current_line = state.current_line;
                let v = state.variables.get(&x.to_lowercase());
                if v.is_none() {
                    bail!(ErrorKind::MissingVariable(x.to_string(), current_line));
                }
                v.unwrap().clone()
            };
            get_printable(&v, state)
        }
        Expression::True => Ok("true".to_string()),
        Expression::False => Ok("false".to_string()),
        Expression::Mysterious => Ok("mysterious".to_string()),
        Expression::Null => Ok("null".to_string()),
        _ => {
            bail!(ErrorKind::Unimplemented(
                format!("Say '{:?}'", value),
                state.current_line,
            ));
        }
    }
}

fn alter_variable(state: &mut State, target: &str, f: &Fn(f64) -> f64) -> Result<()> {
    let val = {
        let current_line = state.current_line;
        let v = state.variables.get(&target.to_lowercase());
        if v.is_none() {
            bail!(ErrorKind::MissingVariable(target.to_string(), current_line));
        }
        v.unwrap().clone()
    };
    debug!("Value of {} is {:?}", target, val);
    match val {
        Expression::Floating(x) => {
            state
                .variables
                .insert(target.to_lowercase(), Expression::Floating(f(x)));
        }
        _ => {
            bail!(ErrorKind::Unimplemented(
                format!("Attempt to alter non-integer '{}'", target),
                state.current_line,
            ));
        }
    };
    return Ok(());
}

fn run_core(state: &mut State, program: &Program, mut pc: usize) -> Result<(Expression)> {
    let mut total_instr = 0;
    loop {
        total_instr += 1;
        if total_instr > 1_000_000 {
            bail!(ErrorKind::InstructionLimit(state.current_line));
        }
        let command_line = match program.commands.get(pc) {
            Some(c) => c,
            None => break,
        };
        state.current_line = command_line.line;
        debug!("command: {:?}", command_line);
        match command_line.cmd {
            Command::Assignment {
                ref target,
                ref value,
            } => {
                let val = run_expression(state, program, &value)?;
                state.pronoun = Some(target.clone());
                state.variables.insert(target.to_lowercase(), val);
            }
            Command::Increment {
                ref target,
                ref count,
            } => {
                alter_variable(state, &target, &|x| x + count)?;
            }
            Command::Decrement {
                ref target,
                ref count,
            } => {
                alter_variable(state, &target, &|x| x - count)?;
            }
            Command::Until {
                ref expression,
                loop_end,
            } => {
                let resolve = run_expression(state, program, &expression)?;
                if to_boolean(state, &resolve)? {
                    match loop_end {
                        Some(val) => {
                            pc = val;
                        }
                        None => bail!(ErrorKind::NoEndLoop(state.current_line)),
                    }
                }
            }
            Command::While {
                ref expression,
                loop_end,
            } => {
                let resolve = run_expression(state, program, &expression)?;
                if !to_boolean(state, &resolve)? {
                    match loop_end {
                        Some(val) => {
                            pc = val;
                        }
                        None => bail!(ErrorKind::NoEndLoop(state.current_line)),
                    }
                }
            }
            Command::Next { loop_start } | Command::Continue { loop_start } => {
                pc = loop_start - 1;
            }
            Command::Say { ref value } => {
                let resolve = run_expression(state, program, &value)?;
                let x = get_printable(&resolve, state)?;
                writeln!(state.writer, "{}", x)?;
            }
            Command::FunctionDeclaration { func_end, .. } => match func_end {
                Some(val) => {
                    pc = val;
                }
                None => bail!(ErrorKind::NoEndFunction(state.current_line)),
            },
            Command::Return { ref return_value } => {
                return run_expression(state, program, &return_value);
            }
            Command::EndFunction => {
                return run_expression(state, program, &Expression::Nothing);
            }
            Command::If {
                ref expression,
                if_end,
                else_loc,
            } => {
                let resolve = run_expression(state, program, &expression)?;
                debug!("if: {:?} {:?}", &resolve, expression);
                if !to_boolean(state, &resolve)? {
                    match else_loc {
                        Some(val) => {
                            pc = val;
                        }
                        None => match if_end {
                            Some(val) => {
                                pc = val;
                            }
                            None => bail!(ErrorKind::NoEndOfIf(state.current_line)),
                        },
                    }
                }
            }
            Command::Else { if_start } => {
                let if_cmd = &program.commands[if_start];
                match if_cmd.cmd {
                    Command::If { if_end, .. } => match if_end {
                        Some(val) => {
                            pc = val;
                        }
                        None => bail!(ErrorKind::NoEndOfIf(state.current_line)),
                    },
                    _ => {
                        panic!("Matched else with non-if");
                    }
                }
            }
            Command::Call { ref name, ref args } => {
                call_function(state, program, &name, &args)?;
            }
            Command::EndIf => {}
            Command::Listen { target: ref opt_target } => {
                let mut input = String::new();
                io::stdin().read_line(&mut input)?;
                if let Some(target) = opt_target {
                    state.variables.insert(
                        target.to_lowercase(),
                        Expression::String(input.trim_right_matches('\n').to_string()),
                    );
                }
            }
        }
        pc += 1;
    }
    return Ok(Expression::Nothing);
}
