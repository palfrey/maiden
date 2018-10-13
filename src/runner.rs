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
    let res = run_binop_shortcut(state, program, first, second, None, false, f);
    if res? {
        Ok(Expression::True)
    } else {
        Ok(Expression::False)
    }
}

fn expression_to_number(inp: Expression) -> Expression {
    return match inp {
        Expression::Floating(_) => inp,
        Expression::Null => Expression::Floating(0.0),
        _ => {
            panic!("Can't convert {:?} to number", inp);
        }
    };
}

fn run_binop_shortcut(
    state: &mut State,
    program: &Program,
    first: &Expression,
    second: &Expression,
    shortcut: Option<Expression>,
    shortcut_return: bool,
    f: fn(&State, &Expression, &Expression) -> Result<bool>,
) -> Result<bool> {
    let res_first = run_expression(state, program, first.deref())?;
    if shortcut.is_some() && shortcut.unwrap() == res_first {
        return Ok(shortcut_return);
    }
    let res_second = run_expression(state, program, second.deref())?;
    debug!("first: {:?} second: {:?}", res_first, res_second);

    // Check for same types comparison first
    match res_first {
        Expression::True | Expression::False => match res_second {
            Expression::True | Expression::False => {
                return Ok(f(state, &res_first, &res_second)?);
            }
            _ => {}
        },
        Expression::String(_) => match res_second {
            Expression::String(_) => {
                return Ok(f(state, &res_first, &res_second)?);
            }
            _ => {}
        },
        _ => {}
    }

    // Try numeric conversion instead
    return Ok(f(
        state,
        &expression_to_number(res_first),
        &expression_to_number(res_second),
    )?);
}

fn run_mathbinop(
    state: &mut State,
    program: &Program,
    first: &Expression,
    second: &Expression,
    op: &Expression,
    f: fn(f64, f64) -> f64,
) -> Result<Expression> {
    let res_first = run_expression(state, program, first.deref())?;
    let res_second = run_expression(state, program, second.deref())?;
    match res_first {
        Expression::Floating(ref i) => {
            let first_value = *i;
            match res_second {
                Expression::Floating(ref i) => {
                    let second_value = *i;
                    return Ok(Expression::Floating(f(first_value, second_value)));
                }
                Expression::String(ref s_s) => match op {
                    Expression::Add(_, _) => {
                        return Ok(Expression::String(format!("{}{}", first_value, s_s)));
                    }
                    Expression::Times(_, _) => {
                        return Ok(Expression::String(s_s.repeat(first_value as usize)));
                    }
                    _ => {}
                },
                Expression::Null => {
                    return Ok(Expression::Floating(f(first_value, 0f64)));
                }
                _ => {}
            };
        }
        Expression::String(ref s_f) => match op {
            Expression::Add(_, _) => match res_second {
                Expression::String(ref s_s) => {
                    return Ok(Expression::String(s_f.clone() + s_s));
                }
                _ => {
                    let printed_second = get_printable(&res_second, state);
                    if let Ok(p_s) = printed_second {
                        return Ok(Expression::String(format!("{}{}", s_f, p_s)));
                    }
                }
            },
            Expression::Times(_, _) => match res_second {
                Expression::Floating(ref i) => {
                    let second_value = *i;
                    return Ok(Expression::String(s_f.repeat(second_value as usize)));
                }
                Expression::Null => {
                    return Ok(Expression::String("".to_string()));
                }
                _ => {}
            },
            _ => {}
        },
        Expression::Null => {
            match res_second {
                Expression::Floating(ref i) => {
                    let second_value = *i;
                    return Ok(Expression::Floating(f(0f64, second_value)));
                }
                Expression::String(ref s_s) => match op {
                    Expression::Add(_, _) => {
                        return Ok(Expression::String(format!("null{}", s_s)));
                    }
                    _ => {}
                },
                _ => {}
            };
        }
        _ => {
            if let Expression::Add(_, _) = op {
                match res_second {
                    Expression::String(ref s_s) => {
                        let printed_first = get_printable(&res_first, state);
                        if let Ok(p_f) = printed_first {
                            return Ok(Expression::String(format!("{}{}", p_f, s_s)));
                        }
                    }
                    _ => {}
                }
            }
        }
    };
    bail!(ErrorKind::Unimplemented(
        format!("Math op ({:?}) on values we can't apply", op),
        state.current_line
    ));
}

fn to_boolean(state: &State, expression: &Expression) -> Result<bool> {
    return match *expression {
        Expression::False | Expression::Mysterious | Expression::Null => Ok(false),
        Expression::True => Ok(true),
        Expression::Floating(ref val) => {
            if *val == 0f64 {
                Ok(false)
            } else {
                Ok(true)
            }
        }
        Expression::String(ref val) => {
            if val.is_empty() {
                Ok(false)
            } else {
                Ok(true)
            }
        }
        _ => {
            bail!(ErrorKind::BadBooleanResolve(
                format!("{:?}", expression),
                state.current_line,
            ));
        }
    };
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
        pronoun: None,
    };
    for (i, arg) in args.iter().enumerate() {
        let value = run_expression(&mut new_state, program, &arg)?;
        new_state
            .variables
            .insert(func.args[i].to_lowercase(), value);
    }
    return run_core(&mut new_state, program, func.location + 1);
}

#[cfg_attr(feature = "cargo-clippy", allow(cyclomatic_complexity))] // FIXME: break this up a bit
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
            let res = run_binop_shortcut(
                state,
                program,
                first,
                second,
                Some(Expression::False),
                false,
                |st, f, s| {
                    return Ok(to_boolean(st, f)? && to_boolean(st, s)?);
                },
            )?;
            if res {
                return Ok(Expression::True);
            } else {
                return Ok(Expression::False);
            };
        }
        Expression::Or(ref first, ref second) => {
            let res = run_binop_shortcut(
                state,
                program,
                first,
                second,
                Some(Expression::True),
                true,
                |st, f, s| {
                    return Ok(to_boolean(st, f)? || to_boolean(st, s)?);
                },
            )?;
            if res {
                return Ok(Expression::True);
            } else {
                return Ok(Expression::False);
            };
        }
        Expression::Nor(ref first, ref second) => {
            let res = run_binop_shortcut(
                state,
                program,
                first,
                second,
                Some(Expression::True),
                true,
                |st, f, s| {
                    return Ok(to_boolean(st, f)? || to_boolean(st, s)?);
                },
            )?;
            if res {
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
            return run_mathbinop(state, program, first, second, expression, |f, s| f - s);
        }
        Expression::Add(ref first, ref second) => {
            return run_mathbinop(state, program, first, second, expression, |f, s| f + s);
        }
        Expression::Times(ref first, ref second) => {
            return run_mathbinop(state, program, first, second, expression, |f, s| f * s);
        }
        Expression::Divide(ref first, ref second) => {
            return run_mathbinop(state, program, first, second, expression, |f, s| f / s);
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
        Expression::Pronoun => match state.pronoun {
            Some(ref pronoun) => match state.variables.get(&pronoun.to_lowercase()) {
                Some(exp) => {
                    debug!("Got variable {} with value {:?}", &pronoun, exp);
                    Ok(exp.clone())
                }
                None => {
                    bail!(ErrorKind::MissingVariable(
                        pronoun.clone(),
                        state.current_line
                    ));
                }
            },
            None => {
                bail!(ErrorKind::UndefinedPronoun(state.current_line));
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

fn flip_boolean(state: &mut State, target: &str, val: Expression, count: usize) -> Result<()> {
    if (count & 0x1) == 0 {
        // double-flips do nothing, so just look at the low bit
        return Ok(());
    }
    match val {
        Expression::True => state
            .variables
            .insert(target.to_lowercase(), Expression::False),
        Expression::False => state
            .variables
            .insert(target.to_lowercase(), Expression::True),
        _ => {
            bail!(ErrorKind::Unimplemented(
                format!("Attempt to flip non-boolean '{}'", target),
                state.current_line,
            ));
        }
    };
    return Ok(());
}

fn alter_variable(state: &mut State, target: &str, f: &Fn(f64) -> f64, count: usize) -> Result<()> {
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
        Expression::Null => {
            state
                .variables
                .insert(target.to_lowercase(), Expression::Floating(f(0f64)));
        }
        Expression::False | Expression::True => {
            return flip_boolean(state, target, val, count);
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
                alter_variable(state, &target, &|x| x + count, *count as usize)?;
            }
            Command::Decrement {
                ref target,
                ref count,
            } => {
                alter_variable(state, &target, &|x| x - count, *count as usize)?;
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
            Command::Break { loop_start } => {
                let loop_cmd = &program.commands[loop_start];
                match loop_cmd.cmd {
                    Command::While { loop_end, .. } => match loop_end {
                        Some(val) => {
                            pc = val;
                        }
                        None => bail!(ErrorKind::NoEndLoop(state.current_line)),
                    },
                    _ => {
                        panic!("Matched break with non-while");
                    }
                }
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
            Command::Listen {
                target: ref opt_target,
            } => {
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
