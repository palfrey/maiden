use crate::common::*;
use log::debug;
use std::collections::{BTreeMap, HashMap};
use std::io::{self, Write};
use std::ops::Deref;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum VariableType {
    Global,
    Local,
}

struct State<'a> {
    writer: &'a mut dyn Write,
    variables: &'a mut HashMap<String, (VariableType, Expression)>,
    current_line: usize,
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

fn expression_to_number(inp: Expression, line: usize) -> Result<Expression> {
    debug!("x to number: {:?}", inp);
    return match inp {
        Expression::Floating(_) => Ok(inp),
        Expression::Null => Ok(Expression::Floating(0.0)),
        Expression::String(ref s) => {
            let as_float = s.parse::<f64>();
            if let Ok(float) = as_float {
                return Ok(Expression::Floating(float));
            } else {
                return Err(MaidenError::ParseNumberError {
                    number: s.to_string(),
                    line,
                });
            }
        }
        _ => {
            return Err(MaidenError::Unimplemented {
                description: format!("Can't convert {:?} to number", inp),
                line,
            });
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
                return f(state, &res_first, &res_second);
            }
            _ => {
                let val = to_boolean(state, &res_second);
                if let Ok(b) = val {
                    if b {
                        return f(state, &res_first, &Expression::True);
                    } else {
                        return f(state, &res_first, &Expression::False);
                    }
                }
            }
        },
        Expression::String(_) => {
            if let Expression::String(_) = res_second {
                return f(state, &res_first, &res_second);
            }
            if Expression::Null == res_second {
                return Ok(false);
            }
        }
        Expression::Mysterious => {
            if let Expression::Mysterious = res_second {
                return Ok(true);
            } else {
                return Ok(false);
            }
        }
        _ => {}
    }
    match res_second {
        Expression::True | Expression::False => {
            let val = to_boolean(state, &res_first);
            if let Ok(b) = val {
                if b {
                    return f(state, &Expression::True, &res_second);
                } else {
                    return f(state, &Expression::False, &res_second);
                }
            }
        }
        _ => {}
    }
    let converted_second = expression_to_number(res_second, state.current_line);
    if converted_second.is_err() {
        return Ok(false);
    }

    // Try numeric conversion instead
    return f(
        state,
        &expression_to_number(res_first, state.current_line)?,
        &converted_second?,
    );
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
                Expression::String(ref s_s) => {
                    if let Expression::Add(_, _) = op {
                        return Ok(Expression::String(format!("null{}", s_s)));
                    }
                }
                Expression::Null => {
                    return Ok(Expression::Floating(f(0f64, 0f64)));
                }
                _ => {}
            };
        }
        _ => {
            if let Expression::Add(_, _) = op {
                if let Expression::String(ref s_s) = res_second {
                    let printed_first = get_printable(&res_first, state);
                    if let Ok(p_f) = printed_first {
                        return Ok(Expression::String(format!("{}{}", p_f, s_s)));
                    }
                }
            }
        }
    };
    return Err(MaidenError::Unimplemented {
        description: format!(
            "Math op ({:?}) on values we can't apply: {:?} {:?}",
            op, res_first, res_second
        ),
        line: state.current_line,
    });
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
        Expression::String(ref val) => Ok(!matches!(val.to_lowercase().as_str(), "")),
        Expression::Object(_) => Ok(true),
        _ => {
            return Err(MaidenError::BadBooleanResolve {
                expression: format!("{:?}", expression),
                line: state.current_line,
            });
        }
    };
}

fn call_function(
    state: &mut State,
    program: &Program,
    target: &str,
    args: &[Expression],
) -> Result<Expression> {
    let func_wrap = program.functions.get(&target.to_lowercase());
    if func_wrap.is_none() {
        return Err(MaidenError::MissingFunction {
            name: target.to_string(),
            line: state.current_line,
        });
    }
    let func = func_wrap.unwrap();
    if args.len() != func.args.len() {
        return Err(MaidenError::WrongArgCount {
            expected: func.args.len(),
            got: args.len(),
            line: state.current_line,
        });
    }

    let mut new_variables = state.variables.clone();
    if state.depth == 100 {
        return Err(MaidenError::StackOverflow {
            depth: state.depth,
            line: state.current_line,
        });
    }
    let mut new_state = State {
        writer: state.writer,
        variables: &mut new_variables,
        current_line: state.current_line,
        depth: state.depth + 1,
        pronoun: None,
    };
    for (i, arg) in args.iter().enumerate() {
        let value = run_expression(&mut new_state, program, arg)?;
        new_state
            .variables
            .insert(func.args[i].to_lowercase(), (VariableType::Local, value));
    }

    let result = run_core(
        &mut new_state,
        &mut Program {
            commands: func.block.commands.clone(),
            functions: program.functions.clone(),
        },
        0,
    );
    for (name, (kind, value)) in new_state.variables.iter() {
        if kind == &VariableType::Global {
            state
                .variables
                .insert(name.to_string(), (VariableType::Global, value.clone()));
        }
    }
    return result;
}

#[allow(clippy::cognitive_complexity)] // FIXME: break this up a bit
fn run_expression(
    state: &mut State,
    program: &Program,
    expression: &Expression,
) -> Result<Expression> {
    debug!("Expression: {:?}", expression);
    return match *expression {
        Expression::Is(ref first, ref second) => {
            if let Expression::Not(not_second) = second.deref() {
                return run_binop(state, program, first, not_second, |_, f, s| Ok(f != s));
            } else {
                return run_binop(state, program, first, second, |_, f, s| Ok(f == s));
            }
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
            let res = run_mathbinop(state, program, first, second, expression, |f, s| f / s);
            if let Ok(ok) = res {
                if let Expression::Floating(val) = ok {
                    if val == std::f64::INFINITY {
                        return Err(MaidenError::Infinity {
                            x: format!("{:?}", first),
                            y: format!("{:?}", second),
                            line: state.current_line,
                        });
                    }
                }
                Ok(ok)
            } else {
                res
            }
        }
        Expression::Variable(ref name) => match state.variables.get(&name.to_lowercase()) {
            Some((_, exp)) => {
                debug!("Got variable {} with value {:?}", &name, exp);
                Ok(exp.clone())
            }
            None => {
                if program.functions.get(name).is_some() {
                    return Ok(Expression::Object(name.clone()));
                }
                return Err(MaidenError::MissingVariable {
                    name: name.clone(),
                    line: state.current_line,
                });
            }
        },
        Expression::Call(ref target, ref args) => call_function(state, program, target, args),
        Expression::Pronoun => match state.pronoun {
            Some(ref pronoun) => match state.variables.get(&pronoun.to_lowercase()) {
                Some((_, exp)) => {
                    debug!("Got variable {} with value {:?}", &pronoun, exp);
                    Ok(exp.clone())
                }
                None => {
                    return Err(MaidenError::MissingVariable {
                        name: pronoun.clone(),
                        line: state.current_line,
                    });
                }
            },
            None => {
                return Err(MaidenError::UndefinedPronoun {
                    line: state.current_line,
                });
            }
        },
        Expression::Not(ref arg) => {
            let res = run_expression(state, program, arg)?;
            let boolean = to_boolean(state, &res);
            if let Ok(good_boolean) = boolean {
                if good_boolean {
                    return Ok(Expression::False);
                } else {
                    return Ok(Expression::True);
                }
            } else {
                return Ok(Expression::False);
            }
        }
        _ => Ok(expression.clone()),
    };
}

pub fn run(
    program: &mut Program,
    writer: &mut dyn Write,
) -> Result<HashMap<String, (VariableType, Expression)>> {
    let pc = 0;
    let mut variables = HashMap::new();
    let mut state = State {
        variables: &mut variables,
        writer,
        current_line: 0,
        depth: 0,
        pronoun: None,
    };
    run_core(&mut state, program, pc)?;
    return Ok(variables);
}

fn get_printable(value: &Expression, state: &State) -> Result<String> {
    match *value {
        Expression::Floating(ref x) => Ok(format!("{}", x)),
        Expression::String(ref s) => Ok(s.to_string()),
        Expression::Variable(ref x) => {
            let (_, v) = {
                let current_line = state.current_line;
                let v = state.variables.get(&x.to_lowercase());
                if v.is_none() {
                    return Err(MaidenError::MissingVariable {
                        name: x.to_string(),
                        line: current_line,
                    });
                }
                v.unwrap().clone()
            };
            get_printable(&v, state)
        }
        Expression::Array { ref numeric, .. } => {
            Ok(format!("{}", numeric.keys().max().map_or(0, |x| x + 1)))
        }
        Expression::ArrayRef {
            ref name,
            ref index,
        } => {
            let mysterious_box = Box::new(Expression::Mysterious);
            let string_box;
            let v = {
                let current_line = state.current_line;
                let var_name = match **name {
                    Expression::Variable(ref s) => s,
                    _ => {
                        panic!("Other expression for array name: {:?}", name);
                    }
                };
                let v = state.variables.get(&var_name.to_lowercase());
                if v.is_none() {
                    return Err(MaidenError::MissingVariable {
                        name: var_name.to_string(),
                        line: current_line,
                    });
                }
                let mut local_index = &**index;
                let variable;
                if let Expression::Variable(ref var) = local_index {
                    variable = state.variables.get(var).unwrap().1.clone();
                    local_index = &variable;
                }
                let entry = match v.unwrap().1 {
                    Expression::Array {
                        ref numeric,
                        ref strings,
                    } => match local_index {
                        Expression::String(ref s) => strings.get(s),
                        Expression::Floating(f) => numeric.get(&(*f as usize)),
                        _ => {
                            panic!("Don't know how to array lookup with: {:?}", local_index);
                        }
                    },
                    Expression::String(ref s) => match local_index {
                        Expression::Floating(f) => {
                            let g = *f as usize;
                            match s.get(g..(g + 1)) {
                                Some(slice) => {
                                    string_box = Box::new(Expression::String(slice.to_string()));
                                    Some(&string_box)
                                }
                                None => None,
                            }
                        }
                        _ => {
                            panic!("Don't know how to do string lookup with: {:?}", local_index);
                        }
                    },
                    _ => {
                        panic!("Array ref to non-array: {:?}", v);
                    }
                };
                entry.unwrap_or(&mysterious_box)
            };
            get_printable(v, state)
        }
        Expression::True => Ok("true".to_string()),
        Expression::False => Ok("false".to_string()),
        Expression::Mysterious => Ok("mysterious".to_string()),
        Expression::Null => Ok("null".to_string()),
        _ => {
            return Err(MaidenError::Unimplemented {
                description: format!("Say '{:?}'", value),
                line: state.current_line,
            });
        }
    }
}

fn flip_boolean(
    state: &mut State,
    target: &str,
    val: &Expression,
    count: usize,
    kind: VariableType,
) -> Result<()> {
    if (count & 0x1) == 0 {
        // double-flips do nothing, so just look at the low bit
        return Ok(());
    }
    match val {
        Expression::True => state
            .variables
            .insert(target.to_lowercase(), (kind, Expression::False)),
        Expression::False => state
            .variables
            .insert(target.to_lowercase(), (kind, Expression::True)),
        _ => {
            return Err(MaidenError::Unimplemented {
                description: format!("Attempt to flip non-boolean '{}'", target),
                line: state.current_line,
            });
        }
    };
    return Ok(());
}

fn alter_variable(
    state: &mut State,
    target: &Expression,
    f: &dyn Fn(f64) -> f64,
    count: usize,
) -> Result<()> {
    let name = match target {
        Expression::Variable(n) => n.to_lowercase(),
        Expression::Pronoun => state.pronoun.as_ref().unwrap().to_lowercase(),
        _ => {
            return Err(MaidenError::Unimplemented {
                line: state.current_line,
                description: String::from("Attempt to alter a non-variable expression"),
            });
        }
    };
    let (kind, val) = {
        let current_line = state.current_line;
        let v = state.variables.get(&name);
        if v.is_none() {
            return Err(MaidenError::MissingVariable {
                name: name.to_string(),
                line: current_line,
            });
        }
        v.unwrap().clone()
    };
    debug!("Value of {} is {:?}", name, val);
    match val {
        Expression::Floating(x) => {
            state
                .variables
                .insert(name, (kind, Expression::Floating(f(x))));
        }
        Expression::Null => {
            state
                .variables
                .insert(name, (kind, Expression::Floating(f(0f64))));
        }
        Expression::False | Expression::True => {
            return flip_boolean(state, &name, &val, count, kind);
        }
        _ => {
            return Err(MaidenError::Unimplemented {
                description: format!("Attempt to alter non-integer '{}'", name),
                line: state.current_line,
            });
        }
    };
    return Ok(());
}

fn round_variable(state: &mut State, target: &Expression, f: &dyn Fn(f64) -> f64) -> Result<()> {
    let name = match target {
        Expression::Variable(n) => n.to_lowercase(),
        Expression::Pronoun => state.pronoun.as_ref().unwrap().to_lowercase(),
        _ => {
            return Err(MaidenError::Unimplemented {
                line: state.current_line,
                description: String::from("Attempt to round a non-variable expression"),
            });
        }
    };
    let (kind, val) = {
        let current_line = state.current_line;
        let v = state.variables.get(&name);
        if v.is_none() {
            return Err(MaidenError::MissingVariable {
                name,
                line: current_line,
            });
        }
        v.unwrap().clone()
    };
    debug!("Value of {} is {:?}", name, val);
    match val {
        Expression::Floating(x) => {
            state
                .variables
                .insert(name, (kind, Expression::Floating(f(x))));
        }
        Expression::Null => {
            state
                .variables
                .insert(name, (kind, Expression::Floating(f(0f64))));
        }
        _ => {
            return Err(MaidenError::Unimplemented {
                description: format!("Attempt to alter non-integer '{}'", name),
                line: state.current_line,
            });
        }
    };
    return Ok(());
}

fn get_variable_type(state: &State) -> VariableType {
    if state.depth == 0 {
        VariableType::Global
    } else {
        VariableType::Local
    }
}

#[allow(clippy::cognitive_complexity)] // FIXME: break this up a bit
fn run_core(state: &mut State, program: &mut Program, mut pc: usize) -> Result<Expression> {
    let mut total_instr = 0;
    loop {
        total_instr += 1;
        if total_instr > 1_000_000 {
            return Err(MaidenError::InstructionLimit {
                line: state.current_line,
            });
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
                let val = run_expression(state, program, value)?;
                match &**target {
                    Expression::Variable(name) => {
                        state.pronoun = Some(name.clone());
                        let kind = if let Some((inner_kind, _var)) =
                            state.variables.get(&name.to_lowercase())
                        {
                            *inner_kind
                        } else {
                            get_variable_type(state)
                        };
                        state.variables.insert(name.to_lowercase(), (kind, val));
                    }
                    Expression::Pronoun => {
                        let pronoun = state.pronoun.as_ref().unwrap();
                        state
                            .variables
                            .insert(pronoun.to_lowercase(), (get_variable_type(state), val));
                    }
                    // FIXME: improve with box patterns once stabilised https://github.com/rust-lang/rust/issues/29641
                    Expression::ArrayRef { name, index } => {
                        if let Expression::Variable(var_name) = name.deref() {
                            let mut local_index = &**index;
                            let variable;
                            if let Expression::Variable(ref var) = local_index {
                                variable = state.variables.get(var).unwrap().clone().1;
                                local_index = &variable;
                            }
                            match local_index {
                                Expression::Floating(ref idx) => {
                                    if let Some((_kind, array)) = state.variables.get_mut(var_name)
                                    {
                                        if let Expression::Array {
                                            ref mut numeric, ..
                                        } = array
                                        {
                                            numeric.insert(*idx as usize, Box::new(val));
                                        } else {
                                            panic!(
                                                "Array ref assignment to non-array {} {}",
                                                var_name, idx
                                            );
                                        }
                                    } else {
                                        let mut numeric = BTreeMap::new();
                                        numeric.insert(*idx as usize, Box::new(val));
                                        state.variables.insert(
                                            var_name.to_string(),
                                            (
                                                get_variable_type(state),
                                                Expression::Array {
                                                    numeric,
                                                    strings: BTreeMap::new(),
                                                },
                                            ),
                                        );
                                    }
                                }
                                Expression::String(ref idx) => {
                                    if let Some((_kind, array)) = state.variables.get_mut(var_name)
                                    {
                                        if let Expression::Array {
                                            ref mut strings, ..
                                        } = array
                                        {
                                            strings.insert(idx.to_string(), Box::new(val));
                                        } else {
                                            panic!(
                                                "Array ref assignment to non-array {} {}",
                                                var_name, idx
                                            );
                                        }
                                    } else {
                                        let mut strings = BTreeMap::new();
                                        strings.insert(idx.to_string(), Box::new(val));
                                        state.variables.insert(
                                            var_name.to_string(),
                                            (
                                                get_variable_type(state),
                                                Expression::Array {
                                                    numeric: BTreeMap::new(),
                                                    strings,
                                                },
                                            ),
                                        );
                                    }
                                }
                                _ => {
                                    panic!("Index assignment with {:?}", index);
                                }
                            }
                        }
                    }
                    _ => {
                        panic!("Don't know how to assign to {:?}", target);
                    }
                }
            }
            Command::Increment {
                ref target,
                ref count,
            } => {
                alter_variable(state, target, &|x| x + count, *count as usize)?;
            }
            Command::Decrement {
                ref target,
                ref count,
            } => {
                alter_variable(state, target, &|x| x - count, *count as usize)?;
            }
            Command::Until {
                ref expression,
                ref block,
            } => loop {
                let resolve = run_expression(state, program, expression)?;
                if to_boolean(state, &resolve)? {
                    break;
                }
                run_core(
                    state,
                    &mut Program {
                        commands: block.commands.clone(),
                        functions: program.functions.clone(),
                    },
                    0,
                )
                .unwrap();
            },
            Command::While {
                ref expression,
                ref block,
            } => loop {
                let resolve = run_expression(state, program, expression)?;
                if !to_boolean(state, &resolve)? {
                    break;
                }
                let res = run_core(
                    state,
                    &mut Program {
                        commands: block.commands.clone(),
                        functions: program.functions.clone(),
                    },
                    0,
                )
                .unwrap();
                if res == Expression::Break {
                    break;
                }
            },
            Command::Continue => {
                return Ok(Expression::Continue);
            }
            Command::Break => {
                return Ok(Expression::Break);
            }
            Command::Say { ref value } => {
                let resolve = run_expression(state, program, value)?;
                let x = get_printable(&resolve, state)?;
                writeln!(state.writer, "{}", x)?;
            }
            Command::FunctionDeclaration {
                ref name,
                ref args,
                ref block,
            } => {
                program.functions.insert(
                    name.to_lowercase().to_string(),
                    Function {
                        args: args.to_vec(),
                        block: block.clone(),
                    },
                );
            }
            Command::Return { ref return_value } => {
                return run_expression(state, program, return_value);
            }
            Command::If {
                ref expression,
                ref then,
                ref otherwise,
            } => {
                let resolve = run_expression(state, program, expression)?;
                debug!("if: {:?} {:?}", &resolve, expression);
                if to_boolean(state, &resolve)? {
                    if let Some(block) = then {
                        let res = run_core(
                            state,
                            &mut Program {
                                commands: block.commands.clone(),
                                functions: program.functions.clone(),
                            },
                            0,
                        )
                        .unwrap();
                        if res != Expression::Nothing {
                            return Ok(res);
                        }
                    }
                } else if let Some(block) = otherwise {
                    let res = run_core(
                        state,
                        &mut Program {
                            commands: block.commands.clone(),
                            functions: program.functions.clone(),
                        },
                        0,
                    )
                    .unwrap();
                    if res != Expression::Nothing {
                        return Ok(res);
                    }
                }
            }
            Command::Call { ref name, ref args } => {
                call_function(state, program, name, args)?;
            }
            Command::Listen {
                target: ref opt_target,
            } => {
                let mut input = String::new();
                io::stdin().read_line(&mut input)?;
                if let Some(target) = opt_target {
                    state.variables.insert(
                        target.to_lowercase(),
                        (
                            get_variable_type(state),
                            Expression::String(input.trim_end_matches('\n').to_string()),
                        ),
                    );
                }
            }
            Command::Round { ref target } => {
                round_variable(state, target, &|x| x.round())?;
            }
            Command::Ceil { ref target } => {
                round_variable(state, target, &|x| x.ceil())?;
            }
            Command::Floor { ref target } => {
                round_variable(state, target, &|x| x.floor())?;
            }
            Command::Mutation {
                ref mutator,
                ref source,
                ref target,
                ref lookup,
                ref modifier,
            } => match mutator {
                SymbolType::Cast => {
                    if source.is_some()
                        || target.is_some()
                        || lookup.is_none()
                        || modifier.is_some()
                    {
                        unimplemented!(
                            "Cast for {:?} {:?} {:?} {:?}",
                            source,
                            target,
                            lookup,
                            modifier
                        );
                    }
                    if let Expression::Variable(var_name) = lookup.as_ref().unwrap().deref() {
                        match state.variables.get(&var_name.to_lowercase()).unwrap() {
                            (kind, Expression::String(ref s)) => {
                                let val = f64::from_str(s).unwrap();
                                let new_kind = *kind;
                                state.variables.insert(
                                    var_name.to_lowercase(),
                                    (new_kind, Expression::Floating(val)),
                                );
                            }
                            (kind, Expression::Floating(f)) => {
                                let val = std::char::from_u32(*f as u32).unwrap().to_string();
                                let new_kind = *kind;
                                state.variables.insert(
                                    var_name.to_lowercase(),
                                    (new_kind, Expression::String(val)),
                                );
                            }
                            var => {
                                unimplemented!("Cast for {:?}", var);
                            }
                        }
                    } else {
                        unimplemented!("Cast for non-variable: {:?}", lookup);
                    }
                }
                SymbolType::Split => {
                    let split_by = modifier
                        .as_ref()
                        .map(|b| match b.deref() {
                            Expression::String(s) => s.as_str(),
                            other => panic!("Modifier with non-string: {:?}", other),
                        })
                        .unwrap_or("");
                    let split_array = |to_split: &String| Expression::Array {
                        numeric: to_split
                            .split(split_by)
                            .filter(|x| !x.is_empty())
                            .enumerate()
                            .map(|(k, v)| (k, Box::new(Expression::String(v.to_string()))))
                            .collect(),
                        strings: BTreeMap::new(),
                    };
                    if lookup.is_some() {
                        if let Expression::Variable(var_name) = lookup.as_ref().unwrap().deref() {
                            match state.variables.get(&var_name.to_lowercase()).unwrap() {
                                (kind, Expression::String(s)) => {
                                    let val = split_array(s);
                                    let new_kind = *kind;
                                    state
                                        .variables
                                        .insert(var_name.to_lowercase(), (new_kind, val));
                                }
                                var => {
                                    unimplemented!("Split for {:?}", var);
                                }
                            }
                        } else {
                            unimplemented!("Split for non-variable: {:?}", lookup);
                        }
                    } else if target.is_some() && source.is_some() {
                        match source.as_ref().unwrap().deref() {
                            Expression::String(src) => {
                                if let Expression::Variable(tar) = target.as_ref().unwrap().deref()
                                {
                                    let val = split_array(src);
                                    state.variables.insert(
                                        tar.to_lowercase(),
                                        (get_variable_type(state), val),
                                    );
                                } else {
                                    unimplemented!("Split to {:?}", target);
                                }
                            }
                            Expression::Variable(src) => {
                                if let Expression::Variable(tar) = target.as_ref().unwrap().deref()
                                {
                                    let var = state.variables.get(src).unwrap();
                                    let kind = var.0;
                                    if let Expression::String(ref var_str) = var.1 {
                                        let val = split_array(var_str);
                                        state.variables.insert(tar.to_lowercase(), (kind, val));
                                    } else {
                                        unimplemented!("Split of {:?}", var);
                                    }
                                } else {
                                    unimplemented!("Split to {:?}", target);
                                }
                            }
                            _ => {
                                unimplemented!("Split for {:?}", source);
                            }
                        }
                    } else {
                        unimplemented!(
                            "Split for {:?} {:?} {:?} {:?}",
                            source,
                            target,
                            lookup,
                            modifier
                        );
                    }
                }
                SymbolType::Join => {
                    let join_with = modifier
                        .as_ref()
                        .map(|b| match b.deref() {
                            Expression::String(s) => s.as_str(),
                            other => panic!("Modifier with non-string: {:?}", other),
                        })
                        .unwrap_or("");
                    let join_array = |to_join: &BTreeMap<usize, Box<Expression>>| {
                        Expression::String(
                            to_join
                                .values()
                                .map(|b| match b.deref() {
                                    Expression::String(s) => s.as_str(),
                                    other => panic!("Modifier with non-string: {:?}", other),
                                })
                                .fold(String::new(), |acc, x| {
                                    if acc.is_empty() {
                                        x.to_string()
                                    } else {
                                        acc + join_with + x
                                    }
                                }),
                        )
                    };
                    if lookup.is_some() {
                        if let Expression::Variable(var_name) = lookup.as_ref().unwrap().deref() {
                            match state.variables.get(&var_name.to_lowercase()).unwrap() {
                                (kind, Expression::Array { ref numeric, .. }) => {
                                    let val = join_array(numeric);
                                    let new_kind = *kind;
                                    state
                                        .variables
                                        .insert(var_name.to_lowercase(), (new_kind, val));
                                }
                                var => {
                                    unimplemented!("Join for {:?}", var);
                                }
                            }
                        } else {
                            unimplemented!("Join for non-variable: {:?}", lookup);
                        }
                    } else if target.is_some() && source.is_some() {
                        match source.as_ref().unwrap().deref() {
                            Expression::Array { ref numeric, .. } => {
                                if let Expression::Variable(tar) = target.as_ref().unwrap().deref()
                                {
                                    let val = join_array(numeric);
                                    state.variables.insert(
                                        tar.to_lowercase(),
                                        (get_variable_type(state), val),
                                    );
                                } else {
                                    unimplemented!("Join to {:?}", target);
                                }
                            }
                            Expression::Variable(src) => {
                                if let Expression::Variable(tar) = target.as_ref().unwrap().deref()
                                {
                                    let (kind, var) = state.variables.get(src).unwrap();
                                    if let Expression::Array { ref numeric, .. } = var {
                                        let val = join_array(numeric);
                                        let new_kind = *kind;
                                        state.variables.insert(tar.to_lowercase(), (new_kind, val));
                                    } else {
                                        unimplemented!("Join of {:?}", var);
                                    }
                                } else {
                                    unimplemented!("Join to {:?}", target);
                                }
                            }
                            _ => {
                                unimplemented!("Join for {:?}", source);
                            }
                        }
                    } else {
                        unimplemented!(
                            "Join for {:?} {:?} {:?} {:?}",
                            source,
                            target,
                            lookup,
                            modifier
                        );
                    }
                }
                _ => {
                    unimplemented!(
                        "Mutation: {:?} {:?} {:?} {:?} {:?}",
                        mutator,
                        source,
                        target,
                        lookup,
                        modifier
                    );
                }
            },
        }
        pc += 1;
    }
    return Ok(Expression::Nothing);
}
