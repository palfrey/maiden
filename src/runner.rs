use common::*;
use std::io::Write;
use std::collections::HashMap;
use std::ops::Deref;

struct State<'a> {
    writer: &'a mut Write,
    variables: &'a mut HashMap<String, Expression>,
}

fn run_binop(
    state: &mut State,
    program: &Program,
    first: &Expression,
    second: &Expression,
    f: fn(&Expression, &Expression) -> bool,
) -> Result<Expression> {
    let res_first = run_expression(state, program, first.deref())?;
    let res_second = run_expression(state, program, second.deref())?;
    debug!("first: {:?} second: {:?}", res_first, res_second);
    if f(&res_first, &res_second) {
        Ok(Expression::True)
    } else {
        Ok(Expression::False)
    }
}

fn run_mathbinop(
    state: &mut State,
    program: &Program,
    first: &Expression,
    second: &Expression,
    f: fn(i128, i128) -> i128,
) -> Result<Expression> {
    let res_first = run_expression(state, program, first.deref())?;
    let res_second = run_expression(state, program, second.deref())?;
    debug!("first: {:?} second: {:?}", res_first, res_second);
    // one if-let per if apparently, until https://github.com/rust-lang/rfcs/issues/929 gets resolved
    if let Expression::Integer(f_i) = res_first {
        if let Expression::Integer(s_i) = res_second {
            return Ok(Expression::Integer(f(f_i, s_i)));
        }
    }
    unimplemented!(
        "Math op between non integers: {:?} {:?}",
        res_first,
        res_second
    );
}

fn to_boolean(expression: &Expression) -> bool {
    if let &Expression::False = expression {
        false
    } else if let &Expression::True = expression {
        true
    } else {
        panic!("Bad boolean resolve: {:?}", expression);
    }
}

fn call_function(state: &mut State, program: &Program, target: &str, args: &Vec<Expression>) -> Result<Expression> {
    let func_wrap = program.functions.get(target);
    if func_wrap.is_none() {
        bail!(ErrorKind::MissingFunction(target.to_string()));
    }
    let func = func_wrap.unwrap();
    if args.len() != func.args.len() {
        bail!(ErrorKind::WrongArgCount(func.args.len(), args.len()))
    }

    let mut new_variables = state.variables.clone();
    let mut new_state = State {
        writer: state.writer,
        variables: &mut new_variables,
    };
    for i in 0..args.len() {
        let value = run_expression(&mut new_state, program, &args[i])?;
        new_state.variables.insert(
            func.args[i].to_lowercase(),
            value,
        );
    }
    let ret = run_core(&mut new_state, program, func.location + 1);
    ret
}

fn run_expression(state: &mut State, program: &Program, expression: &Expression) -> Result<Expression> {
    debug!("Expression: {:?}", expression);
    return match expression {
        &Expression::Is(ref first, ref second) => {
            return run_binop(state, program, first, second, |f, s| f == s);
        }
        &Expression::Aint(ref first, ref second) => {
            return run_binop(state, program, first, second, |f, s| f != s);
        }
        &Expression::And(ref first, ref second) => {
            return run_binop(state, program, first, second, |f, s| {
                return to_boolean(f) && to_boolean(s);
            });
        }
        &Expression::GreaterThanOrEqual(ref first, ref second) => {
            return run_binop(state, program, first, second, |f, s| f >= s);
        }
        &Expression::GreaterThan(ref first, ref second) => {
            return run_binop(state, program, first, second, |f, s| f > s);
        }
        &Expression::LessThan(ref first, ref second) => {
            return run_binop(state, program, first, second, |f, s| f < s);
        }
        &Expression::Subtract(ref first, ref second) => {
            return run_mathbinop(state, program, first, second, |f, s| f - s);
        }
        &Expression::Add(ref first, ref second) => {
            return run_mathbinop(state, program, first, second, |f, s| f + s);
        }
        &Expression::Times(ref first, ref second) => {
            return run_mathbinop(state, program, first, second, |f, s| f * s);
        }
        &Expression::String(_) |
        &Expression::Integer(_) => Ok(expression.clone()),
        &Expression::Variable(ref name) => {
            match state.variables.get(&name.to_lowercase()) {
                Some(exp) => {
                    debug!("Got variable {} with value {:?}", &name, exp);
                    Ok(exp.clone())
                }
                None => {
                    bail!(ErrorKind::MissingVariable(name.clone()));
                }
            }
        }
        &Expression::Call(ref target, ref args) => call_function(state, program, target, args),
        _ => {
            unimplemented!("No runner for {:?}", expression);
        }
    };
}

pub fn run(program: Program, writer: &mut Write) -> Result<HashMap<String, Expression>> {
    let pc = 0;
    let mut variables: HashMap<String, Expression> = HashMap::new();
    {
        let mut state = State {
            variables: &mut variables,
            writer: writer,
        };
        run_core(&mut state, &program, pc)?;
    } // FIXME: Drop once NLL is merged
    return Ok(variables);
}

fn get_printable(value: &Expression, state: &mut State) -> Result<String> {
    match value {
        Expression::Integer(x) => Ok(format!("{}", x)),
        Expression::String(s) => Ok(s.to_string()),
        Expression::Variable(x) => {
            let v = state
                .variables
                .get(&x.to_lowercase())
                .expect(&format!("Can't find '{}'", x))
                .clone();
            get_printable(&v, state)
        }
        _ => {
            unimplemented!("Say '{:?}'", value);
        }
    }
}

fn alter_variable(state: &mut State, target: &str, f: &Fn(i128) -> i128) {
    let val = state
        .variables
        .get(&target.to_lowercase())
        .expect(&format!("Can't find '{}'", target))
        .clone();
    debug!("Value of {} is {:?}", target, val);
    match val {
        Expression::Integer(x) => {
            state.variables.insert(
                target.to_lowercase(),
                Expression::Integer(f(x)),
            );
        }
        _ => {
            error!("Attempt to alter non-integer '{}'", target);
        }
    };
}

fn run_core(state: &mut State, program: &Program, mut pc: usize) -> Result<(Expression)> {
    let mut total_instr = 0;
    loop {
        total_instr += 1;
        if total_instr > 100000 {
            panic!("Ran out of instr");
        }
        let command = match program.commands.get(pc) {
            Some(c) => c,
            None => break,
        };
        debug!("command: {:?}", command);
        match command {
            Command::Assignment { target, value } => {
                let val = run_expression(state, program, value)?;
                state.variables.insert(target.to_lowercase(), val);
            }
            Command::Increment { target } => {
                alter_variable(state, target, &|x| x + 1);
            }
            Command::Decrement { target } => {
                alter_variable(state, target, &|x| x - 1);
            }
            Command::Until {
                expression,
                loop_end,
            } => {
                let resolve = run_expression(state, program, expression)?;
                if to_boolean(&resolve) {
                    pc = loop_end.expect("loop_end");
                }
            }
            Command::While {
                expression,
                loop_end,
            } => {
                let resolve = run_expression(state, program, expression)?;
                if !to_boolean(&resolve) {
                    pc = loop_end.expect("loop_end");
                }
            }
            Command::Next { loop_start } => {
                pc = loop_start - 1;
            }
            Command::Say { value } => {
                let resolve = run_expression(state, program, value)?;
                match get_printable(&resolve, state) {
                    Ok(x) => writeln!(state.writer, "{}", x)?,
                    Err(_) => {
                        unimplemented!("Say '{:?}'", value);
                    }
                };
            }
            Command::FunctionDeclaration {
                name: _,
                args: _,
                func_end,
            } => {
                pc = func_end.expect("func_end");
            }
            Command::Return { return_value } => {
                return run_expression(state, program, return_value);
            }
            Command::EndFunction => {
                return run_expression(state, program, &Expression::Nothing);
            }
            Command::If { expression, if_end } => {
                let resolve = run_expression(state, program, expression)?;
                debug!("if: {:?} {:?}", &resolve, expression);
                if !to_boolean(&resolve) {
                    pc = if_end.expect("if_end");
                }
            }
            Command::Call { name, args } => {
                call_function(state, program, name, args)?;
            }
            Command::EndIf => {}
        }
        pc += 1;
    }
    return Ok(Expression::Nothing);
}
