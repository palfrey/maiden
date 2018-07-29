use common::*;
use std::io::Write;
use std::collections::HashMap;
use std::ops::Deref;

struct State<'a> {
    writer: &'a mut Write,
    variables: &'a mut HashMap<String, Expression>
}

fn run_binop(state: &mut State, program: &Program, first: &Expression, second: &Expression, f: fn(&Expression, &Expression) -> bool) -> Result<Expression> {
    let res_first = run_expression(state, program, first.deref())?;
    let res_second = run_expression(state, program, second.deref())?;
    debug!("first: {:?} second: {:?}", res_first, res_second);
    if f(&res_first, &res_second) {
        Ok(Expression::True)
    } else {
        Ok(Expression::False)
    }
}

fn run_mathbinop(state: &mut State, program: &Program, first: &Expression, second: &Expression, f: fn(i32, i32) -> i32) -> Result<Expression> {
    let res_first = run_expression(state, program, first.deref())?;
    let res_second = run_expression(state, program, second.deref())?;
    debug!("first: {:?} second: {:?}", res_first, res_second);
    // one if-let per if apparently, until https://github.com/rust-lang/rfcs/issues/929 gets resolved
    if let Expression::Integer(f_i) = res_first {
        if let Expression::Integer(s_i) = res_second {
            return Ok(Expression::Integer(f(f_i, s_i)));
        }
    }
    unimplemented!("Math op between non integers: {:?} {:?}", res_first, res_second);
}

fn to_boolean(expression: &Expression) -> bool {
    if let &Expression::False = expression {
        false
    }
    else if let &Expression::True = expression {
        true
    }
    else {
        panic!("Bad boolean resolve: {:?}", expression);
    }
}

fn run_expression(state: &mut State, program: &Program, expression: &Expression) -> Result<Expression> {
    debug!("Expression: {:?}", expression);
    return match expression {
        &Expression::Is(ref first, ref second) => {
            return run_binop(state, program, first, second, |f, s| {f == s});
        }
        &Expression::And(ref first, ref second) => {
            return run_binop(state, program, first, second, |f, s| {
                return to_boolean(f) && to_boolean(s);
            });
        }
        &Expression::GreaterThanOrEqual(ref first, ref second) => {
            return run_binop(state, program, first, second, |f, s| {f >= s});
        }
        &Expression::Subtract(ref first, ref second) => {
            return run_mathbinop(state, program, first, second, |f, s| { f - s });
        }
        &Expression::String(_) | &Expression::Integer(_) => {
            Ok(expression.clone())
        }
        &Expression::Variable(ref name) => {
            match state.variables.get(&name.to_lowercase()) {
                Some(exp) => Ok(exp.clone()),
                None => {
                    bail!(ErrorKind::MissingVariable(name.clone()));
                }
            }
        }
        &Expression::Call(ref target, ref args) => {
            let func_wrap = program.functions.get(target);
            if func_wrap.is_none() {
                bail!(ErrorKind::MissingFunction(target.clone()));
            }
            let func = func_wrap.unwrap();
            if args.len() != func.args.len() {
                bail!(ErrorKind::WrongArgCount(func.args.len(), args.len()))
            }
            for i in 0..args.len() {
                let value = run_expression(state, program, &args[0])?;
                state.variables.insert(func.args[i].to_lowercase(), value);
            }
            run_core(state, program, func.location+1)
        }
        _ => {
            unimplemented!("No runner for {:?}", expression);
        }
    }
}

pub fn run(program: Program, writer: &mut Write) -> Result<HashMap<String, Expression>> {
    let pc = 0;
    let mut variables: HashMap<String, Expression> = HashMap::new();
    {
        let mut state = State {
            variables: &mut variables,
            writer: writer
        };
        run_core(&mut state, &program, pc)?;
    } // FIXME: Drop once NLL is merged
    return Ok(variables);
}

fn run_core(state: &mut State, program: &Program, mut pc: usize) -> Result<(Expression)> {
    let mut total_instr = 0;
    loop {
        total_instr +=1;
        if total_instr > 1000 {
            panic!("Ran out of instr");
        }
        let command = match program.commands.get(pc) {
            Some(c) => c,
            None => break
        };
        debug!("command: {:?}", command);
        match command {
            Command::Assignment {target, value} => {
                let val = run_expression(state, program, value)?;
                state.variables.insert(target.to_lowercase(), val);
            }
            Command::Increment {target} => {
                let val = state.variables.get(&target.to_lowercase()).expect(&format!("Can't find '{}'", target)).clone();
                info!("Value of {} is {:?}", target, val);
                match val {
                    Expression::Integer(x) => {
                        state.variables.insert(target.to_lowercase(), Expression::Integer(x+1));
                    }
                    _ => {
                        error!("Attempt to increment non-integer '{}'", target);
                    }
                };
            }
            Command::Until {expression, loop_end} => {
                let resolve = run_expression(state, program, expression)?;
                if to_boolean(&resolve) {
                    pc = loop_end.expect("loop_end");
                }
            }
            Command::While {expression, loop_end} => {
                let resolve = run_expression(state, program, expression)?;
                if !to_boolean(&resolve) {
                    pc = loop_end.expect("loop_end");
                }
            }
            Command::Next {loop_start} => {
                pc = loop_start-1;
            }
            Command::Say {value} => {
                match value {
                    Expression::Integer(x) => writeln!(state.writer, "{}", x)?,
                    Expression::String(s) => writeln!(state.writer, "{}", s)?,
                    _ => {
                        warn!("{:?}", value);
                        unimplemented!();
                    }
                };
            }
            Command::FunctionDeclaration {name, args, func_end} => {
                pc = func_end.expect("func_end");
            }
            Command::EndFunction{return_value} => {
                return run_expression(state, program, return_value);
            }
            Command::If {expression, if_end} => {
                let resolve = run_expression(state, program, expression)?;
                if !to_boolean(&resolve) {
                    pc = if_end.expect("if_end");
                }
            }
            _ => {
                unimplemented!("{:?}", command);
            }
        }
        pc +=1;
    }
    return Ok(Expression::Nothing);
}