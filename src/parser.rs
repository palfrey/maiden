use crate::common::{
    Block, Command, CommandLine, Expression, MaidenError, Program, Result, SymbolType,
};
use crate::peg::{Rockstar, Rule};
use log::debug;
use pest::iterators::Pair;
use pest::Parser;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
enum Item {
    Expression(Expression),
    Symbol(SymbolType),
    Command(CommandLine),
    Block(Block),
}

impl Item {
    fn expr(self) -> Result<Expression> {
        if let Item::Expression(e) = self {
            Ok(e)
        } else {
            Err(MaidenError::NotAnExpression {
                other: format!("{:?}", self),
                line: 0,
            })
        }
    }
    fn symbol(self) -> Result<SymbolType> {
        if let Item::Symbol(e) = self {
            Ok(e)
        } else {
            panic!("Not a symboltype: {:?}", self)
        }
    }
    fn command(self) -> Result<CommandLine> {
        if let Item::Command(e) = self {
            Ok(e)
        } else {
            panic!("Not a command: {:?}", self)
        }
    }
    fn block(self) -> Result<Block> {
        if let Item::Block(e) = self {
            Ok(e)
        } else {
            panic!("Not a block: {:?}", self)
        }
    }
}

fn pair_line(pair: &Pair<Rule>) -> usize {
    pair.as_span().start_pos().line_col().0
}

fn depair_program<'i, I>(pairs: &'i mut I, content: &'i str) -> Result<Program>
where
    I: Iterator<Item = pest::iterators::Pair<'i, Rule>>,
{
    let pair = pairs.next().expect("one pair");
    match pair.as_rule() {
        Rule::program => {}
        Rule::EOI => {
            return Ok(Program {
                commands: vec![],
                functions: HashMap::new(),
            })
        }
        rule => {
            panic!("Bad rule (program): {:?}", rule);
        }
    }
    let span = pair.as_span();
    if span.start() != 0 {
        panic!("Non-zero start");
    }
    if span.end() != content.len() {
        let text = content[span.end()..].trim();
        if !text.is_empty() {
            panic!("Unparsed text: {}", text);
        }
    }
    let mut commands = vec![];
    for line in pair.into_inner() {
        match line.as_rule() {
            Rule::line | Rule::EOI => {}
            rule => {
                panic!("Bad rule (lines): {:?}", rule);
            }
        }
        let (line_no, _) = line.as_span().start_pos().line_col();
        let depaired = depair(&mut line.into_inner(), 0)?;
        match depaired {
            Item::Command(command) => {
                commands.push(command);
            }
            Item::Symbol(SymbolType::Empty) => {}
            Item::Expression(Expression::Call(name, args)) => {
                commands.push(CommandLine {
                    cmd: Command::Call { name, args },
                    line: line_no,
                });
            }
            item => {
                println!("Something else {:?}", item);
            }
        }
    }
    Ok(Program {
        commands,
        functions: HashMap::new(),
    })
}

impl From<Expression> for Item {
    fn from(exp: Expression) -> Item {
        Item::Expression(exp)
    }
}

impl From<SymbolType> for Item {
    fn from(sym: SymbolType) -> Item {
        Item::Symbol(sym)
    }
}

impl From<CommandLine> for Item {
    fn from(command: CommandLine) -> Item {
        Item::Command(command)
    }
}

impl From<Block> for Item {
    fn from(block: Block) -> Item {
        Item::Block(block)
    }
}

fn pair_to_command_line(pair: &Pair<Rule>, command: Command) -> Item {
    CommandLine {
        cmd: command,
        line: pair_line(pair),
    }
    .into()
}

fn remove<T>(items: &mut Vec<T>, index: usize, line: usize) -> Result<T> {
    if items.len() > index {
        Ok(items.remove(index))
    } else {
        Err(MaidenError::Incomplete { line })
    }
}

// FIXME: Split this up
#[allow(clippy::cognitive_complexity)]
fn depair_core(pair: Pair<'_, Rule>, level: usize) -> Result<Item> {
    let line = pair_line(&pair);
    let rule = pair.as_rule();
    let level_string = format!("({}){}", level, "  ".repeat(level));
    let res = match rule {
        Rule::EOI => SymbolType::Empty.into(),
        Rule::common_variable | Rule::proper_variable | Rule::simple_variable => {
            Expression::Variable(pair.as_span().as_str().to_string()).into()
        }
        Rule::true_kw => Expression::True.into(),
        Rule::false_kw => Expression::False.into(),
        Rule::is_kw | Rule::is => SymbolType::Is.into(),
        Rule::output => {
            let value = depair(&mut pair.into_inner(), level + 1)?.expr()?;
            CommandLine {
                cmd: Command::Say { value },
                line,
            }
            .into()
        }
        Rule::string => {
            let mut value = pair.as_str();
            if value.len() < 2 {
                return Err(MaidenError::BadString {
                    length: value.len(),
                    line,
                });
            }
            value = &value[1..value.len() - 1];
            Expression::String(value.to_string()).into()
        }
        Rule::number => {
            let value = pair.as_str();
            Expression::Floating(value.parse::<f64>().unwrap()).into()
        }
        Rule::conditional => {
            let mut pairs: Vec<_> = pair.into_inner().collect();
            let expression = depair_core(remove(&mut pairs, 0, line)?, level + 1)?.expr()?;
            if pairs.is_empty() {
                return Err(MaidenError::NoEndOfIf { line });
            }
            let first = remove(&mut pairs, 0, line)?;
            let consequent;
            let alternate;
            match first.as_rule() {
                Rule::consequent => {
                    consequent = Some(depair_core(first, level + 1)?.block()?);
                    alternate = if pairs.is_empty() {
                        None
                    } else {
                        let block =
                            depair_core(remove(&mut pairs, 0, line)?, level + 1)?.block()?;
                        if !block.commands.is_empty() {
                            Some(block)
                        } else {
                            None
                        }
                    };
                }
                Rule::alternate => {
                    consequent = None;
                    alternate = {
                        let block = depair_core(first, level + 1)?.block()?;
                        if !block.commands.is_empty() {
                            Some(block)
                        } else {
                            None
                        }
                    }
                }
                rule => {
                    panic!("Bad rule (conditional): {:?}", rule);
                }
            }
            CommandLine {
                cmd: Command::If {
                    expression,
                    then: consequent,
                    otherwise: alternate,
                },
                line,
            }
            .into()
        }
        Rule::block => {
            debug!("{}Depairing Block", level_string);
            let items = depair_seq(&mut pair.into_inner(), level + 1)?;
            let mut commands = vec![];
            for item in items {
                commands.push(item.command()?);
            }
            Block { commands }.into()
        }
        Rule::equality_check => {
            debug!("{}Depairing equality_check", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            if items.len() == 1 {
                return Ok(remove(&mut items, 0, line)?);
            }
            let is = remove(&mut items, 1, line)?;
            let first = Box::new(remove(&mut items, 0, line)?.expr()?);
            let second = Box::new(remove(&mut items, 0, line)?.expr()?);
            match is {
                Item::Symbol(SymbolType::Is) => Expression::Is(first, second),
                Item::Symbol(SymbolType::Aint) => Expression::Aint(first, second),
                _ => panic!("Not is: {:?}", is),
            }
            .into()
        }
        Rule::statement => {
            let item = depair(&mut pair.into_inner(), level + 1)?;
            if let Item::Expression(Expression::Is(target, value)) = item {
                return Ok(CommandLine {
                    cmd: Command::Assignment {
                        target: *target,
                        value: *value,
                    },
                    line,
                }
                .into());
            }
            item
        }
        Rule::not => {
            debug!("{}Depairing not", level_string);
            let mut pairs = pair.into_inner();
            let compare = pairs.peek().unwrap().as_rule() == Rule::comparison;
            let item = depair(&mut pairs, level + 1)?;
            if compare {
                item
            } else {
                Expression::Not(Box::new(item.expr()?)).into()
            }
        }
        Rule::put_assignment => {
            debug!("{}Depairing put_assignment", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            let value = remove(&mut items, 0, line)?.expr()?;
            let target = remove(&mut items, 0, line)?.expr()?;
            CommandLine {
                cmd: Command::Assignment { target, value },
                line,
            }
            .into()
        }
        Rule::assignable => {
            debug!("{}Depairing assignable", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            let variable = remove(&mut items, 0, line)?;
            let indexer = remove(&mut items, 0, line);
            match indexer {
                Ok(index) => Expression::ArrayRef {
                    name: Box::new(variable.expr()?),
                    index: Box::new(index.expr()?),
                }
                .into(),
                Err(_) => variable,
            }
        }
        Rule::join => SymbolType::Join.into(),
        Rule::split => SymbolType::Split.into(),
        Rule::cast => SymbolType::Cast.into(),
        Rule::assignment => {
            debug!("{}Depairing assignment", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            if items.is_empty() {
                panic!("Empty assignment!");
            }
            let target = remove(&mut items, 0, line)?.expr()?;
            match items.len() {
                1 => CommandLine {
                    cmd: Command::Assignment {
                        target,
                        value: remove(&mut items, 0, line)?.expr()?,
                    },
                    line,
                }
                .into(),
                2 => {
                    let operator = remove(&mut items, 0, line)?.symbol()?;
                    let first = target.clone();
                    let mut second = match remove(&mut items, 0, line)? {
                        Item::Expression(expr) => vec![expr],
                        Item::Symbol(SymbolType::ExpressionList(items)) => items,
                        item => {
                            panic!("Something else for assignment: {:?}", item);
                        }
                    };
                    match operator {
                        SymbolType::Add => {
                            let mut expr = first;
                            for s in second.drain(0..) {
                                expr = Expression::Add(Box::new(expr), Box::new(s));
                            }
                            CommandLine {
                                cmd: Command::Assignment {
                                    target,
                                    value: expr,
                                },
                                line,
                            }
                            .into()
                        }
                        SymbolType::Subtract => {
                            let mut expr = first;
                            for s in second.drain(0..) {
                                expr = Expression::Subtract(Box::new(expr), Box::new(s));
                            }
                            CommandLine {
                                cmd: Command::Assignment {
                                    target,
                                    value: expr,
                                },
                                line,
                            }
                            .into()
                        }
                        SymbolType::Times => {
                            let mut expr = first;
                            for s in second.drain(0..) {
                                expr = Expression::Times(Box::new(expr), Box::new(s));
                            }
                            CommandLine {
                                cmd: Command::Assignment {
                                    target,
                                    value: expr,
                                },
                                line,
                            }
                            .into()
                        }
                        SymbolType::Divide => {
                            let mut expr = first;
                            for s in second.drain(0..) {
                                expr = Expression::Divide(Box::new(expr), Box::new(s));
                            }
                            CommandLine {
                                cmd: Command::Assignment {
                                    target,
                                    value: expr,
                                },
                                line,
                            }
                            .into()
                        }
                        SymbolType::Is => {
                            let s = remove(&mut second, 0, line)?;
                            CommandLine {
                                cmd: Command::Assignment { target, value: s },
                                line,
                            }
                            .into()
                        }
                        _ => {
                            panic!("Bad assignment operator: {:?}", operator);
                        }
                    }
                }
                _ => {
                    panic!("Bad assignment: {:?}", items);
                }
            }
        }
        Rule::arithmetic | Rule::product => {
            debug!("{}Depairing arithmetic", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            if items.len() == 1 {
                return remove(&mut items, 0, line);
            }
            if items.len() % 2 != 1 {
                panic!("Weird arithmetic: {:?}", items);
            };
            let mut first = remove(&mut items, 0, line)?.expr()?;
            while !items.is_empty() {
                let operator = remove(&mut items, 0, line)?.symbol()?;
                let apply_operator = move |first, other| match operator {
                    SymbolType::Add => Expression::Add(Box::new(first), Box::new(other)),
                    SymbolType::Subtract => Expression::Subtract(Box::new(first), Box::new(other)),
                    SymbolType::Times => Expression::Times(Box::new(first), Box::new(other)),
                    SymbolType::Divide => Expression::Divide(Box::new(first), Box::new(other)),
                    _ => {
                        panic!("Unknown operator: {:?}", operator);
                    }
                };
                match remove(&mut items, 0, line)? {
                    Item::Expression(second) => {
                        first = apply_operator(first, second);
                    }
                    Item::Symbol(SymbolType::ExpressionList(mut multiple)) => {
                        if !multiple.is_empty() {
                            for second in multiple.drain(0..) {
                                first = apply_operator(first, second);
                            }
                        }
                    }
                    item => {
                        panic!("Other item for arithmetic: {:?}", item);
                    }
                };
            }
            first.into()
        }
        Rule::and => {
            debug!("{}Depairing and", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            if items.len() == 1 {
                return Ok(remove(&mut items, 0, line)?);
            }
            Expression::And(
                Box::new(remove(&mut items, 0, line)?.expr()?),
                Box::new(remove(&mut items, 0, line)?.expr()?),
            )
            .into()
        }
        Rule::or => {
            debug!("{}Depairing or", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            if items.len() == 1 {
                return Ok(remove(&mut items, 0, line)?);
            }
            Expression::Or(
                Box::new(remove(&mut items, 0, line)?.expr()?),
                Box::new(remove(&mut items, 0, line)?.expr()?),
            )
            .into()
        }
        Rule::nor => {
            debug!("{}Depairing nor", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            if items.len() == 1 {
                return Ok(remove(&mut items, 0, line)?);
            }
            Expression::Nor(
                Box::new(remove(&mut items, 0, line)?.expr()?),
                Box::new(remove(&mut items, 0, line)?.expr()?),
            )
            .into()
        }
        Rule::add => SymbolType::Add.into(),
        Rule::subtract => SymbolType::Subtract.into(),
        Rule::multiply => SymbolType::Times.into(),
        Rule::divide => SymbolType::Divide.into(),
        Rule::pronoun => Expression::Pronoun.into(),
        Rule::ne => SymbolType::Aint.into(),
        Rule::return_kw => SymbolType::Return.into(),
        Rule::function_return => {
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            CommandLine {
                cmd: Command::Return {
                    return_value: remove(&mut items, 1, line)?.expr()?,
                },
                line,
            }
            .into()
        }
        Rule::poetic_number => {
            let value = pair.as_str();
            let mut number: f64 = 0.0;
            let mut decimal = false;
            let mut decimal_places = 0;
            for raw_word in value.split_whitespace() {
                let mut word = raw_word.to_string();
                word.retain(|c| c.is_alphabetic());
                if word.is_empty() {
                    continue;
                }
                if number > 0.0 {
                    number *= 10.0;
                }
                number += (word.len() % 10) as f64;
                if decimal {
                    decimal_places += 1;
                }
                if raw_word.ends_with('.') {
                    decimal = true;
                }
            }
            if decimal_places > 0 {
                number /= 10.0_f64.powf(decimal_places as f64);
            }
            debug!("number '{}' parsed as {}", value, number);
            Expression::Floating(number).into()
        }
        Rule::poetic_string => Expression::String(pair.as_str().to_string()).into(),
        Rule::null => Expression::Null.into(),
        Rule::mysterious => Expression::Mysterious.into(),
        Rule::readline => {
            debug!("{}Depairing listen", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            if items.is_empty() {
                return Ok(CommandLine {
                    cmd: Command::Listen { target: None },
                    line,
                }
                .into());
            }
            if items.len() != 1 {
                panic!("listen: {:?}", items);
            }
            if let Item::Expression(Expression::Variable(name)) = remove(&mut items, 0, line)? {
                CommandLine {
                    cmd: Command::Listen { target: Some(name) },
                    line,
                }
                .into()
            } else {
                panic!("listen: {:?}", items);
            }
        }
        Rule::variable_list => {
            debug!("{}Depairing variable_list", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            let mut variables = vec![];
            if !items.is_empty() {
                for item in items.drain(0..) {
                    match item {
                        Item::Symbol(SymbolType::Empty) => {}
                        Item::Expression(Expression::Variable(name)) => {
                            variables.push(name);
                        }
                        Item::Symbol(SymbolType::VariableList(vars)) => {
                            variables.extend(vars);
                        }
                        _ => {
                            panic!("Non-variable in variable list: {:?}", item);
                        }
                    }
                }
            }
            SymbolType::VariableList(variables).into()
        }
        Rule::args_list => {
            debug!("{}Depairing args_list", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            let mut expressions = vec![];
            if !items.is_empty() {
                for item in items.drain(0..) {
                    match item {
                        Item::Symbol(SymbolType::Empty) => {}
                        Item::Expression(expr) => {
                            expressions.push(expr);
                        }
                        Item::Symbol(SymbolType::ArgsList(exprs))
                        | Item::Symbol(SymbolType::ExpressionList(exprs)) => {
                            expressions.extend(exprs);
                        }
                        _ => {
                            panic!("Non-expression in expr list: {:?}", item);
                        }
                    }
                }
            }
            SymbolType::ArgsList(expressions).into()
        }
        Rule::expression_list => {
            debug!("{}Depairing expression_list", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            let mut expressions = vec![];
            if items.len() == 1 {
                return Ok(remove(&mut items, 0, line)?);
            }
            if !items.is_empty() {
                for item in items.drain(0..) {
                    match item {
                        Item::Symbol(SymbolType::Empty) => {}
                        Item::Expression(expr) => {
                            expressions.push(expr);
                        }
                        Item::Symbol(SymbolType::ExpressionList(exprs)) => {
                            expressions.extend(exprs);
                        }
                        _ => {
                            panic!("Non-expression in expr list: {:?}", item);
                        }
                    }
                }
            }
            SymbolType::ExpressionList(expressions).into()
        }
        Rule::function => {
            debug!("{}Depairing function", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            let name = if let Expression::Variable(n) = remove(&mut items, 0, line)?.expr()? {
                n
            } else {
                panic!("Non-variable name for function");
            };
            let args = if let SymbolType::VariableList(variables) =
                remove(&mut items, 0, line)?.symbol()?
            {
                variables
            } else {
                panic!("Non-variable list for function");
            };
            let block = remove(&mut items, 0, line)?.block()?;
            CommandLine {
                cmd: Command::FunctionDeclaration { name, args, block },
                line,
            }
            .into()
        }
        Rule::function_call => {
            debug!("{}Depairing function_call", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            let name = if let Expression::Variable(n) = remove(&mut items, 0, line)?.expr()? {
                n
            } else {
                panic!("Non-variable name for function_call");
            };
            let args_list = remove(&mut items, 0, line)?.symbol()?;
            if let SymbolType::ArgsList(variables) = args_list {
                Expression::Call(name, variables).into()
            } else {
                panic!("Non-args list: {:?}", args_list);
            }
        }
        Rule::up_kw => SymbolType::Up.into(),
        Rule::down_kw => SymbolType::Down.into(),
        Rule::increment => {
            debug!("{}Depairing increment", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            let target = remove(&mut items, 0, line)?.expr()?;
            CommandLine {
                cmd: Command::Increment {
                    target,
                    count: items.len() as f64,
                },
                line,
            }
            .into()
        }
        Rule::decrement => {
            debug!("{}Depairing decrement", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            let target = remove(&mut items, 0, line)?.expr()?;
            CommandLine {
                cmd: Command::Decrement {
                    target,
                    count: items.len() as f64,
                },
                line,
            }
            .into()
        }
        Rule::variable_list_separator | Rule::expression_list_separator => SymbolType::Empty.into(),
        Rule::greater => SymbolType::GreaterThan.into(),
        Rule::great => SymbolType::GreaterThanOrEqual.into(),
        Rule::smaller => SymbolType::LessThan.into(),
        Rule::small => SymbolType::LessThanOrEqual.into(),
        Rule::comparison => {
            debug!("{}Depairing comparison", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            if items.len() == 1 {
                return Ok(remove(&mut items, 0, line)?);
            }
            if items.len() != 3 {
                panic!("Bad comparison: {:?}", items);
            }
            let first = Box::new(remove(&mut items, 0, line)?.expr()?);
            let operator = remove(&mut items, 0, line)?.symbol()?;
            let second = Box::new(remove(&mut items, 0, line)?.expr()?);
            match operator {
                SymbolType::GreaterThan => Expression::GreaterThan(first, second),
                SymbolType::GreaterThanOrEqual => Expression::GreaterThanOrEqual(first, second),
                SymbolType::LessThan => Expression::LessThan(first, second),
                SymbolType::LessThanOrEqual => Expression::LessThanOrEqual(first, second),
                _ => {
                    panic!("Unknown operator: {:?}", operator);
                }
            }
            .into()
        }
        Rule::while_kw => SymbolType::While.into(),
        Rule::until_kw => SymbolType::Until.into(),
        Rule::continue_kw => pair_to_command_line(&pair, Command::Continue),
        Rule::break_kw => pair_to_command_line(&pair, Command::Break),
        Rule::loop_kw => {
            debug!("{}Depairing loop", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            let kind = remove(&mut items, 0, line)?.symbol()?;
            let condition = remove(&mut items, 0, line)?.expr()?;
            let block = remove(&mut items, 0, line)?.block()?;
            CommandLine {
                cmd: match kind {
                    SymbolType::While => Command::While {
                        expression: condition,
                        block,
                    },
                    SymbolType::Until => Command::Until {
                        expression: condition,
                        block,
                    },
                    _ => {
                        panic!("Unrecognised block type: {:?}", kind);
                    }
                },
                line,
            }
            .into()
        }
        Rule::math_round => {
            let pair = depair(&mut pair.into_inner(), level + 1)?;
            CommandLine {
                cmd: Command::Round {
                    target: pair.expr()?,
                },
                line,
            }
            .into()
        }
        Rule::floor => {
            let pair = depair(&mut pair.into_inner(), level + 1)?;
            CommandLine {
                cmd: Command::Floor {
                    target: pair.expr()?,
                },
                line,
            }
            .into()
        }
        Rule::ceil => {
            let pair = depair(&mut pair.into_inner(), level + 1)?;
            CommandLine {
                cmd: Command::Ceil {
                    target: pair.expr()?,
                },
                line,
            }
            .into()
        }
        Rule::modifier => {
            debug!("{}Depairing modifier", level_string);
            Expression::Modifier(Box::new(depair(&mut pair.into_inner(), level + 1)?.expr()?))
                .into()
        }
        Rule::mutation => {
            debug!("{}Depairing mutation", level_string);
            let mut items = depair_seq(&mut pair.into_inner(), level + 1)?;
            let mutator = remove(&mut items, 0, line)?.symbol()?;
            let mut count = items.len();
            let modifier =
                if let Some(Item::Expression(Expression::Modifier(changer))) = items.last() {
                    count -= 1;
                    Some(changer)
                } else {
                    None
                };
            panic!(
                "Mutation ({:?}): {:?} {:?} {}",
                mutator, modifier, items, count
            );
        }
        rule => {
            let original = pair.clone();
            let inner = pair.into_inner();
            let count = inner.count();
            if count == 0 {
                if rule == Rule::alternate {
                    return Ok(Block { commands: vec![] }.into());
                }
                let (line_no, col_no) = original.as_span().start_pos().line_col();
                panic!(
                    "{}Empty pair at {}, {}: {:?}",
                    level_string, line_no, col_no, original
                );
            } else if count == 1 {
                debug!("{}Depairing {:?}", level_string, rule);
                depair(&mut original.into_inner(), level + 1)?
            } else {
                debug!("{}List rule: {:?}", level_string, rule);
                panic!(
                    "Unbuilt list rule '{:?}': {:?}",
                    rule,
                    depair_seq(&mut original.into_inner(), level + 1)?
                );
            }
        }
    };
    Ok(res)
}

fn depair<'i, I>(pairs: &'i mut I, level: usize) -> Result<Item>
where
    I: Iterator<Item = pest::iterators::Pair<'i, Rule>>,
{
    let mut items = vec![];
    let level_string = format!("({}){}", level, "  ".repeat(level));
    let mut line = 0;
    for pair in pairs {
        line = pair_line(&pair);
        let item = depair_core(pair, level)?;
        if item == SymbolType::Empty.into() {
            continue;
        }
        items.push(item);
    }
    match items.len() {
        0 => {
            debug!("{}Empty", level_string);
            Ok(SymbolType::Empty.into())
        }
        1 => Ok(remove(&mut items, 0, line)?),
        _ => {
            panic!("{}Many! {:?}", level_string, items);
        }
    }
}

fn depair_seq<'i, I>(pairs: &'i mut I, level: usize) -> Result<Vec<Item>>
where
    I: Iterator<Item = pest::iterators::Pair<'i, Rule>>,
{
    let mut items = vec![];
    for pair in pairs {
        items.push(depair_core(pair, level)?);
    }
    return Ok(items);
}

pub fn parse(buffer: &str) -> Result<Program> {
    let mut parsed =
        Rockstar::parse(Rule::program, &buffer).map_err(|e| MaidenError::Pest { kind: e })?;
    return depair_program(&mut parsed, &buffer);
}

#[cfg(test)]
mod tests {
    use super::{parse, MaidenError};

    #[test]
    fn end_of_if() {
        let err = parse("if 1 is 2");
        if let Err(MaidenError::NoEndOfIf { line }) = err {
            assert_eq!(line, 1);
        } else {
            assert!(false, format!("{:?}", err));
        }
    }
}
