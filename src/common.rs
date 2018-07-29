use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub enum Expression {
    // Single items
    Integer(i32),
    String(String),
    Variable(String),
    True,
    False,
    Call(String, Vec<Expression>),
    Nothing,

    // binary operators
    Is(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    GreaterThanOrEqual(Box<Expression>, Box<Expression>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd)]
pub enum SymbolType {
    Dummy,
    And,
    Is,
    Build,
    Up,
    Until,
    While,
    Next,
    Continue,
    Return,
    Say,
    If,
    Taking { target: String, args: Vec<String> },
    Takes,
    Comma,
    Subtract,
    Put,
    Where,
    Newline,
    GreaterThanOrEqual,
    Variable(String),
    String(String),
    Words(Vec<String>),
    Integer(u32),
    Comment,
}

pub static LOWEST_PRECDENCE: SymbolType = SymbolType::Dummy;

#[derive(Debug, PartialEq)]
pub enum Command {
    Assignment { target: String, value: Expression },
    Until {
        expression: Expression,
        loop_end: Option<usize>,
    },
    While {
        expression: Expression,
        loop_end: Option<usize>,
    },
    If {
        expression: Expression,
        if_end: Option<usize>,
    },
    Increment { target: String },
    Next { loop_start: usize },
    Say { value: Expression },
    FunctionDeclaration {
        name: String,
        args: Vec<String>,
        func_end: Option<usize>,
    },
    EndFunction { return_value: Expression },
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub location: usize,
    pub args: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub commands: Vec<Command>,
    pub functions: HashMap<String, Function>,
}

error_chain!{
    foreign_links {
        Io(::std::io::Error);
    }
    errors {
        UnparsedText(t: String) {
            description("extra text we couldn't parse")
            display("extra unparsed text: '{}'", t)
        }
        MissingVariable(name: String)
        MissingFunction(name: String)
        WrongArgCount(expected: usize, got: usize)
    }
}
