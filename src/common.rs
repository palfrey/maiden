use nom::types::CompleteStr;
use nom_locate::LocatedSpan;
use std::collections::HashMap;
pub type Span<'a> = LocatedSpan<CompleteStr<'a>>;

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub enum Expression {
    // Single items
    Integer(i128),
    String(String),
    Variable(String),
    True,
    False,
    Call(String, Vec<Expression>),
    Nothing,

    // binary operators
    Is(Box<Expression>, Box<Expression>),
    Aint(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Times(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    GreaterThanOrEqual(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone)]
pub enum SymbolType {
    Dummy,
    And,
    Is,
    Build,
    Up,
    Knock,
    Down,
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
    GreaterThanOrEqual,
    GreaterThan,
    LessThan,
    Add,
    Subtract,
    Times,
    Put,
    Where,
    Newline,
    Aint,
    Else,
    Variable(String),
    String(String),
    Words(Vec<String>),
    Integer(String),
    Comment,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub line: u32,
    pub symbol: SymbolType,
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
    EndIf,
    Increment { target: String },
    Decrement { target: String },
    Next { loop_start: usize },
    Continue { loop_start: usize },
    Say { value: Expression },
    FunctionDeclaration {
        name: String,
        args: Vec<String>,
        func_end: Option<usize>,
    },
    Return { return_value: Expression },
    EndFunction,
    Call { name: String, args: Vec<Expression> },
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub location: usize,
    pub args: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub struct CommandLine {
    pub cmd: Command,
    pub line: u32,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub commands: Vec<CommandLine>,
    pub functions: HashMap<String, Function>,
}

error_chain!{
    foreign_links {
        Io(::std::io::Error);
    }
    errors {
        Nom(kind: ::nom::ErrorKind) {
            description("parsing error")
            display("parsing error: {:?}", kind)
        }
        UnparsedText(t: String, line: u32) {
            display("Unparsed text '{}'", t)
        }
        MissingVariable(name: String, line: u32) {
            display("Missing variable '{}'", name)
        }
        MissingFunction(name: String, line: u32) {
            display("Missing function '{}'", name)
        }
        WrongArgCount(expected: usize, got: usize, line: u32) {
            display("Wrong argument count to function (expected {}, got {})", expected, got)
        }
        UnbalancedExpression(expression: String, line: u32) {
            display("Unbalanced expression {}", expression)
        }
        BadBooleanResolve(expression: String, line: u32) {
            display("Bad boolean resolve: {:?}", expression)
        }
        NoRunner(expression: String, line: u32) {
            display("Don't know how to execute the expression '{}'", expression)
        }
        BadCommandSequence(sequence: Vec<SymbolType>, line: u32) {
            display("Don't recognise command sequence {:?}", sequence)
        }
        ParseIntError(number: String, line: u32) {
            display("Unparsable integer: '{}'", number)
        }
        NoSymbols(line: u32) {
            display("No symbols!")
        }
        BadIs(sequence: Vec<SymbolType>, line: u32) {
            display("Bad 'is' section: {:?}", sequence)
        }
        BadPut(sequence: Vec<SymbolType>, line: u32) {
            display("Bad 'put' section: {:?}", sequence)
        }
        BadFunctionDeclaration(sequence: Vec<SymbolType>, line: u32) {
            display("Bad 'function declaration' section: {:?}", sequence)
        }
        NoEndOfIf(line: u32) {
            display("No end of if statement")
        }
        NoEndFunction(line: u32) {
            display("No end of function")
        }
        NoEndLoop(line: u32) {
            display("No end of loop")
        }
    }
}

impl<'a> From<::nom::Err<Span<'a>>> for Error {
    fn from(err: ::nom::Err<Span<'a>>) -> Error {
        let kind = err.into_error_kind();
        Error::from_kind(ErrorKind::Nom(kind))
    }
}

#[cfg(target_arch = "wasm32")]
pub fn get_error_line(e: &Error) -> u32 {
    match e {
        Error(kind, _) => {
            match kind {
                ErrorKind::MissingVariable(_, line) => line.clone(),
                ErrorKind::UnparsedText(_, line) => line.clone(),
                ErrorKind::MissingFunction(_, line) => line.clone(),
                ErrorKind::WrongArgCount(_, _, line) => line.clone(),
                ErrorKind::UnbalancedExpression(_, line) => line.clone(),
                ErrorKind::NoRunner(_, line) => line.clone(),
                ErrorKind::BadCommandSequence(_, line) => line.clone(),
                ErrorKind::ParseIntError(_, line) => line.clone(),
                ErrorKind::BadIs(_, line) => line.clone(),
                ErrorKind::BadPut(_, line) => line.clone(),
                ErrorKind::NoEndOfIf(line) => line.clone(),
                ErrorKind::NoEndFunction(line) => line.clone(),
                ErrorKind::NoEndLoop(line) => line.clone(),
                ErrorKind::BadBooleanResolve(_, line) => line.clone(),
                ErrorKind::BadFunctionDeclaration(_, line) => line.clone(),
                _ => 0,
            }
        }
    }
}
