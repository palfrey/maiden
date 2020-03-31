use failure::Fail;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap};

use crate::peg;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    // Single items
    String(String),
    Floating(f64),
    Variable(String),
    Object(String), // currently just functions
    ArrayRef {
        name: Box<Expression>,
        index: Box<Expression>,
    },
    Array {
        numeric: BTreeMap<usize, Box<Expression>>,
        strings: BTreeMap<String, Box<Expression>>,
    },
    Modifier(Box<Expression>),
    True,
    False,
    Call(String, Vec<Expression>),
    Nothing,
    Null,
    Mysterious,
    Pronoun,
    Not(Box<Expression>),

    // needed by loops
    Break,
    Continue,

    // binary operators
    Is(Box<Expression>, Box<Expression>),
    Aint(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Times(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Nor(Box<Expression>, Box<Expression>),
    GreaterThanOrEqual(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    LessThanOrEqual(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
}

impl PartialOrd for Expression {
    fn partial_cmp(&self, other: &Expression) -> Option<Ordering> {
        match self {
            Expression::Floating(s) => {
                if let Expression::Floating(o) = other {
                    s.partial_cmp(o)
                } else {
                    None
                }
            }
            Expression::String(s) => {
                if let Expression::String(o) = other {
                    s.partial_cmp(o)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolType {
    Is,
    Up,
    Down,
    Until,
    While,
    Return,
    GreaterThanOrEqual,
    GreaterThan,
    LessThan,
    LessThanOrEqual,
    Add,
    Subtract,
    Times,
    Aint,
    Divide,
    Empty,
    Join,
    Cast,
    Split,
    VariableList(Vec<String>),
    ArgsList(Vec<Expression>),
    ExpressionList(Vec<Expression>),
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub line: usize,
    pub symbol: SymbolType,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub commands: Vec<CommandLine>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Command {
    Assignment {
        target: Box<Expression>,
        value: Box<Expression>,
    },
    Until {
        expression: Expression,
        block: Block,
    },
    While {
        expression: Expression,
        block: Block,
    },
    If {
        expression: Expression,
        then: Option<Block>,
        otherwise: Option<Block>,
    },
    Increment {
        target: Expression,
        count: f64,
    },
    Decrement {
        target: Expression,
        count: f64,
    },
    Continue,
    Break,
    Say {
        value: Expression,
    },
    Listen {
        target: Option<String>,
    },
    FunctionDeclaration {
        name: String,
        args: Vec<String>,
        block: Block,
    },
    Return {
        return_value: Expression,
    },
    Call {
        name: String,
        args: Vec<Expression>,
    },
    Floor {
        target: Expression,
    },
    Ceil {
        target: Expression,
    },
    Round {
        target: Expression,
    },
    Mutation {
        mutator: SymbolType,
        source: Option<Box<Expression>>,
        target: Option<Box<Expression>>,
        lookup: Option<Box<Expression>>,
        modifier: Option<Box<Expression>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub args: Vec<String>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CommandLine {
    pub cmd: Command,
    pub line: usize,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub commands: Vec<CommandLine>,
    pub functions: HashMap<String, Function>,
}

#[derive(Debug, Fail)]
pub enum MaidenError {
    #[fail(display = "parsing error: {}", kind)]
    Pest { kind: pest::error::Error<peg::Rule> },
    #[fail(display = "IO Error")]
    Io {
        #[fail(cause)]
        io_error: std::io::Error,
    },
    #[fail(display = "Missing variable '{}'", name)]
    MissingVariable { name: String, line: usize },
    #[fail(display = "Missing function '{}'", name)]
    MissingFunction { name: String, line: usize },
    #[fail(
        display = "Wrong argument count to function (expected {}, got {})",
        expected, got
    )]
    WrongArgCount {
        expected: usize,
        got: usize,
        line: usize,
    },
    #[fail(display = "Bad boolean resolve: {:?}", expression)]
    BadBooleanResolve { expression: String, line: usize },
    #[fail(display = "Unparsable number: '{}'", number)]
    ParseNumberError { number: String, line: usize },
    #[fail(display = "No end of if statement")]
    NoEndOfIf { line: usize },
    #[fail(display = "Unimplemented: {}", description)]
    Unimplemented { description: String, line: usize },
    #[fail(display = "Exceeded maximum allowed stack depth of {}", depth)]
    StackOverflow { depth: u32, line: usize },
    #[fail(display = "Hit instruction limit of 10,000,000. Infinite loop?")]
    InstructionLimit { line: usize },
    #[fail(display = "Got to a pronoun, but no variable defined")]
    UndefinedPronoun { line: usize },
    #[fail(display = "Got infinity on divide between {} and {}", x, y)]
    Infinity { x: String, y: String, line: usize },
    #[fail(display = "Expected another item, but didn't get one")]
    Incomplete { line: usize },
    #[fail(display = "Bad string. Expected length at least 2 and got {}", length)]
    BadString { length: usize, line: usize },
    #[fail(display = "Expected an expression, got: {}", other)]
    NotAnExpression { other: String, line: usize },
    #[fail(display = "Expected an symbol, got: {}", other)]
    NotASymbol { other: String, line: usize },
    #[fail(display = "Expected an command, got: {}", other)]
    NotACommand { other: String, line: usize },
    #[fail(display = "Expected an block, got: {}", other)]
    NotABlock { other: String, line: usize },
}

pub type Result<T> = ::core::result::Result<T, MaidenError>;

impl From<std::io::Error> for MaidenError {
    fn from(err: std::io::Error) -> MaidenError {
        return MaidenError::Io { io_error: err };
    }
}
