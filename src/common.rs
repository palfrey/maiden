use nom::types::CompleteStr;
use nom_locate::LocatedSpan;
use std::collections::HashMap;
pub type Span<'a> = LocatedSpan<CompleteStr<'a>>;

#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub enum Expression {
    // Single items
    String(String),
    Floating(f64),
    Variable(String),
    Object(String), // currently just functions
    True,
    False,
    Call(String, Vec<Expression>),
    Nothing,
    Null,
    Mysterious,
    Pronoun,
    Not(Box<Expression>),

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

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone)]
pub enum SymbolType {
    Dummy,
    And,
    Or,
    Nor,
    Is,
    Build {
        target: String,
        count: usize,
    },
    Knock {
        target: String,
        count: usize,
    },
    Until,
    While,
    Next,
    Continue,
    Return,
    Say,
    If,
    Taking {
        target: String,
        args: Vec<SymbolType>,
    },
    Takes {
        name: String,
        args: Vec<String>,
    },
    Comma,
    GreaterThanOrEqual,
    GreaterThan,
    LessThan,
    LessThanOrEqual,
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
    Floating(String),
    Comment,
    Listen,
    Divide,
    True,
    False,
    Null,
    Mysterious,
    Pronoun,
    Not(Box<SymbolType>),
    Break,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub line: u32,
    pub symbol: SymbolType,
}

pub static LOWEST_PRECDENCE: SymbolType = SymbolType::Dummy;

#[derive(Debug, PartialEq)]
pub enum Command {
    Assignment {
        target: String,
        value: Expression,
    },
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
        else_loc: Option<usize>,
    },
    EndIf,
    Increment {
        target: String,
        count: f64,
    },
    Decrement {
        target: String,
        count: f64,
    },
    Next {
        loop_start: usize,
    },
    Continue {
        loop_start: usize,
    },
    Break {
        loop_start: usize,
    },
    Say {
        value: Expression,
    },
    Listen {
        target: Option<String>,
    },
    FunctionDeclaration {
        name: String,
        args: Vec<String>,
        func_end: Option<usize>,
    },
    Return {
        return_value: Expression,
    },
    EndFunction,
    Call {
        name: String,
        args: Vec<Expression>,
    },
    Else {
        if_start: usize,
    },
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
        ParseNumberError(number: String, line: u32) {
            display("Unparsable number: '{}'", number)
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
        ElseWithNoIf(line: u32) {
            display("Else with no if statement")
        }
        MultipleElse(line: u32) {
            display("More than one else statement")
        }
        NoEndFunction(line: u32) {
            display("No end of function")
        }
        NoEndLoop(line: u32) {
            display("No end of loop")
        }
        ContinueOutsideLoop(line: u32) {
            display("Continue outside of a loop")
        }
        BreakOutsideLoop(line: u32) {
            display("Break outside of a loop")
        }
        NextOutsideLoop(line: u32) {
            display("Next outside of a loop")
        }
        Unimplemented(description: String, line: u32) {
            display("Unimplemented: {}", description)
        }
        BadIncrement(sequence: Vec<SymbolType>, line: u32) {
            display("Bad 'increment' section: {:?}", sequence)
        }
        BadDecrement(sequence: Vec<SymbolType>, line: u32) {
            display("Bad 'decrement' section: {:?}", sequence)
        }
        StackOverflow(depth: u32, line: u32) {
            display("Exceeded maximum allowed stack depth of {}", depth)
        }
        InstructionLimit(line: u32) {
            display("Hit instruction limit of 10,000,000. Infinite loop?")
        }
        UndefinedPronoun(line: u32) {
            display("Got to a pronoun, but no variable defined")
        }
        Infinity(x: String, y: String, line: u32) {
            display("Got infinity on divide between {} and {}", x, y)
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
        Error(kind, _) => match kind {
            ErrorKind::MissingVariable(_, line) => line.clone(),
            ErrorKind::UnparsedText(_, line) => line.clone(),
            ErrorKind::MissingFunction(_, line) => line.clone(),
            ErrorKind::WrongArgCount(_, _, line) => line.clone(),
            ErrorKind::UnbalancedExpression(_, line) => line.clone(),
            ErrorKind::NoRunner(_, line) => line.clone(),
            ErrorKind::BadCommandSequence(_, line) => line.clone(),
            ErrorKind::ParseNumberError(_, line) => line.clone(),
            ErrorKind::BadIs(_, line) => line.clone(),
            ErrorKind::BadPut(_, line) => line.clone(),
            ErrorKind::NoEndOfIf(line) => line.clone(),
            ErrorKind::ElseWithNoIf(line) => line.clone(),
            ErrorKind::MultipleElse(line) => line.clone(),
            ErrorKind::NoEndFunction(line) => line.clone(),
            ErrorKind::NoEndLoop(line) => line.clone(),
            ErrorKind::BadBooleanResolve(_, line) => line.clone(),
            ErrorKind::BadFunctionDeclaration(_, line) => line.clone(),
            ErrorKind::BadIncrement(_, line) => line.clone(),
            ErrorKind::BadDecrement(_, line) => line.clone(),
            ErrorKind::Unimplemented(_, line) => line.clone(),
            ErrorKind::StackOverflow(_, line) => line.clone(),
            ErrorKind::InstructionLimit(line) => line.clone(),
            ErrorKind::UndefinedPronoun(line) => line.clone(),
            ErrorKind::Infinity(.., line) => line.clone(),
            _ => 0,
        },
    }
}
