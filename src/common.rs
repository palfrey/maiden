#[derive(Debug, PartialEq, Clone, PartialOrd)]
pub enum Expression {
    Integer(i32),
    String(String),
    Variable(String),
    And(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Is(Box<Expression>, Box<Expression>),
    GreaterThanOrEqual(Box<Expression>, Box<Expression>),
    Call(String, Vec<Expression>),
    True,
    False,
}

#[derive(Debug, PartialEq, Eq)]
pub enum SymbolType {
    Is,
    Build,
    Up,
    Until,
    While,
    Next,
    Continue,
    Return,
    Say,
    And,
    If,
    Taking{target: String, args: Vec<String>},
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
    Integer(u32)
}

#[derive(Debug)]
pub enum Command {
    Assignment { target: String, value: Expression },
    Until { expression: Expression, loop_end: Option<usize> },
    While { expression: Expression, loop_end: Option<usize> },
    If { expression: Expression, if_end: Option<usize> },
    Increment { target: String },
    Next { loop_start: usize },
    Say { value: Expression },
    FunctionDeclaration { name: String, args: Vec<Expression>, func_end: Option<usize> },
    EndFunction,
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
        UnbalancedExpression(problem: String)
        MissingVariable(name: String)
    }
}