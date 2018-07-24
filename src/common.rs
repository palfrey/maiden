#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Integer(i32),
    String(String)
}

#[derive(Debug, PartialEq, Eq)]
pub enum SymbolType {
    Is,
    Build,
    Up,
    Until,
    While,
    Next,
    Return,
    Say,
    And,
    If,
    Taking,
    Takes,
    Comma,
    String(String),
    Words(String)
}

#[derive(Debug)]
pub enum Command {
    Assignment { target: String, value: SymbolType },
    UntilIs { target: String, value: SymbolType, loop_end: Option<usize> },
    Increment { target: String },
    Next { loop_start: usize},
    Say { value: SymbolType }
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
        NonAlphabeticWord(w: String)
    }
}