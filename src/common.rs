#[derive(Debug)]
pub enum Command {
    Assignment { target: String, value: String },
    UntilIs { target: String, value: String, loop_end: Option<usize> },
    Increment { target: String },
    Next { loop_start: usize},
    Say { value: String }
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