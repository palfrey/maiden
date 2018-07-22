#[macro_use]
extern crate nom;

use nom::types::CompleteStr;

fn main() {
    println!("Hello, world!");
}

pub fn is_space(chr: char) -> bool {
  chr == ' ' || chr == '\t'
}

pub fn is_newline(chr: char) -> bool {
  chr == '\r' || chr == '\n'
}

pub fn is_alphabetic(chr: char) -> bool {
  (chr >= 'A' && chr <= 'Z') || (chr >= 'a' && chr <= 'z')
}

pub fn is_digit(chr: char) -> bool {
  chr >= '0' && chr <= '9'
}

pub fn is_alphanumeric(chr: char) -> bool {
  is_alphabetic(chr) || is_digit(chr)
}

named!(line<CompleteStr, Vec<&str> >, many0!(
    do_parse!(
        word: take_while!(is_alphanumeric) >>
        take_while!(is_space) >>
        (*word)
    )));

named!(lines<CompleteStr, Vec<Vec<&str> > >, many0!(
    do_parse!(
        a_line: line >>
        take_while!(is_newline) >>
        (a_line)
    )));

// enum Command {
//     Assignment { target: String, value: String }
// }

// named!(command<Vec<&str>, Command>,
//     alt!()

fn parse(input: &str) {
    let raw_lines = lines(CompleteStr(input)).unwrap().1;
    println!("{:?}", raw_lines);
}

#[test]
fn test_counting() {
    let program = "Limit is 100
Counter is 0
Fizz is 3
Buzz is 5
Until Counter is Limit
	Build Counter up
End";
    parse(program);
}