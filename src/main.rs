#![allow(non_shorthand_field_patterns)]

#[macro_use]
extern crate nom;
#[macro_use]
extern crate log;
extern crate pretty_env_logger;

mod common;
mod parser;
mod runner;

use std::io::Cursor;
use std::collections::HashMap;
use runner::Variable;

fn main() {
    println!("Hello, world!");
}

fn test_program(program: &str, end_variables: HashMap<String, Variable>) {
    pretty_env_logger::try_init().unwrap_or(());
    let commands = parser::parse(program);
    let mut writer = Cursor::new(Vec::new());
    let variables = runner::run(commands, &mut writer).unwrap();
    writer.set_position(0);
    let res = std::str::from_utf8(writer.get_ref()).unwrap();
    if res != "" {
        println!("{}", res);
    }
    assert_eq!(end_variables, variables);
}

// https://gist.github.com/DmitrySoshnikov/8439eac0a09d9fafe55a83c88d049117
macro_rules! hashmap(
    { $($key:expr => $value:expr),+, } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key.to_string(), $value);
            )+
            m
        }
     };
);

#[test]
fn test_counting() {
    let program = "Limit is 100
Counter is 0
Fizz is 3
Buzz is 5
Until Counter is Limit
	Build Counter up
End";
    let end_variables = hashmap! {
        "buzz" => Variable::Integer(5),
        "limit" => Variable::Integer(100),
        "counter" => Variable::Integer(100),
        "fizz" => Variable::Integer(3),
    };
    test_program(program, end_variables);
}

#[test]
fn test_rocking_counting() {
    let program = "Desire is a lovestruck ladykiller
My world is nothing
Fire is ice
Hate is water
Until my world is Desire,
Build my world up
And around we go";
    let end_variables = hashmap! {
        "my world" => Variable::Integer(100),
        "fire" => Variable::Integer(3),
        "hate" => Variable::Integer(5),
        "desire" => Variable::Integer(100),
    };
    test_program(program, end_variables);
}