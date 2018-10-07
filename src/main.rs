#![recursion_limit = "5000"]
#![deny(warnings)]
#![cfg_attr(feature = "cargo-clippy", allow(needless_return))]

#[macro_use]
extern crate nom;
#[macro_use]
extern crate log;
#[cfg(not(target_arch = "wasm32"))]
extern crate pretty_env_logger;
#[macro_use]
extern crate error_chain;
#[cfg(not(target_arch = "wasm32"))]
extern crate clap;
extern crate regex;
#[macro_use]
extern crate nom_locate;

#[cfg(target_arch = "wasm32")]
#[macro_use]
extern crate yew;
#[cfg(target_arch = "wasm32")]
use yew::prelude::*;
#[cfg(target_arch = "wasm32")]
#[macro_use]
extern crate stdweb;
#[cfg(target_arch = "wasm32")]
use stdweb::web::IParentNode;

mod common;
mod parser;
mod runner;

#[cfg(not(target_arch = "wasm32"))]
use clap::{App, Arg};
#[cfg(not(target_arch = "wasm32"))]
use std::fs::File;
#[cfg(not(target_arch = "wasm32"))]
use std::io::{self, Read};

#[cfg(not(target_arch = "wasm32"))]
fn main() -> common::Result<()> {
    pretty_env_logger::try_init().unwrap_or(());
    let matches = App::new("Maiden")
        .version("1.0")
        .author("Tom Parker <palfrey@tevp.net>")
        .about("Rockstar interpreter")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        ).get_matches();
    let mut f = File::open(matches.value_of("INPUT").unwrap())?;
    let mut buffer = String::new();
    f.read_to_string(&mut buffer)?;

    let program = parser::parse(&buffer)?;
    runner::run(&program, &mut io::stdout())?;
    Ok(())
}

#[cfg(target_arch = "wasm32")]
mod web;

#[cfg(target_arch = "wasm32")]
fn main() {
    yew::initialize();
    let app: App<web::Model> = App::new();
    let app_element = stdweb::web::document()
        .query_selector("#app")
        .unwrap()
        .unwrap();
    app.mount(app_element);
    yew::run_loop();
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::Expression;
    use std::collections::HashMap;
    use std::io::Cursor;

    fn test_program(code: &str, end_variables: HashMap<String, Expression>, expected_output: &str) {
        pretty_env_logger::try_init().unwrap_or(());
        let program = parser::parse(code).unwrap();
        info!("Commands: {:?}", program.commands);
        let mut writer = Cursor::new(Vec::new());
        let variables = runner::run(&program, &mut writer).unwrap();
        writer.set_position(0);
        let res = std::str::from_utf8(writer.get_ref()).unwrap();
        if res != "" {
            debug!("{}", res);
        }
        assert_eq!(expected_output, res);
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
            "buzz" => Expression::Integer(5),
            "limit" => Expression::Integer(100),
            "counter" => Expression::Integer(100),
            "fizz" => Expression::Integer(3),
        };
        test_program(program, end_variables, "");
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
            "my world" => Expression::Integer(100),
            "fire" => Expression::Integer(3),
            "hate" => Expression::Integer(5),
            "desire" => Expression::Integer(100),
        };
        test_program(program, end_variables, "");
    }

    #[test]
    fn rocking_fizzbuzz() {
        let program = "Midnight takes your heart and your soul
    While your heart is as high as your soul
    Put your heart without your soul into your heart

    Give back your heart

    Desire is a lovestruck ladykiller
    My world is nothing
    Fire is ice
    Hate is water
    Until my world is Desire,
    Build my world up
    If Midnight taking my world, Fire is nothing and Midnight taking my world, Hate is nothing
    Shout \"FizzBuzz!\"
    Take it to the top

    If Midnight taking my world, Fire is nothing
    Shout \"Fizz!\"
    Take it to the top

    If Midnight taking my world, Hate is nothing
    Say \"Buzz!\"
    Take it to the top

    Whisper my world
    And around we go";
        let end_variables = hashmap! {
            "my world" => Expression::Integer(100),
            "fire" => Expression::Integer(3),
            "hate" => Expression::Integer(5),
            "desire" => Expression::Integer(100),
        };
        test_program(
            program,
            end_variables,
            concat!(
                "1\n2\nFizz!\n4\nBuzz!\nFizz!\n7\n8\nFizz!\nBuzz!\n11\nFizz!\n13\n14\nFizzBuzz!\n16\n17\nFizz!\n",
                "19\nBuzz!\nFizz!\n22\n23\nFizz!\nBuzz!\n26\nFizz!\n28\n29\nFizzBuzz!\n31\n32\nFizz!\n34\nBuzz!\n",
                "Fizz!\n37\n38\nFizz!\nBuzz!\n41\nFizz!\n43\n44\nFizzBuzz!\n46\n47\nFizz!\n49\nBuzz!\nFizz!\n52\n",
                "53\nFizz!\nBuzz!\n56\nFizz!\n58\n59\nFizzBuzz!\n61\n62\nFizz!\n64\nBuzz!\nFizz!\n67\n68\nFizz!\n",
                "Buzz!\n71\nFizz!\n73\n74\nFizzBuzz!\n76\n77\nFizz!\n79\nBuzz!\nFizz!\n82\n83\nFizz!\nBuzz!\n86\n",
                "Fizz!\n88\n89\nFizzBuzz!\n91\n92\nFizz!\n94\nBuzz!\nFizz!\n97\n98\nFizz!\nBuzz!\n"
            ),
        );
    }

    #[test]
    fn multi_word_say() {
        let end_variables = HashMap::new();
        test_program(
            "say \"shout let it all out\"",
            end_variables,
            "shout let it all out\n",
        );
    }

    #[test]
    fn multiple_uppercase_proper_variable() {
        let end_variables = hashmap! {
            "id" => Expression::Integer(3),
        };
        test_program("put foo into ID", end_variables, "");
    }

    #[test]
    fn double_increment() {
        let end_variables = hashmap!{
            "my world" => Expression::Integer(2),
        };
        test_program(
            "Put 0 into my world\nBuild my world up, up",
            end_variables,
            "",
        );
    }

    #[test]
    fn double_decrement() {
        let end_variables = hashmap!{
            "the walls" => Expression::Integer(-2),
        };
        test_program(
            "Put 0 into the walls\nKnock the walls down, down",
            end_variables,
            "",
        );
    }

    #[test]
    fn expression_comment() {
        let end_variables = hashmap! {
            "baz" => Expression::String("foo".to_string()),
        };
        test_program("Baz is \"foo\" (bar)", end_variables, "");
    }

    #[test]
    fn skip_else() {
        let end_variables = hashmap! {
            "foo" => Expression::String("foo".to_string()),
        };
        test_program(
            "if nothing is nothing
        Foo is \"foo\"
        Else
        Bar is \"bar\"

        ",
            end_variables,
            "",
        );
    }

    #[test]
    fn numeric_args() {
        let err = test_error("Multiply taking 3, 5");
        if let common::ErrorKind::MissingFunction(name, line) = err {
            assert_eq!(name, "Multiply");
            assert_eq!(line, 1);
        } else {
            assert!(false, err);
        }
    }

    fn test_error(input: &str) -> common::ErrorKind {
        pretty_env_logger::try_init().unwrap_or(());
        let program = parser::parse(input).unwrap();
        let mut writer = Cursor::new(Vec::new());
        runner::run(&program, &mut writer).err().unwrap().0
    }

    #[test]
    fn missing_variable() {
        let err = test_error("Put Desire into my world");
        if let common::ErrorKind::MissingVariable(name, line) = err {
            assert_eq!(name, "Desire");
            assert_eq!(line, 1);
        } else {
            assert!(false, err);
        }
    }

    #[test]
    fn end_of_if() {
        let err = test_error("if this is tests");
        if let common::ErrorKind::NoEndOfIf(line) = err {
            assert_eq!(line, 1);
        } else {
            assert!(false, err);
        }
    }

    #[test]
    fn no_end_func() {
        let err = test_error("What Remains takes the fighters and a war");
        if let common::ErrorKind::NoEndFunction(line) = err {
            assert_eq!(line, 1);
        } else {
            assert!(false, err);
        }
    }

    #[test]
    fn no_end_loop() {
        let err = test_error("until this is that");
        if let common::ErrorKind::NoEndLoop(line) = err {
            assert_eq!(line, 1);
        } else {
            assert!(false, err);
        }
    }
}
