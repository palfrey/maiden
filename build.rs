use failure::{Error, ResultExt};
use serde::Deserialize;
use std::{
    collections::HashMap,
    fs::{self, File},
    io::Write,
    path::Path,
    process::Command,
};
use walkdir::WalkDir;

fn make_tests() -> Result<(), Error> {
    let out_dir = std::env::var("OUT_DIR")?;
    let destination = std::path::Path::new(&out_dir).join("test.rs");
    let mut f = std::fs::File::create(destination)?;

    for entry in WalkDir::new("tests").follow_links(true) {
        let name = entry?
            .path()
            .to_str()
            .unwrap()
            .to_string()
            .replace("tests/", "");
        if name.to_lowercase().ends_with(".rock") {
            let test_name = name.replace(['.', '-', '/'], "_");
            if test_name == "upstream_fixtures_equality_mysterious_rock"
                || test_name == "upstream_fixtures_equality_equalityComparison_rock"
                || test_name == "upstream_fixtures_equality_nothing_rock"
            {
                continue; // FIXME: Blocked by https://github.com/RockstarLang/rockstar/pull/238
            }
            if test_name == "upstream_fixtures_operators_multiplicationOperator_rock" {
                continue; // FIXME: Blocked by https://github.com/RockstarLang/rockstar/issues/162
            }

            let function = if name.contains("failures") {
                "parse_fail_file_test"
            } else {
                "success_file_test"
            };
            write!(
                f,
                "
    #[test]
    #[allow(non_snake_case)]
    fn {test_name}() {{
        {function}(\"{name}\");
    }}",
                name = name,
                test_name = test_name,
                function = function
            )?;
        }
    }
    Ok(())
}

fn display_name(name: &str) -> String {
    if ["break", "continue", "return", "true", "false", "loop"].contains(&name) {
        format!("{name}_kw")
    } else if name == "_" {
        return "SPACING".to_owned();
    } else if name == "EOF" {
        return "EOI".to_owned();
    } else {
        name.to_string()
    }
}

#[derive(Deserialize, Debug, Clone)]
#[serde(untagged)]
enum ClassPart {
    String(String),
    List(Vec<String>),
}

#[derive(Deserialize, Debug, Clone)]
#[serde(rename_all(deserialize = "lowercase"))]
#[serde(tag = "type")]
enum Expression {
    Action {
        expression: Box<Expression>,
    },
    Labeled {
        expression: Box<Expression>,
    },
    #[serde(rename = "zero_or_more")]
    ZeroOrMore {
        expression: Box<Expression>,
    },
    #[serde(rename = "one_or_more")]
    OneOrMore {
        expression: Box<Expression>,
    },
    #[serde(rename = "rule_ref")]
    RuleRef {
        name: String,
    },
    Choice {
        alternatives: Vec<Expression>,
    },
    Sequence {
        elements: Vec<Expression>,
    },
    Class {
        parts: Vec<ClassPart>,
        inverted: bool,
    },
    Literal {
        value: String,
    },
    #[serde(rename = "simple_not")]
    SimpleNot {
        expression: Box<Expression>,
    },
    #[serde(rename = "semantic_not")]
    SemanticNot {
        expression: Option<Box<Expression>>,
    },
    Optional {
        expression: Box<Expression>,
    },
    Text {
        expression: Box<Expression>,
    },
    Group {
        expression: Box<Expression>,
    },
    Any {},
}

impl Expression {
    fn needs_brackets(&self) -> bool {
        return match self {
            Expression::Choice { alternatives: _ } | Expression::Sequence { elements: _ } => true,
            Expression::Class { parts, inverted: _ } => {
                if parts.len() > 1 {
                    return true;
                } else if let ClassPart::List(_) = parts.first().unwrap() {
                    return true;
                } else {
                    return false;
                }
            }
            _ => false,
        };
    }

    fn print(&self, level: u8) -> String {
        let raw_print = match self {
            Expression::Action { expression }
            | Expression::Labeled { expression }
            | Expression::Group { expression }
            | Expression::Text { expression } => expression.print(level),
            Expression::ZeroOrMore { expression } => {
                let child = expression.print(level + 1);
                format!("{child}*")
            }
            Expression::OneOrMore { expression } => {
                let child = expression.print(level + 1);
                format!("{child}+")
            }
            Expression::RuleRef { name } => display_name(name),
            Expression::Choice { alternatives } => {
                let options: Vec<String> =
                    alternatives.iter().map(|e| e.print(level + 1)).collect();
                options.join(" | ")
            }
            Expression::Sequence { elements } => {
                let options: Vec<String> = elements.iter().map(|e| e.print(level + 1)).collect();
                options.join(" ~ ")
            }
            Expression::Literal { value } => {
                if level > 1 {
                    if value == "while" {
                        return "while_kw".to_owned();
                    }
                    if value == "until" {
                        return "until_kw".to_owned();
                    }
                }
                if level == 3 {
                    if value == "up" {
                        return "up_kw".to_owned();
                    }
                    if value == "down" {
                        return "down_kw".to_owned();
                    }
                }
                if value == "\n" {
                    return "NEWLINE".to_owned();
                }
                format!("^{:?}", value)
            }
            Expression::Class { parts, inverted } => {
                let items: Vec<String> = parts
                    .iter()
                    .map(|p| match p {
                        ClassPart::String(s) => format!("{:?}", s),
                        ClassPart::List(items) => {
                            let formatted: Vec<String> =
                                items.iter().map(|s| format!("'{}'", s)).collect();
                            formatted.join("..")
                        }
                    })
                    .collect();
                let res = items.join(" | ");
                if *inverted {
                    format!("(!{res} ~ ANY)")
                } else {
                    res
                }
            }
            Expression::SimpleNot { expression } => {
                let child: String = expression.print(level + 1);
                format!("! {child}")
            }
            Expression::SemanticNot { expression } => {
                if let Some(exp) = expression {
                    let child = exp.print(level + 1);
                    format!("!{child}")
                } else {
                    "".to_owned()
                }
            }
            Expression::Any {} => String::from("ANY"),
            Expression::Optional { expression } => {
                let child = expression.print(level + 1);
                format!("{child}?")
            }
        };

        if level > 0 && self.needs_brackets() {
            format!("({raw_print})")
        } else {
            raw_print
        }
    }
}

#[derive(Deserialize, Debug)]
struct Rule {
    #[serde(alias = "type")]
    kind: String,
    name: String,
    expression: Expression,
}

#[derive(Deserialize)]
struct Ast {
    rules: Vec<Rule>,
}

fn make_peg() -> Result<(), Error> {
    if !Path::new("node_modules/.bin/peggy").exists() {
        Command::new("./pnpm")
            .args(["install"])
            .output()
            .context("pnpm install")?;
    }
    if !Path::new("target/rockstar.ast").exists() {
        Command::new("./node_modules/.bin/peggy")
            .args([
                "--ast",
                "./spec/satriani/rockstar.peg",
                "-o",
                "target/rockstar.ast",
            ])
            .output()
            .context("peggy")?;
    }

    let mut output_peg = File::create("src/rockstar.peg").context("Creating rockstar.peg")?;
    output_peg.write_all(
        b"/*
PEG grammar for Rockstar (https://codewithrockstar.com)
Generated from spec/satriani/rockstar.peg, do not edit here!
*/

",
    )?;
    let ast: Ast = serde_json::from_str(
        &fs::read_to_string("target/rockstar.ast").context("Reading the AST")?,
    )?;

    fn write_rule(output_peg: &mut File, rule: &Rule) -> Result<(), Error> {
        let name = &rule.name;
        let printed = rule.expression.print(0);
        let ignore = {
            if [
                "noise",
                "whitespace",
                "comment",
                "ignore_rest_of_line",
                "SPACING",
                "EOL",
            ]
            .contains(&name.as_str())
            {
                "_"
            } else if [
                "true",
                "false",
                "number",
                "common_variable",
                "poetic_number",
                "poetic_digit",
                "proper_noun",
                "proper_variable",
                "simple_variable",
                "poetic_string",
            ]
            .contains(&name.as_str())
            {
                "@"
            } else {
                ""
            }
        };
        let disp_name = display_name(name);
        output_peg.write_all(format!("{disp_name} = {}{{ {printed} }}\n\n", ignore).as_bytes())?;
        Ok(())
    }

    let mut extras: HashMap<String, Vec<Rule>> = HashMap::new();
    extras.insert(
        String::from("function_call"),
        vec![Rule {
            name: "args_list".to_owned(),
            kind: "rule".to_owned(),
            expression: Expression::RuleRef {
                name: "expression_list".to_owned(),
            },
        }],
    );
    extras.insert(
        "loop".to_owned(),
        vec![
            Rule {
                name: "while_kw".to_owned(),
                kind: "rule".to_owned(),
                expression: Expression::Literal {
                    value: "while".to_owned(),
                },
            },
            Rule {
                name: "until_kw".to_owned(),
                kind: "rule".to_owned(),
                expression: Expression::Literal {
                    value: "until".to_owned(),
                },
            },
        ],
    );
    extras.insert(
        "increment".to_owned(),
        vec![Rule {
            name: "up_kw".to_owned(),
            kind: "rule".to_owned(),
            expression: Expression::Literal {
                value: "up".to_owned(),
            },
        }],
    );
    extras.insert(
        "decrement".to_owned(),
        vec![Rule {
            name: "down_kw".to_owned(),
            kind: "rule".to_owned(),
            expression: Expression::Literal {
                value: "down".to_owned(),
            },
        }],
    );
    extras.insert(
        "eq".to_owned(),
        vec![
            Rule {
                name: "ne".to_owned(),
                kind: "rule".to_owned(),
                expression: Expression::Choice {
                    alternatives: vec![
                        Expression::Literal {
                            value: "aint".to_owned(),
                        },
                        Expression::Literal {
                            value: "ain\'t".to_owned(),
                        },
                    ],
                },
            },
            Rule {
                name: "is_kw".to_owned(),
                kind: "rule".to_owned(),
                expression: Expression::Literal {
                    value: "is".to_owned(),
                },
            },
        ],
    );
    extras.insert(
        "assignment".to_owned(),
        vec![Rule {
            name: "put_assignment".to_owned(),
            kind: "rule".to_owned(),
            expression: Expression::Sequence {
                elements: vec![
                    Expression::Literal {
                        value: "put".to_owned(),
                    },
                    Expression::OneOrMore {
                        expression: Box::new(Expression::RuleRef {
                            name: "SPACING".to_owned(),
                        }),
                    },
                    Expression::RuleRef {
                        name: "nor".to_owned(),
                    },
                    Expression::RuleRef {
                        name: "target".to_owned(),
                    },
                ],
            },
        }],
    );

    for mut rule in ast.rules {
        if rule.kind != "rule" {
            continue;
        }
        if rule.name == "EOF" {
            continue;
        }
        if rule.name == "program" {
            rule.expression = Expression::Sequence {
                elements: vec![
                    Expression::RuleRef {
                        name: "SOI".to_owned(),
                    },
                    rule.expression,
                    Expression::RuleRef {
                        name: "EOI".to_owned(),
                    },
                ],
            };
        }
        let not_keyword = Expression::SemanticNot {
            expression: Some(Box::new(Expression::RuleRef {
                name: "keyword".to_owned(),
            })),
        };
        if rule.name == "simple_variable" {
            if let Expression::Action {
                expression: ref mut act_expression,
            } = rule.expression
            {
                if let Expression::Sequence { ref mut elements } = **act_expression {
                    elements.insert(0, not_keyword.clone());
                    elements.remove(2);
                }
            }
        }
        if rule.name == "proper_noun" {
            if let Expression::Action {
                expression: ref mut act_expression,
            } = rule.expression
            {
                if let Expression::Sequence { ref mut elements } = **act_expression {
                    elements.insert(0, not_keyword);
                    elements.remove(2);
                }
            }
        }
        if let Some(extra_rules) = extras.get(&rule.name) {
            for extra_rule in extra_rules {
                write_rule(&mut output_peg, extra_rule)?;
            }
        }
        if rule.name == "_" {
            rule.name = "SPACING".to_owned();
        }
        if rule.name == "eq" {
            rule.expression = Expression::Choice {
                alternatives: vec![
                    Expression::RuleRef {
                        name: "ne".to_owned(),
                    },
                    Expression::RuleRef {
                        name: "is_kw".to_owned(),
                    },
                ],
            };
        }
        if rule.name == "function_call" {
            if let Expression::Action {
                expression: ref mut items,
            } = rule.expression
            {
                if let Expression::Sequence { ref mut elements } = **items {
                    if let Expression::Labeled {
                        expression: ref mut labelled_expression,
                    } = elements.last_mut().unwrap()
                    {
                        if let Expression::RuleRef { ref mut name } = **labelled_expression {
                            *name = "args_list".to_owned();
                        }
                    }
                }
            }
        }
        if rule.name == "assignment" {
            if let Expression::Choice {
                ref mut alternatives,
            } = rule.expression
            {
                for choice in alternatives {
                    if let Expression::Action {
                        expression: ref mut act_expression,
                    } = choice
                    {
                        if let Expression::Sequence { ref mut elements } = **act_expression {
                            if let Expression::Literal { value } = elements.first().unwrap() {
                                if value == "put" {
                                    **act_expression = Expression::RuleRef {
                                        name: "put_assignment".to_owned(),
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                println!("Fail rule 1 {:?}", rule)
            }
        }
        if rule.name == "variable" {
            output_peg.write_all(r#"// To disallow identifiers like "My back is hurting" (which is illegal because "back" is a keyword)
// we need to explicitly define all language keywords, and they MUST be matched in descending order of length
// because of Complicated Weird Parser Reasons.
kw10 = { ^"mysterious" }
kw8 = { ^"stronger" | ^"continue"}
kw7 = { ^"between" | ^"greater" | ^"nothing" | ^"nowhere" | ^"smaller" | ^"whisper" | ^"without"}
kw6 = { ^"ain't" | ^"bigger" | ^"listen" | ^"nobody" | ^"return" | ^"scream" | ^"taking" | ^"weaker" | ^"higher"
    | ^"strong"}
kw5 = { ^"break" | ^"build" | ^"empty" | ^"false" | ^"great" | ^"knock" | ^"lower" | ^"right" | ^"shout" | ^"small"
    | ^"take " | ^"takes" | ^"times" | ^"until" | ^"unite" | ^"while" | ^"wrong" | ^"minus"}
kw4 = { ^"aint" | ^"back" | ^"cast" | ^"burn" | ^"join" | ^"down" | ^"else" | ^"give" | ^"gone" | ^"high" | ^"into" | ^"less" | ^"lies" | ^"null"
    | ^"plus" | ^"says" | ^"than" | ^"them" | ^"they" | ^"true" | ^"weak" | ^"were" | ^"your" | ^"over" | ^"with"}
kw3 = { ^"and" | ^"big" | ^"her" | ^"him" | ^"hir" | ^"it " | ^"let" | ^"low" | ^"nor" | ^"not" | ^"put" | ^"say" | ^"she"
    | ^"the" | ^"top" | ^"ver" | ^"was" | ^"xem" | ^"yes" | ^"zie" | ^"zir"}
kw2 = { ^"an" | ^"as" | ^"at" | ^"be" | ^"he" | ^"if" | ^"is" | ^"it" | ^"my" | ^"no" | ^"of" | ^"ok" | ^"or" | ^"to" | ^"up" | ^"ve"
    | ^"xe" | ^"ze" }
kw1 = { ^"a" }

keyword = @{(kw10 | kw8 | kw7 | kw6 | kw5 | kw5 | kw4 | kw3 | kw2 | kw1) ~ !letter }

"#.as_bytes())?;
        }
        if rule.name == "line" {
            if let Expression::Choice {
                ref mut alternatives,
            } = rule.expression
            {
                if let Expression::Action { ref mut expression } = alternatives[0] {
                    if let Expression::Sequence { ref mut elements } = **expression {
                        elements[2] = Expression::Choice {
                            alternatives: vec![
                                Expression::RuleRef {
                                    name: "EOL".to_owned(),
                                },
                                Expression::Sequence {
                                    elements: vec![
                                        Expression::ZeroOrMore {
                                            expression: Box::new(Expression::RuleRef {
                                                name: "SPACING".to_owned(),
                                            }),
                                        },
                                        Expression::RuleRef {
                                            name: "EOI".to_owned(),
                                        },
                                    ],
                                },
                            ],
                        }
                    }
                }
            }
        }
        write_rule(&mut output_peg, &rule)?;
    }
    Ok(())
}

fn main() -> Result<(), Error> {
    make_tests()?;
    make_peg()?;
    Ok(())
}
