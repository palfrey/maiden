#[cfg(any(test, target_arch = "wasm32"))]
use crate::common::{Command, CommandLine, Program};

#[cfg(any(test, target_arch = "wasm32"))]
fn print_command(
    command: &Command,
    last_line: usize,
    indent: u16,
    max_number_length: usize,
) -> String {
    match command {
        Command::FunctionDeclaration { name, args, block } => format!(
            "FunctionDeclaration {{ name: \"{}\", args: {:?}, block: Block {{\n{}",
            name,
            args,
            print_commands(&block.commands, last_line, indent + 1, max_number_length)
        ),
        _ => format!("{:?}", command),
    }
}

#[cfg(any(test, target_arch = "wasm32"))]
fn print_commands(
    commands: &Vec<CommandLine>,
    mut last_line: usize,
    mut indent: u16,
    max_number_length: usize,
) -> String {
    let mut res = String::new();
    for command in commands {
        while command.line != 0 && last_line < command.line - 1 {
            last_line += 1;
            res += &format!("{:0width$}:\n", last_line, width = max_number_length);
        }
        last_line = command.line;
        res += &format!("{:0width$}: ", command.line, width = max_number_length);
        for _ in 0..indent {
            res += "  ";
        }
        res += &(print_command(&command.cmd, last_line, indent, max_number_length) + "\n");
        match command.cmd {
            Command::If { .. } | Command::While { .. } | Command::Until { .. } => {
                indent += 1;
            }
            _ => {}
        }
    }
    res
}

#[cfg(any(test, target_arch = "wasm32"))]
pub fn print_program(program: &Program) -> String {
    let indent = 0;
    let last_line = 0;
    let max_line: f32 = (program.commands.iter().fold(0, |acc, x| acc.max(x.line))) as f32;
    let max_number_length: usize = (max_line + 1.0).log10().ceil() as usize;
    print_commands(&program.commands, last_line, indent, max_number_length)
}

#[cfg(test)]
mod tests {
    use super::print_program;
    use crate::parser;

    #[test]
    fn test_print_function() {
        let code = "Midnight takes your heart & your soul
        Give back your heart
        ";
        let program = parser::parse(code).unwrap();
        let expected = "1: FunctionDeclaration { name: \"Midnight\", args: [\"your heart\", \"your soul\"], block: Block {
2:   Return { return_value: Variable(\"your heart\") }
        }}\n";
        assert_eq!(expected, print_program(&program));
    }
}
