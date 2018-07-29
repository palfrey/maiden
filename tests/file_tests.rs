extern crate assert_cmd;

#[cfg(test)]
mod integration {
    use std::process::Command;
    use assert_cmd::prelude::*;
    use std::fs::File;
    use std::io::Read;

    #[test]
    fn calling_maiden_with_fizzbuzz() {
        let mut f = File::open("./tests/fizzbuzz.rock.expected").unwrap();
        let mut expected = Vec::new();
        f.read_to_end(&mut expected).unwrap();

        let args = ["./tests/fizzbuzz.rock"];
        let mut mb = Command::main_binary().unwrap();
        let output = mb.args(&args).output().unwrap();
        assert_eq!(output.stdout, expected);
    }
}