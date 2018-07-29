extern crate assert_cmd;

#[cfg(test)]
mod integration {
    use std::process::Command;
    use assert_cmd::prelude::*;
    use std::fs::File;
    use std::io::Read;
    use std;

    #[test]
    fn fizzbuzz() {
        file_test("fizzbuzz")
    }

    #[test]
    fn minimalist() {
        file_test("fizzbuzz_minimalist")
    }

    fn file_test(name: &str) {
        let mut f = File::open(&format!("./tests/{}.rock.expected", name)).unwrap();
        let mut expected = String::new();
        f.read_to_string(&mut expected).unwrap();

        let args = [&format!("./tests/{}.rock", name)];
        let mut mb = Command::main_binary().unwrap();
        let output = mb.args(&args).output().unwrap();
        let stderr = std::str::from_utf8(&output.stderr).unwrap();
        assert_eq!(stderr, "");
        let stdout = std::str::from_utf8(&output.stdout).unwrap();
        assert_eq!(stdout, expected);
    }
}
