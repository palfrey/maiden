extern crate assert_cmd;

#[cfg(test)]
mod integration {
    use assert_cmd::prelude::*;
    use std;
    use std::fs::File;
    use std::io::{ErrorKind, Read};
    use std::process::Command;

    include!(concat!(env!("OUT_DIR"), "/test.rs"));

    fn file_test(name: &str) {
        let mut f = File::open(&format!("./tests/{}.out", name)).unwrap();
        let mut expected = String::new();
        f.read_to_string(&mut expected).unwrap();

        let args = [&format!("./tests/{}", name)];
        let mut mb = Command::main_binary().unwrap();
        let input = File::open(&format!("./tests/{}.in'", name));
        let input_buffer = match input {
            Ok(mut in_f) => {
                let mut provided = String::new();
                in_f.read_to_string(&mut provided).unwrap();
                provided
            }
            Err(err) => {
                if err.kind() == ErrorKind::NotFound {
                    "".to_string()
                } else {
                    panic!("Error while reading input file: {}", err);
                }
            }
        };

        let output = mb
            .args(&args)
            .with_stdin()
            .buffer(input_buffer)
            .output()
            .unwrap();
        let stderr = std::str::from_utf8(&output.stderr).unwrap();
        assert_eq!(stderr, "");
        let stdout = std::str::from_utf8(&output.stdout).unwrap();
        assert_eq!(stdout, expected);
    }
}
