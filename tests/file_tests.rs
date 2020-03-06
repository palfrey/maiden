#![allow(warnings)]
extern crate assert_cmd;

#[cfg(test)]
mod integration {
    use assert_cmd::prelude::*;
    use pretty_assertions::assert_eq;
    use std;
    use std::fs::File;
    use std::io::{ErrorKind, Read};
    use std::process::Command;
    use std::path::Path;
    use log::warn;

    include!(concat!(env!("OUT_DIR"), "/test.rs"));

    fn file_or_empty(fname_raw: String) -> String {
        let fname = Path::new(&fname_raw);
        if fname.exists() {
            let mut f = File::open(&fname).expect(&format!("Trying to open '{}'", fname.display()));
            let mut expected = String::new();
            f.read_to_string(&mut expected).unwrap();
            expected
        } else {
            String::new()
        }
    }

    fn success_file_test(name: &str) {
        let expected_out = file_or_empty(format!("./tests/{}.out", name));
        let expected_err = file_or_empty(format!("./tests/{}.err", name));

        if !expected_err.is_empty() {
            warn!("Don't yet support non-zero errors, because of differences in implementation");
            return;
        }

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
        // assert_eq!(stderr, expected_err);
        let stdout = std::str::from_utf8(&output.stdout).unwrap();
        assert_eq!(stdout, expected_out, "stdout: {}\n\nstderr: {}", stdout, stderr);
    }

    fn parse_fail_file_test(name: &str) {
        let args = [&format!("./tests/{}", name)];
        let mut mb = Command::main_binary().unwrap();
        let output = mb.args(&args).output().unwrap();
        assert!(!output.status.success());

        let stderr = std::str::from_utf8(&output.stderr).unwrap();
        assert!(stderr.contains("Error:"), stderr.to_string());

        let stdout = std::str::from_utf8(&output.stdout).unwrap();
        assert_eq!(stdout, "");
    }
}
