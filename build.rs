use std::io::Write;
extern crate walkdir;
use failure::Error;
use walkdir::WalkDir;

fn main() -> Result<(), Error> {
    let out_dir = std::env::var("OUT_DIR")?;
    let destination = std::path::Path::new(&out_dir).join("test.rs");
    let mut f = std::fs::File::create(&destination)?;

    for entry in WalkDir::new("tests").follow_links(true) {
        let name = entry?
            .path()
            .to_str()
            .unwrap()
            .to_string()
            .replace("tests/", "");
        if name.to_lowercase().ends_with(".rock") {
            let test_name = name.replace('.', "_").replace('-', "_").replace('/', "_");
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
