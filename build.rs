use std::io::Write;
extern crate walkdir;
use walkdir::WalkDir;

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let destination = std::path::Path::new(&out_dir).join("test.rs");
    let mut f = std::fs::File::create(&destination).unwrap();

    for entry in WalkDir::new("tests").follow_links(true) {
        let name = entry
            .unwrap()
            .path()
            .to_str()
            .unwrap()
            .to_string()
            .replace("tests/", "");
        if name.ends_with(".rock") {
            let test_name = name.replace(".", "_").replace("-", "_").replace("/", "_");
            if test_name == "upstream_fixtures_equality_mysterious_rock"
                || test_name == "upstream_fixtures_equality_equalityComparison_rock"
                || test_name == "upstream_fixtures_equality_nothing_rock"
            {
                continue; // FIXME: Blocked by https://github.com/RockstarLang/rockstar/pull/238
            }
            if test_name == "upstream_fixtures_operators_multiplicationOperator_rock" {
                continue; // FIXME: Blocked by https://github.com/RockstarLang/rockstar/issues/162
            }

            if test_name.contains("_arrays_") {
                continue; // we don't implement arrays yet
            }
            if test_name == "upstream_fixtures_operators_list_expressions_arithmetic_rock" {
                continue; // haven't fully implemented list expressions
            }
            if test_name.contains("math_rounding") {
                continue; // don't support rounding yet
            }
            if test_name.contains("types_parsing") {
                continue; // don't support casts yet
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
            )
            .unwrap();
        }
    }
}
