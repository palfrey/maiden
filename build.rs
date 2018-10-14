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
            if test_name == "upstream_correct_operators_equalityComparison_rock"
                || test_name == "upstream_correct_truthinessTest_rock"
            {
                continue; // FIXME: Blocked by https://github.com/dylanbeattie/rockstar/issues/168
            }
            write!(
                f,
                "
    #[test]
    #[allow(non_snake_case)]
    fn {test_name}() {{
        file_test(\"{name}\");
    }}",
                name = name,
                test_name = test_name
            ).unwrap();
        }
    }
}
