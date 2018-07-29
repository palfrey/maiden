use std::io::Write;
use std::fs;

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let destination = std::path::Path::new(&out_dir).join("test.rs");
    let mut f = std::fs::File::create(&destination).unwrap();

    let paths = fs::read_dir("tests").unwrap();

    for path in paths {
        let name = path.unwrap().file_name().into_string().unwrap();
        if name.ends_with(".rock") {
            let test_name = name.replace(".","_");
            write!(
                f,
                "
    #[test]
    fn {test_name}() {{
        file_test(\"{name}\");
    }}",
                name = name,
                test_name = test_name
            ).unwrap();
        }
    }
}
