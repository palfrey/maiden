#[derive(Debug)]
pub enum Command {
    Assignment { target: String, value: String },
    UntilIs { target: String, value: String, loop_end: Option<usize> },
    Increment { target: String },
    Next { loop_start: usize}
}