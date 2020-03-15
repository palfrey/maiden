use crate::common::{Command, MaidenError, Program};
use crate::parser;
use crate::runner;
use std;
use stdweb::js;
use yew::html;
use yew::prelude::*;
use yew::services::ConsoleService;

pub struct Model {
    value: String,
    program: String,
    parse_error: bool,
    res: String,
    run_error: bool,
    input_callback: Callback<String>,
    console: ConsoleService,
}

pub enum Msg {
    GotInput(String),
}

impl Model {
    fn get_line(&self, line: usize) -> &str {
        if line == 0 {
            &self.value
        } else {
            self.value.split("\n").nth(line - 1).unwrap()
        }
    }

    fn nicer_error(&self, err: &MaidenError) -> String {
        let line = get_error_line(err);
        format!(
            "{} at line {}: \"{}\"",
            err,
            line,
            self.get_line(line as usize)
        )
    }

    fn ast_tab(&self) -> Html<Self> {
        if self.parse_error {
            html! {
                { &self.program }
            }
        } else {
            html! {
                <pre> { &self.program } </pre>
            }
        }
    }

    fn run_program(&mut self) {
        let program = parser::parse(&self.value);
        match program {
            Err(err) => {
                self.program = self.nicer_error(&err);
                self.parse_error = true;
                self.res = "".to_string()
            }
            Ok(mut val) => {
                self.program = print_program(&val);
                self.parse_error = false;
                let mut writer = std::io::Cursor::new(Vec::new());
                let res = runner::run(&mut val, &mut writer);
                self.res = "".into();
                if let Err(err) = res {
                    self.res += &self.nicer_error(&err);
                    self.run_error = true;
                } else {
                    self.run_error = false;
                }
                writer.set_position(0);
                self.res += std::str::from_utf8(writer.get_ref()).unwrap().into();
                if self.res.is_empty() {
                    self.res = "<No output from program>".to_string();
                }
            }
        }
    }
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, mut link: ComponentLink<Self>) -> Self {
        let mut res = Self {
            value: include_str!("../tests/local/modulo.rock").into(),
            program: "".into(),
            parse_error: false,
            res: "".into(),
            run_error: false,
            input_callback: link.send_back(|data| Msg::GotInput(data)),
            console: ConsoleService::new(),
        };
        res.run_program();
        res
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::GotInput(input_data) => {
                self.console.log("Change");
                self.value = input_data;
                self.run_program();
                true
            }
        }
    }
}

impl Renderable<Model> for Model {
    fn view(&self) -> Html<Self> {
        let callback = self.input_callback.clone();
        let do_input = move |payload: String| callback.emit(payload);
        js! {
            function codeMirrorCallback() {
                if (window.codeMirror) {
                    if (!window.codeMirror.configured) {
                        window.codeMirror.on("change", function(cm, change) {
                            var callback = @{do_input};
                            callback(cm.getValue());
                        });
                        window.codeMirror.setValue(@{&self.value});
                        console.log("setup callback");
                        window.codeMirror.configured = true;
                    }
                }
                else {
                    window.setTimeout(codeMirrorCallback, 500);
                }
            }
            codeMirrorCallback();
        }
        html! {
            <div class="row",>
                <div class="col-xl-6",>
                    <textarea id="editor",
                        class="form-control",
                        value=&self.value,
                        placeholder="placeholder",>
                    </textarea>
                </div>
                <div class="col-xl-6",>
                    <ul class=("nav", "nav-tabs"), id="outputTabs", role="tablist",>
                        <li class="nav-item",>
                            <a class=("nav-link", "active"),
                                id="ast-tab", data-toggle="tab",
                                href="#ast", role="tab",
                                style=if self.parse_error {"color: red"} else {"color: green"},
                                aria-controls="ast", aria-selected="true",>{ "AST" }</a>
                        </li>
                        <li class="nav-item",
                            id="output-tab-li",
                            style=if self.parse_error {"display: none"} else {""},>
                            <a class="nav-link", id="output-tab",
                                data-toggle="tab", href="#output",
                                role="tab", aria-controls="output",
                                style=if self.run_error {"color: red"} else {"color: green"},
                                aria-selected="false",>{ "Output" }</a>
                        </li>
                    </ul>
                    <div class="tab-content", id="outputTabsContent",>
                        <div class=("tab-pane", "fade", "show", "active"),
                            id="ast", role="tabpanel", aria-labelledby="ast-tab",>
                            {self.ast_tab()}
                        </div>
                        <div class=("tab-pane", "fade"),
                            id="output",
                            style=if self.parse_error {"display: none"} else {""},
                            role="tabpanel", aria-labelledby="output-tab",>
                            <pre>{&self.res}</pre>
                        </div>
                    </div>
                </div>
            </div>
        }
    }
}

fn print_command(command: &Command) -> String {
    format!("{:?}", command)
}

pub fn print_program(program: &Program) -> String {
    let mut res = String::new();
    let mut indent = 0;
    let mut last_line = 0;
    let max_line: f32 = (program.commands.iter().fold(0, |acc, x| acc.max(x.line))) as f32;
    let max_number_length: usize = (max_line + 1.0).log10().ceil() as usize;
    for command in &program.commands {
        // match command.cmd {
        //     Command::Else { .. } | Command::EndIf  => {
        //         indent -= 1;
        //     }
        //     _ => {}
        // }
        while last_line < command.line - 1 {
            last_line += 1;
            res += &format!("{:0width$}:\n", last_line, width = max_number_length);
        }
        last_line = command.line;
        res += &format!("{:0width$}: ", command.line, width = max_number_length);
        for _ in 0..indent {
            res += "  ";
        }
        res += &(print_command(&command.cmd) + "\n");
        match command.cmd {
            Command::FunctionDeclaration { .. }
            | Command::If { .. }
            | Command::While { .. }
            | Command::Until { .. } => {
                indent += 1;
            }
            _ => {}
        }
    }
    return res;
}

fn get_error_line(e: &MaidenError) -> usize {
    match e {
        MaidenError::MissingVariable { line, .. } => line.clone(),
        MaidenError::UnparsedText { line, .. } => line.clone(),
        MaidenError::MissingFunction { line, .. } => line.clone(),
        MaidenError::WrongArgCount { line, .. } => line.clone(),
        MaidenError::UnbalancedExpression { line, .. } => line.clone(),
        MaidenError::BadCommandSequence { line, .. } => line.clone(),
        MaidenError::ParseNumberError { line, .. } => line.clone(),
        MaidenError::BadIs { line, .. } => line.clone(),
        MaidenError::BadPut { line, .. } => line.clone(),
        MaidenError::NoEndOfIf { line } => line.clone(),
        MaidenError::ElseWithNoIf { line } => line.clone(),
        MaidenError::MultipleElse { line } => line.clone(),
        MaidenError::NoEndFunction { line } => line.clone(),
        MaidenError::NoEndLoop { line } => line.clone(),
        MaidenError::BadBooleanResolve { line, .. } => line.clone(),
        MaidenError::Unimplemented { line, .. } => line.clone(),
        MaidenError::StackOverflow { line, .. } => line.clone(),
        MaidenError::InstructionLimit { line } => line.clone(),
        MaidenError::UndefinedPronoun { line } => line.clone(),
        MaidenError::Infinity { line, .. } => line.clone(),
        _ => 0,
    }
}
