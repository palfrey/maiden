use common;
use parser;
use runner;
use std;
use yew::prelude::*;

pub struct Model {
    value: String,
    program: String,
    parse_error: bool,
    res: String,
    run_error: bool,
    link: ComponentLink<Model>,
}

pub enum Msg {
    GotInput(String),
}

fn nicer_error(err: &common::Error, full_text: &str) -> String {
    let line = common::get_error_line(err) as usize;
    let line_display = if line == 0 {
        &full_text
    } else {
        full_text.split("\n").nth(line - 1).unwrap()
    };
    format!(
        "{} at line {}: \"{}\"",
        err.0,
        line,
        line_display
    )
}

impl Model {
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
        let program = parser::parse(&self.value.clone());
        match program {
            Err(err) => {
                self.program = nicer_error(&err, &self.value);
                self.parse_error = true;
                self.res = "".to_string()
            }
            Ok(val) => {
                self.program = parser::print_program(&val);
                self.parse_error = false;
                self.res = "".into();
                let res = std::panic::catch_unwind(|| {
                    let mut writer = std::io::Cursor::new(Vec::new());
                    let res = runner::run(&val, &mut writer);
                    let mut display_result = "".to_string();
                    let run_error;
                    if let Err(err) = res {
                        display_result += &nicer_error(&err, "");
                        run_error = true;
                    } else {
                        run_error = false;
                    }
                    writer.set_position(0);
                    display_result += std::str::from_utf8(writer.get_ref()).unwrap().into();
                    if display_result.is_empty() {
                        display_result = "<No output from program>".to_string();
                    }
                    (display_result, run_error)
                });
                match res {
                    Ok((display_result, run_error)) => {
                        self.res = display_result;
                        self.run_error = run_error;
                    }
                    Err(err) => {
                        self.res += &format!("{:?}", err);
                        self.run_error = true;
                    }
                };
            }
        }
    }
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, link: ComponentLink<Self>) -> Self {
        let mut res = Self {
            value: include_str!("../tests/modulo.rock").into(),
            program: "".into(),
            parse_error: false,
            res: "".into(),
            run_error: false,
            link: link,
        };
        res.run_program();
        res
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::GotInput(input_data) => {
                println!("Change");
                self.value = input_data;
                self.run_program();
                true
            }
        }
    }
}

impl Renderable<Model> for Model {
    fn view(&self) -> Html<Self> {
        let input_callback = self.link.send_back(Msg::GotInput);
        let do_input = move |payload: String| input_callback.emit(payload);
        js! {
            function codeMirrorCallback() {
                if (window.codeMirror) {
                    if (!window.codeMirror.configured) {
                        window.codeMirror.on("change", function(cm, change) {
                            var callback = @{do_input};
                            callback(cm.getValue());
                        });
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
