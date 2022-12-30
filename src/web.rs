use crate::common::MaidenError;
use crate::display;
use crate::parser;
use crate::runner;
use js_sys::Function;
use std;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsCast;
use wasm_bindgen::JsValue;
use yew::html;
use yew::prelude::*;

pub struct Model {
    value: String,
    program: String,
    parse_error: bool,
    res: String,
    run_error: bool,
    interval: Option<i32>,
    _setup_fn: Closure<dyn FnMut()>,
    change_fn: Option<Closure<dyn FnMut(JsValue, JsValue)>>,
}

pub enum Msg {
    GotInput(String),
    CodeMirrorSetup,
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
        if line == 0 {
            format!("{}", err)
        } else {
            format!(
                "{} at line {}: \"{}\"",
                err,
                line,
                self.get_line(line as usize)
            )
        }
    }

    fn ast_tab(&self) -> Html {
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
                self.program = display::print_program(&val);
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

    fn create(ctx: &Context<Self>) -> Self {
        web_sys::console::log_1(&"Create".into());
        let window = web_sys::window().unwrap();
        let link = ctx.link().clone();
        let setup_fn = Closure::wrap(
            Box::new(move || link.send_message(Msg::CodeMirrorSetup)) as Box<dyn FnMut()>
        );
        let handle = window
            .set_interval_with_callback_and_timeout_and_arguments_0(
                setup_fn.as_ref().unchecked_ref(),
                500,
            )
            .unwrap();
        let mut res = Self {
            value: include_str!("../tests/local/modulo.rock").into(),
            program: "".into(),
            parse_error: false,
            res: "".into(),
            run_error: false,
            interval: Some(handle),
            _setup_fn: setup_fn,
            change_fn: None,
        };
        res.run_program();
        web_sys::console::log_1(&"end Create".into());
        res
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::GotInput(input_data) => {
                web_sys::console::log_1(&"Change".into());
                self.value = input_data;
                self.run_program();
                true
            }
            Msg::CodeMirrorSetup => {
                web_sys::console::log_1(&"CodeMirrorSetup".into());
                let window = web_sys::window().unwrap();
                if let Ok(code_mirror) = js_sys::Reflect::get(&window, &"codeMirror".into()) {
                    if !js_sys::Reflect::has(&code_mirror, &"configured".into()).unwrap() {
                        web_sys::console::log_1(&"start setup callback".into());

                        let local_link = ctx.link().clone();
                        let change_fn =
                            Closure::wrap(Box::new(move |cm: JsValue, _change: JsValue| {
                                let get_value = Function::from(
                                    js_sys::Reflect::get(&cm, &"getValue".into()).unwrap(),
                                );
                                local_link.send_message(Msg::GotInput(
                                    get_value.call0(&cm).unwrap().as_string().unwrap(),
                                ));
                            })
                                as Box<dyn FnMut(JsValue, JsValue)>);

                        self.change_fn.replace(change_fn);

                        self.change_fn.as_ref().and_then(|cf| {
                            Function::from(
                                js_sys::Reflect::get(&code_mirror, &"on".into()).unwrap(),
                            )
                            .call2(&code_mirror, &"change".into(), &cf.as_ref().unchecked_ref())
                            .unwrap();
                            None::<u32>
                        });
                        web_sys::console::log_1(&"setup callback".into());
                        let set_value = Function::from(
                            js_sys::Reflect::get(&code_mirror, &"setValue".into()).unwrap(),
                        );
                        web_sys::console::log_1(&"got setValue".into());
                        set_value
                            .call1(&code_mirror, &self.value.clone().into())
                            .unwrap();
                        web_sys::console::log_1(&"ran setValue".into());
                        js_sys::Reflect::set(&code_mirror, &"configured".into(), &JsValue::TRUE)
                            .unwrap();
                        web_sys::console::log_1(&"end setup callback".into());
                        let taken_interval = self.interval.take();
                        if let Some(interval) = taken_interval {
                            let window = web_sys::window().unwrap();
                            web_sys::console::log_1(&format!("interval {}", interval).into());
                            window.clear_interval_with_handle(interval);
                        }
                    }
                }
                true
            }
        }
    }

    fn view(&self, _ctx: &Context<Self>) -> Html {
        html! {
            <div class="row">
                <div class="col-xl-6">
                    <textarea id="editor"
                        class="form-control"
                        value={self.value.clone()}
                        placeholder="placeholder">
                    </textarea>
                </div>
                <div class="col-xl-6">
                    <ul class="nav nav-tabs" id="outputTabs" role="tablist">
                        <li class="nav-item">
                            <a class="nav-link active"
                                id="ast-tab" data-toggle="tab"
                                href="#ast" role="tab"
                                style={if self.parse_error {"color: red"} else {"color: green"}}
                                aria-controls="ast" aria-selected="true">{ "AST" }</a>
                        </li>
                        <li class="nav-item"
                            id="output-tab-li"
                            style={if self.parse_error {"display: none"} else {""}}>
                            <a class="nav-link" id="output-tab"
                                data-toggle="tab" href="#output"
                                role="tab" aria-controls="output"
                                style={if self.run_error {"color: red"} else {"color: green"}}
                                aria-selected="false">{ "Output" }</a>
                        </li>
                    </ul>
                    <div class="tab-content" id="outputTabsContent">
                        <div class="tab-pane fade show active"
                            id="ast" role="tabpanel" aria-labelledby="ast-tab">
                            {self.ast_tab()}
                        </div>
                        <div class="tab-pane fade"
                            id="output"
                            style={if self.parse_error {"display: none"} else {""}}
                            role="tabpanel" aria-labelledby="output-tab">
                            <pre>{&self.res}</pre>
                        </div>
                    </div>
                </div>
            </div>
        }
    }
}

fn get_error_line(e: &MaidenError) -> usize {
    match e {
        MaidenError::MissingVariable { ref line, .. }
        | MaidenError::MissingFunction { ref line, .. }
        | MaidenError::WrongArgCount { ref line, .. }
        | MaidenError::ParseNumberError { ref line, .. }
        | MaidenError::NoEndOfIf { ref line }
        | MaidenError::BadBooleanResolve { ref line, .. }
        | MaidenError::Unimplemented { ref line, .. }
        | MaidenError::StackOverflow { ref line, .. }
        | MaidenError::InstructionLimit { ref line }
        | MaidenError::UndefinedPronoun { ref line }
        | MaidenError::Infinity { ref line, .. }
        | MaidenError::Incomplete { ref line, .. }
        | MaidenError::NotAnExpression { ref line, .. }
        | MaidenError::NotASymbol { ref line, .. }
        | MaidenError::NotACommand { ref line, .. }
        | MaidenError::NotABlock { ref line, .. }
        | MaidenError::BadString { ref line, .. } => *line,
        MaidenError::Pest { .. } | MaidenError::Io { .. } => 0,
    }
}
