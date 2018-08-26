use parser;
use runner;
use std;
use yew::prelude::*;

type Context = ();
pub struct Model {
    value: String,
    program: String,
    res: String,
}

pub enum Msg {
    GotInput(String),
    ClickRun,
}

impl Component<Context> for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, _: &mut Env<Context, Self>) -> Self {
        Model {
            value: include_str!("../tests/modulo.rock").into(),
            program: "Click 'Run program' to see output".into(),
            res: "Click 'Run program' to see output".into(),
        }
    }

    fn update(&mut self, msg: Self::Message, _: &mut Env<Context, Self>) -> ShouldRender {
        match msg {
            Msg::GotInput(new_value) => {
                self.value = new_value;
            }
            Msg::ClickRun => {
                let program = parser::parse(&self.value);
                match program {
                    Err(err) => {
                        self.program = format!("{:?}", err.0);
                    }
                    Ok(val) => {
                        self.program = parser::print_program(&val);
                        let mut writer = std::io::Cursor::new(Vec::new());
                        let res = runner::run(val, &mut writer);
                        self.res = "".into();
                        if let Err(err) = res {
                            self.res += &format!("{:?}", err.0);
                        }
                        writer.set_position(0);
                        self.res += std::str::from_utf8(writer.get_ref()).unwrap().into();
                        js! { @(no_return)
                            $("#outputTabs li#output-tab-li a").tab("show");
                        };
                    }
                }
            }
        }
        true
    }
}

impl Renderable<Context, Model> for Model {
    fn view(&self) -> Html<Context, Self> {
        html! {
            <div class="container-fluid",>
                <div class="row",>
                    <div class="col",>
                        <button type="button",
                            class=("btn", "btn-primary"),
                            onclick=|_| Msg::ClickRun,>{ "Run program" }</button>
                        <textarea id="editor",
                            class="form-control",
                            value=&self.value,
                            oninput=|e| Msg::GotInput(e.value),
                            placeholder="placeholder",>
                        </textarea>
                    </div>
                    <div class="col",>
                        <ul class=("nav", "nav-tabs"), id="outputTabs", role="tablist",>
                            <li class="nav-item",>
                                <a class=("nav-link", "active"),
                                    id="ast-tab", data-toggle="tab",
                                    href="#ast", role="tab",
                                    aria-controls="ast", aria-selected="true",>{ "AST" }</a>
                            </li>
                            <li class="nav-item", id="output-tab-li",>
                                <a class="nav-link", id="output-tab",
                                    data-toggle="tab", href="#output",
                                    role="tab", aria-controls="output",
                                    aria-selected="false",>{ "Output" }</a>
                            </li>
                        </ul>
                        <div class="tab-content", id="outputTabsContent",>
                            <div class=("tab-pane", "fade", "show", "active"),
                                id="ast", role="tabpanel", aria-labelledby="ast-tab",>
                                <pre>{&self.program}</pre>
                            </div>
                            <div class=("tab-pane", "fade"), id="output",
                                role="tabpanel", aria-labelledby="output-tab",>
                                <pre>{&self.res}</pre>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        }
    }
}
