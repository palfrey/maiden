use yew::prelude::*;
use parser;
use std;
use runner;

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
            program: "".into(),
            res: "".into(),
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
                        <textarea class="form-control",
                            rows=20,
                            value=&self.value,
                            oninput=|e| Msg::GotInput(e.value),
                            placeholder="placeholder",>
                        </textarea>
                        <button type="button",
                            class=("btn", "btn-primary"),
                            onclick=|_| Msg::ClickRun,>{ "Run program" }</button>
                    </div>
                    <div class="col",>
                        <pre>{&self.program}</pre>
                        <pre>{&self.res}</pre>
                    </div>
                </div>
            </div>
        }
    }
}
