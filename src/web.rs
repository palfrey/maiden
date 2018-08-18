use yew::prelude::*;
use parser;

type Context = ();
pub struct Model {
    value: String,
    program: String,
}

pub enum Msg {
    GotInput(String),
    ClickRun,
}

impl Component<Context> for Model {
    // Some details omitted. Explore the examples to get more.

    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, _: &mut Env<Context, Self>) -> Self {
        Model {
            value: "".into(),
            program: "".into()
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
            <div>
                <div>
                    <textarea rows=5,
                        value=&self.value,
                        oninput=|e| Msg::GotInput(e.value),
                        placeholder="placeholder",>
                    </textarea>
                     <button onclick=|_| Msg::ClickRun,>{ "run program" }</button>
                </div>
                <div>
                    <pre>{&self.program}</pre>
                </div>
            </div>
        }
    }
}