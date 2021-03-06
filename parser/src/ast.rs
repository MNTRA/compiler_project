use crate::parsers::common::GlobalScope;

//use crate::grammars::Scope;

#[derive(Debug)]
pub struct Ast {
    global: GlobalScope,
}

impl From<GlobalScope> for Ast {
    fn from(global: GlobalScope) -> Self {
        Self {
            global,
        }
    }
}