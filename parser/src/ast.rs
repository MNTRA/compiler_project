use crate::parsers::common::GlobalScope;

//use crate::grammars::Scope;

#[derive(Debug)]
pub struct Ast {
    scope: GlobalScope,
}

impl From<GlobalScope> for Ast {
    fn from(scope: GlobalScope) -> Self {
        Self {
            scope,
        }
    }
}
