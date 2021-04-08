use diagnostics::span::Span;

#[derive(Debug, Default)]
pub struct Module {
    functions: Vec<(Visibility, FnItem)>
}

impl Module {
    pub fn add_function(&mut self, func: (Visibility, FnItem)) {
        self.functions.push(func)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

impl Visibility {
    pub fn is_public(&self) -> bool {
        *self == Visibility::Public
    }
}

#[derive(Debug)]
pub struct Ident{
    pub value: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct Type {
    pub reference: Option<Reference>,
    pub ident: Ident,
    pub span: Span,
}

#[derive(Debug)]
pub enum TypedIdent {
    Typed {
        mutability: Mutability,
        ident: Ident,
        ty: Type
    },
    UnTyped {
        mutability: Mutability,
        ident: Ident,
    }
}

#[derive(Debug)]
pub enum Reference {
    Shared(Span),
    Exclusive{
        ref_span: Span,
        mut_span: Span,
    },
}

#[derive(Debug)]
pub enum Mutability {
    Mutable(Span),
    Immutable,
}

#[derive(Debug)]
pub struct FnSig {
    pub ident: Ident,
    pub params: Vec<TypedIdent>,
    pub ret_type: Option<Type>,
}

#[derive(Debug)]
pub struct FnItem {
    pub sig: FnSig,
    pub block: Block
}

#[derive(Default, Debug)]
pub struct Block {
    stmts: Vec<()>,
    expr: ()
}