use quote::quote;
use syn::{
    parse_macro_input,
    Token,
    parse::{Parse, ParseStream },
    Ident,
    Result,
    ItemFn
};
use proc_macro::TokenStream;

struct CreateTokenHander {
    ident: Ident,
    func: ItemFn
}

impl Parse for CreateTokenHander {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse::<Ident>()?;
        input.parse::<Token![,]>()?;
        let func = input.parse::<ItemFn>()?;
        Ok(Self {
            ident,
            func
        })
    }
}

#[proc_macro]
pub fn create_token_handler(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as CreateTokenHander);

    let struct_name = input.ident;
    let func = input.func;

    let out = quote!{
        pub struct #struct_name<'a, 'b> {
            inner: crate::parser::syntax_pass::SyntaxTokenizer<'a, 'b>
        }
        impl<'a, 'b> #struct_name<'a, 'b> {
            #func
        }
        impl<'a, 'b> ::std::ops::Deref for #struct_name<'a, 'b> {
            type Target = SyntaxPassIter<'a>;
            fn deref(&self) -> &Self::Target { &self.inner }
        }
        
        impl<'a, 'b> ::std::ops::DerefMut for #struct_name<'a, 'b> {
            fn deref_mut(&mut self) -> &mut Self::Target { &mut self.inner }
        }
    };
    TokenStream::from(out)
}