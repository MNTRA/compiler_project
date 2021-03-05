use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{
        Parse,
        ParseStream,
    },
    parse_macro_input,
    *,
};

struct StructParserInput {
    ident: Ident,
    func: ItemFn,
}

impl Parse for StructParserInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse::<Ident>()?;
        input.parse::<Token![,]>()?;
        let func = input.parse::<ItemFn>()?;
        Ok(Self {
            ident,
            func,
        })
    }
}


struct ParserInput {
    
}


pub fn create_parser_impl (input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as StructParserInput);

    let struct_name = input.ident;
    let func = input.func;

    let out = quote! {
        struct #struct_name<'a, 'src> {
            inner: crate::parse_stream::ParseStream<'a, 'src> ,
        }
        impl<'a, 'src> #struct_name<'a, 'src>{
            fn new(stream: &'a mut crate::parse_stream::SyntaxTokenParser<'src>) -> Self {
                Self {
                    inner: crate::parse_stream::ParseStream::new(
                        stream
                    )
                }
            }
            #func
        }
        impl<'a, 'src> std::ops::Deref for #struct_name<'a, 'src> {
            type Target = crate::parse_stream::ParseStream<'a, 'src>;
            fn deref(&self) -> &Self::Target {
                &self.inner
            }
        }
        impl<'a, 'src> std::ops::DerefMut for #struct_name<'a, 'src> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.inner
            }
        }
    };
    TokenStream::from(out)
}