use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{
        Parse,
        ParseStream,
    },
    parse_macro_input,
    punctuated::Punctuated,
    *,
};

pub struct PuncDef {
    ident: Ident,
}

impl Parse for PuncDef {
    fn parse(input: ParseStream) -> Result<Self> {
        let inner;
        let _ = bracketed!(inner in input);
        Ok(Self {
            ident: inner.parse::<Ident>()?,
        })
    }
}

struct PuncDefInput {
    keywords: Vec<PuncDef>,
}

impl Parse for PuncDefInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let parser = Punctuated::<PuncDef, Token![,]>::parse_terminated;
        let args = parser(input)?;
        let mut defs = Vec::new();
        for arg in args {
            defs.push(arg);
        }
        Ok(PuncDefInput {
            keywords: defs,
        })
    }
}

pub(crate) fn define_punctuation_impl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as PuncDefInput);
    let mut token_defs = Vec::new();
    for keyword in input.keywords {
        let PuncDef {
            ident,
        } = keyword;

        token_defs.push(quote! {
            #[derive(Debug, Clone, Copy)]
            pub struct #ident;
            impl crate::parser::Parser<()> for #ident {
                type Output = Self;
                fn parse (
                    input: &mut crate::parser::ParseStream,
                    data: ParseData<()>
                ) -> crate::parser::ParseResult<Self::Output> {
                    type ST = ::lexer::SyntaxTokenType;
                    type PT = ::lexer::PunctuationType;
                    let token = input.get_next_token()?;
                    match token.ty {
                        ST::Punctuation(PT::#ident) => {
                            input.consume();
                            // TODO (George): Create Store of Idents to reduce allocs
                            Ok(#ident)
                        },
                        _ => {
                            Err(anyhow::Error::new(
                                crate::parser::ParseError::TokenDidntMatch
                            ))
                        }
                    }
                }
            }
        });
    }
    let out = quote! {
        #(#token_defs)*
    };

    TokenStream::from(out)
}
