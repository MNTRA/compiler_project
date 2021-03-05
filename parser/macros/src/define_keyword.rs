use convert_case::{
    Case,
    Casing,
};
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{
        Parse,
        ParseStream,
    },
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    *,
};

pub struct KeywordDefinition {
    token_str: String,
    struct_ident: Ident,
}

struct DefineKeywordInput {
    keywords: Vec<KeywordDefinition>,
}

impl DefineKeywordInput {
    fn check_well_formed(
        string: &str,
        span: impl Spanned,
    ) -> Result<()> {
        if string.len() == 0 {
            return Err(Error::new(span.span(), "Empty strings are not Keywords"));
        } else if string.len() == 1 {
            return Err(Error::new(span.span(), "Keywords with 1 char are Illegal"));
        } else {
            for c in string.chars() {
                if !c.is_ascii() {
                    return Err(Error::new(
                        span.span(),
                        "Keyword tokens must only contain ASCII chars",
                    ));
                } else if c.is_numeric() {
                    return Err(Error::new(
                        span.span(),
                        "Keyword tokens can't contain numbers",
                    ));
                } else if c.is_whitespace() {
                    return Err(Error::new(
                        span.span(),
                        "Keyword tokens can't contain whitespace",
                    ));
                }
            }
            return Ok(());
        }
    }
}

impl Parse for DefineKeywordInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let parser = Punctuated::<LitStr, Token![,]>::parse_terminated;
        let args = parser(input)?;
        let mut keyword_definitions: Vec<KeywordDefinition> = Vec::new();
        for arg in args {
            let token_str = arg.value();
            DefineKeywordInput::check_well_formed(&token_str, &arg)?;
            let token_struct_ident = token_str.to_case(Case::Pascal);
            let struct_ident = parse_str(&token_struct_ident)?;

            keyword_definitions.push(KeywordDefinition {
                token_str,
                struct_ident,
            });
        }
        Ok(DefineKeywordInput {
            keywords: keyword_definitions,
        })
    }
}

pub(crate) fn define_keyword_impl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DefineKeywordInput);
    let mut token_defs = Vec::new();
    for keyword in input.keywords {
        let KeywordDefinition {
            token_str,
            struct_ident,
        } = keyword;

        token_defs.push(quote! {
            #[derive(Debug, Clone, Copy)]
            pub struct #struct_ident;
            impl crate::parser::Parser<()> for #struct_ident {
                type Output = Self;
                fn parse (
                    input: &mut crate::parser::ParseStream,
                    data: ParseData<()>
                ) -> crate::parser::ParseResult<Self::Output> {
                    input.if_next_is (
                        ::lexer::SyntaxTokenType::Identifier,
                        |input, t| {
                            match t.data.src {
                                #token_str => {
                                    input.consume();
                                    Ok(Self)
                                },
                                _ => Err(anyhow::Error::new (
                                        crate::parser::ParseError::TokenDidntMatch
                                    ))
                            }
                        }
                    )
                }
            }
        });
    }
    let out = quote! {
        #(#token_defs)*
    };

    TokenStream::from(out)
}
