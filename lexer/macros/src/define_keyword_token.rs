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

struct DefineKeywordTokenInput {
    keywords: Vec<KeywordDefinition>,
}

impl DefineKeywordTokenInput {
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

impl Parse for DefineKeywordTokenInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let parser = Punctuated::<LitStr, Token![,]>::parse_terminated;
        let args = parser(input)?;
        let mut keyword_definitions: Vec<KeywordDefinition> = Vec::new();
        for arg in args {
            let token_str = arg.value();
            DefineKeywordTokenInput::check_well_formed(&token_str, &arg)?;
            let token_struct_ident = token_str.to_case(Case::Pascal);
            let struct_ident = parse_str(&token_struct_ident)?;

            keyword_definitions.push(KeywordDefinition {
                token_str,
                struct_ident,
            });
        }
        Ok(DefineKeywordTokenInput {
            keywords: keyword_definitions,
        })
    }
}

pub(crate) fn define_keyword_token_impl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DefineKeywordTokenInput);
    let mut token_defs = Vec::new();
    for keyword in input.keywords {
        let KeywordDefinition {
            token_str,
            struct_ident,
        } = keyword;

        token_defs.push(quote! {
            pub struct #struct_ident;
            impl Token for #struct_ident {
                const NAME: &'static str = #token_str;
            }
        });
    }

    let main_block = quote! {
        #(#token_defs)*
    };

    let out = quote! {
        #main_block
    };

    TokenStream::from(out)
}
