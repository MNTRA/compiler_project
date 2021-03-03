use convert_case::{
    Case,
    Casing,
};
use proc_macro::TokenStream;
use quote::{
    quote,
};
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

struct KeywordDefinition {
    token_str: String,
    struct_ident: Ident,
    static_ident: Ident,

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
            let token_str = arg.clone().value();
            DefineKeywordTokenInput::check_well_formed(&token_str, &arg)?;
            let token_struct_ident = token_str.clone().to_case(Case::Pascal);
            let token_static_ident = token_str.clone().to_case(Case::Upper);

            keyword_definitions.push(KeywordDefinition {
                token_str,
                struct_ident: parse_str(&token_struct_ident)?,
                static_ident: parse_str(&token_static_ident)?,
            });
        }
        Ok(DefineKeywordTokenInput {
            keywords: keyword_definitions,
        })
    }
}

pub fn define_keyword_token_impl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DefineKeywordTokenInput);

    let mut token_defs = Vec::new();
    let mut macro_rules_defs = Vec::new();

    for keyword in input.keywords {
        let struct_ident = keyword.struct_ident;
        //let static_ident = keyword.static_ident;
        let token_str = keyword.token_str;

        token_defs.push(quote! {
            pub struct #struct_ident;
            impl Token for #struct_ident {
                const STR: &'static str = #token_str;
            }
        });

        macro_rules_defs.push(quote!{
            [token_str]    => { crate::lexer::token::#struct_ident },
        });
    }

    let main_block = quote! {
        #(#token_defs)*
    };

    let macro_rules_block = quote! {
        macro_rules! Token {
            #(#macro_rules_defs)*
        }
    };

    let out = quote!{
        #main_block
        #macro_rules_block
    };

    TokenStream::from(out)
}
