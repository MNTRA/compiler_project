mod create_parser;
mod define_keyword;
mod define_punctuation;

use proc_macro::TokenStream;

#[proc_macro]
pub fn create_parser(input: TokenStream) -> TokenStream { create_parser::create_parser_impl(input) }

#[proc_macro]
pub fn define_keyword(input: TokenStream) -> TokenStream {
    define_keyword::define_keyword_impl(input)
}

#[proc_macro]
pub fn define_punctuation(input: TokenStream) -> TokenStream {
    define_punctuation::define_punctuation_impl(input)
}
