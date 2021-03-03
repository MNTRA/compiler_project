mod create_tokenizer;
mod define_keyword_token;

use proc_macro::TokenStream;

#[proc_macro]
pub fn define_keyword_token (input: TokenStream) -> TokenStream {
    define_keyword_token::define_keyword_token_impl(input)
}

#[proc_macro]
pub fn create_raw_tokenizer (input: TokenStream) -> TokenStream {
    create_tokenizer::create_raw_tokenizer_impl(input)
}

#[proc_macro]
pub fn create_syntax_tokenizer (input: TokenStream) -> TokenStream {
    create_tokenizer::create_syntax_tokenizer_impl(input)
}