mod create_tokenizer;

use proc_macro::TokenStream;

#[proc_macro]
pub fn create_raw_tokenizer(input: TokenStream) -> TokenStream {
    create_tokenizer::create_raw_tokenizer_impl(input)
}

#[proc_macro]
pub fn create_syntax_tokenizer(input: TokenStream) -> TokenStream {
    create_tokenizer::create_syntax_tokenizer_impl(input)
}
