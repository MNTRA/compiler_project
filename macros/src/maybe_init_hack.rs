// use proc_macro::TokenStream;
// use proc_macro2::Literal;
// use quote::{
//     quote,
// };
// use syn::{
//     parse::{
//         Parse,
//         ParseStream,
//     },
//     parse_macro_input,
//     punctuated::Punctuated,
//     spanned::Spanned,
//     *,
// };

// // struct MaybeInitHackInput{
//     _count: LitInt,
// }

// impl Parse for MaybeInitHackInput {
//     fn parse(input: ParseStream) -> Result<Self> {
//         let count = input.parse::<LitInt>()?;
//         Ok(MaybeInitHackInput{
//             count
//         })
//     }
// }

// pub fn maybe_init_hack_impl(input: TokenStream) -> TokenStream {
//     let _input = parse_macro_input!(input as MaybeInitHackInput);
//     todo!()
// }