mod token;
mod maybe_init_hack;

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

struct CreateTokenHander {
    ident: Ident,
    func: ItemFn,
}

impl Parse for CreateTokenHander {
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

#[proc_macro]
pub fn create_token_handler(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as CreateTokenHander);

    let struct_name = input.ident;
    let func = input.func;

    let out = quote! {
        pub struct #struct_name<'a, 'b> {
            pub(self) inner: &'b mut crate::parser::syntax_pass::SyntaxTokenizer<'a, 'b>
        }
        impl<'a, 'b> #struct_name<'a, 'b> {
            #func
        }
        impl<'a, 'b> ::std::ops::Deref for #struct_name<'a, 'b> {
            type Target = &'b mut SyntaxTokenizer<'a, 'b>;
            fn deref(&self) -> &Self::Target { &self.inner }
        }

        impl<'a, 'b> ::std::ops::DerefMut for #struct_name<'a, 'b> {
            fn deref_mut(&mut self) -> &mut Self::Target { &mut self.inner }
        }
    };
    TokenStream::from(out)
}

struct CreateSyntaxToken {
    ty: Type,
    variant: Variant,
}

impl Parse for CreateSyntaxToken {
    fn parse(input: ParseStream) -> Result<Self> {
        let variant = input.parse::<Variant>()?;
        Ok(Self {
            ty: parse_str("crate::parser::SyntaxTokenType")?,
            variant,
        })
    }
}

#[proc_macro]
pub fn create_syntax_token(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as CreateSyntaxToken);
    let ty = input.ty;
    let variant = input.variant;

    let out = quote! {
        {
            self.create_and_buffer_syntax_token(
                1,
                #ty::#variant
            );
            return;
        }
    };

    TokenStream::from(out)
}

#[proc_macro]
pub fn define_keyword_token(input: TokenStream) -> TokenStream {
    crate::token::define_keyword_token_impl(input)
}

// #[proc_macro]
// pub fn maybe_init_hack (input: TokenStream) -> TokenStream {
//     crate::maybe_init_hack::maybe_init_hack_impl(input)
// }
