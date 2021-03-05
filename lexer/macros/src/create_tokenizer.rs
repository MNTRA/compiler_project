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

pub(crate) fn create_raw_tokenizer_impl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as CreateTokenHander);

    let struct_name = input.ident;
    let func = input.func;

    let out = quote! {
        struct #struct_name<'a, 'b> {
            inner: crate::raw_token_stream::RawTokenizer<'a, 'b>,
        }
        impl<'a, 'b> #struct_name<'a, 'b>{
            fn new(source: &'b str, mut cursor: &'a mut Cursor<Chars<'b>>) -> Self {
                Self {
                    inner: RawTokenizer {
                        source,
                        length: 0,
                        offset: cursor.offset(),
                        cursor,
                    }
                }
            }
            #func
        }
        impl<'a, 'b> std::ops::Deref for #struct_name<'a, 'b> {
            type Target = RawTokenizer<'a, 'b>;
            fn deref(&self) -> &Self::Target {
                &self.inner
            }
        }
        impl<'a, 'b> std::ops::DerefMut for #struct_name<'a, 'b> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.inner
            }
        }
    };
    TokenStream::from(out)
}

pub(crate) fn create_syntax_tokenizer_impl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as CreateTokenHander);

    let struct_name = input.ident;
    let func = input.func;

    let out = quote! {
        struct #struct_name<'a, 'b> {
            inner: crate::syntax_token_stream::SyntaxTokenizer<'a, 'b>,
        }
        impl<'a, 'b> #struct_name<'a, 'b>{
            fn new(stream: &'a mut SyntaxTokenStream<'b>) -> Self {
                Self {
                    inner: SyntaxTokenizer::new(stream)
                }
            }
            #func
        }
        impl<'a, 'b> std::ops::Deref for #struct_name<'a, 'b> {
            type Target = SyntaxTokenizer<'a, 'b>;
            fn deref(&self) -> &Self::Target {
                &self.inner
            }
        }
        impl<'a, 'b> std::ops::DerefMut for #struct_name<'a, 'b> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.inner
            }
        }
    };
    TokenStream::from(out)
}
