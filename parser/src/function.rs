use ast::{
    Block,
    FnItem,
    FnSig,
    Ident,
    Type,
    TypedIdent,
};
use diagnostics::{
    DiagnosticBuilder,
    Level,
};

use crate::{
    module::ModuleItem,
    parser::{
        handle_unexpected_token,
        BraceChecker,
        Parser,
        KT,
        PT,
        ST,
    },
};

impl<'src> Parser<'src> {
    #[inline]
    pub fn parse_function_item(&mut self) -> Result<FnItem, DiagnosticBuilder> {
        let sig = self.parse_fn_sig()?;
        let block = self.expect_block()?;
        Ok(FnItem {
            sig,
            block,
        })
    }

    #[inline]
    pub fn parse_fn_sig(&mut self) -> Result<FnSig, DiagnosticBuilder> {
        // eat the 'fn' token
        self.next_token(true);

        // Function Name
        let ident = self.expect_ident().map_err(|_| {
            DiagnosticBuilder::default()
                .with_level(Level::Error)
                .with_label(format!("Expected identifier"))
                .with_marked_span(self.current_token().span())
        })?;

        // Function Parameters
        let params = self.parse_fn_params()?;

        // Function Return Type
        let ret_type = if self.current_token().ty == ST::Punctuation(PT::Hyphen) {
            let t = self
                .expect_slice(&[
                    ST::Punctuation(PT::Hyphen),
                    ST::Punctuation(PT::RAngleBracket),
                ])
                .map_err(|ty| match ty {
                    ST::Punctuation(PT::Hyphen) => DiagnosticBuilder::default()
                        .with_level(Level::Error)
                        .with_label(format!(
                            "Expected '->', found {:?}",
                            self.current_token().ty
                        ))
                        .with_marked_span(self.current_token().span()),
                    ST::Punctuation(PT::RAngleBracket) => DiagnosticBuilder::default()
                        .with_level(Level::Error)
                        .with_label(format!("Missing '>'"))
                        .with_marked_span(self.current_token().span()),
                    _ => unreachable!(),
                })
                .and_then(|_| self.expect_type())?;
            Some(t)
        } else {
            None
        };

        Ok(FnSig {
            ident,
            params,
            ret_type,
        })
    }

    fn parse_fn_params(&mut self) -> Result<Vec<TypedIdent>, DiagnosticBuilder> {
        // eat the '(' token
        self.expect_punctuation(PT::LParen)
            .map_err(handle_unexpected_token)?;
        self.brace_checker().inc_parens();

        let mut parameters = vec![];
        let mut expects_param = false;
        // do-while hack from https://gist.github.com/huonw/8435502
        while {
            if self.expect_punctuation(PT::RParen).is_ok() && !expects_param {
                false
            } else {
                // ident : Type
                let mutability = self.parse_mutability();
                let ident = self.expect_ident().map_err(handle_unexpected_token)?;

                self.expect_punctuation(PT::Colon)
                    .map_err(handle_unexpected_token)?;

                let ty = self.expect_type()?;
                parameters.push(TypedIdent::Typed {
                    mutability,
                    ident,
                    ty,
                });
                true
            }
        } {
            if self.expect_punctuation(PT::RParen).is_ok() {
                break;
            }
            let _comma_token = self.expect_punctuation(PT::Comma)
                .map_err(handle_unexpected_token)?;
            expects_param = true;
                
            match self.current_token().ty {
                ST::Identifier => continue,
                ST::Punctuation(PT::RParen) => {
                    return Err(DiagnosticBuilder::default()
                        .with_level(Level::Error)
                        .with_label("trailing commas are not allowed")
                        .with_hint("remove the trailing ','")
                        .with_marked_span(self.current_token().span())
                    );
                }
                _ => {
                    return Err(handle_unexpected_token(
                        (ST::Identifier, self.current_token()))
                    );
                }
            }
        }

        Ok(parameters)
    }
}
