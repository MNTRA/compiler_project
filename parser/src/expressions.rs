use ast::Block;
use diagnostics::{
    DiagnosticBuilder,
    Level,
};
use lexer::SyntaxToken;
use tokio::task;

use crate::parser::{
    handle_unexpected_token,
    EnclosedKind,
    Parser,
    PT,
    ST,
};

/// Everything should be an expression

impl<'src> Parser<'src> {
    pub fn expect_block(&mut self) -> Result<Block, DiagnosticBuilder> {
        self.expect_punctuation(PT::LBrace)
            .map_err(handle_unexpected_token)?;

        // Checks if the braces are empty, aka: {}
        // In this case we dont need to spawn a task to parse the inner
        // statements/expressions.
        let out = if self.current_token().ty != ST::Punctuation(PT::RBrace) {
            Some(task::block_in_place(|| {
                
                
                
                Block::default()
            
            
            }))
        } else {
            None
        }
        .unwrap_or_default();

        self.expect_punctuation(PT::RBrace)
            .map_err(handle_unexpected_token)?;
        Ok(out)
    }

    pub fn parse_statement(&mut self) -> Result<(), DiagnosticBuilder> {

        


        todo!()
    }
}




pub enum Statement {
    Let
}