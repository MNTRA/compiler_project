use ast::{
    Block,
    FnItem,
    FnSig,
    Visibility,
};
use diagnostics::{
    DiagnosticBuilder,
    Level,
};
use edit_distance::edit_distance;

use crate::parser::{
    Parser,
    KT,
    ST,
};

pub const LEGAL_MODULE_KEYWORDS: &[KT] = &[KT::Pub, KT::Fn, KT::Static];

#[derive(Debug)]
pub enum ModuleItem {
    Function { vis: Visibility, fn_item: FnItem },
}

impl ModuleItem {
    pub fn from_fn_item(
        vis: Visibility,
        fn_item: FnItem,
    ) -> Self {
        Self::Function {
            vis,
            fn_item,
        }
    }
}

impl<'src> Parser<'src> {
    #[inline]
    pub fn parse_module_item(&mut self) -> Result<ModuleItem, DiagnosticBuilder> {
        let mut vis = Visibility::Private;
        loop {
            match self.current_token().ty {
                ST::Keyword(KT::Pub) => {
                    if vis.is_public() {
                        return Err(DiagnosticBuilder::default()
                            .with_level(Level::Error)
                            .with_label("can't use 'pub' here")
                            .with_hint("Use a valid keyword")
                            .with_marked_span(self.current_token().span()));
                        }
                        vis = self.parse_visibility();
                        continue;
                    },
                    ST::Keyword(KT::Fn) => {
                        return Ok(ModuleItem::from_fn_item(vis, self.parse_function_item()?));
                    },
                    ST::Identifier => {
                        let mut diagnostic = DiagnosticBuilder::default()
                            .with_level(Level::Error)
                            .with_label("Expected Keyword")
                            .with_marked_span(self.current_token().span());

                    let ident = self.get_current_token_str();
                    if let Some(keyword) = try_predict_keyword(ident) {
                        diagnostic
                            .set_hint(format!("Did you mean '{}'?", keyword.as_str()));
                    }

                    return Err(diagnostic);
                },
                _ => {
                    return Err(DiagnosticBuilder::default()
                        .with_level(Level::Error)
                        .with_label("can't use 'pub' here")
                        .with_hint("Use a valid keyword"));
                },
            }
        }
    }

    #[inline]
    fn parse_visibility(&mut self) -> Visibility {
        if self.current_token().ty == ST::Keyword(KT::Pub) {
            self.next_token(true);
            Visibility::Public
        } else {
            Visibility::Private
        }
    }
}

fn try_predict_keyword(ident: &str) -> Option<KT> {
    let mut predicted_keyword = None;
    let mut lowest_lev_dist = usize::MAX;
    for legal_keyword in LEGAL_MODULE_KEYWORDS {
        let current_lev_dist = edit_distance(ident, legal_keyword.as_str());
        if current_lev_dist < lowest_lev_dist {
            lowest_lev_dist = current_lev_dist;
            predicted_keyword = Some(*legal_keyword);
        }
    }
    if lowest_lev_dist > 3 {
        None
    } else {
        predicted_keyword
    }
}
