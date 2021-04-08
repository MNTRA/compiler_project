use std::{
    panic::{
        catch_unwind,
        panic_any,
        resume_unwind,
        AssertUnwindSafe,
        PanicInfo,
    },
    path::Path,
    rc::Rc,
    sync::Arc,
};

use diagnostics::{
    print_diagnostics,
};
use errors::{
    CompilerBug,
    FatalError,
};
use async_scoped::TokioScope;
use tokio::{
    fs::read_to_string,
    task::{
        self,
        JoinHandle,
    },
};

#[tokio::main]
pub async fn compiler_main(data: CompilerInfo) {
    let parse_info = parser::ParseInfoBuilder::new()
        .source(&data.source)
        .build();

    let out = TokioScope::scope_and_block(|scope| {
        scope.spawn(parser::parse_root(parse_info));
    });

    print_diagnostics(&data.source.clone());
}

pub struct CompilerInfo {
    pub source: String,
    pub file_path: Arc<Path>,
}

type PanicHook = Box<dyn Fn(&PanicInfo<'_>) + 'static + Sync + Send>;

fn catch_fatal_errors<F, O>(func: F) -> Result<O, FatalError>
where
    F: FnOnce() -> O,
{
    catch_unwind(AssertUnwindSafe(func)).map_err(|err| {
        if err.is::<FatalError>() {
            return FatalError;
        }
        if let Ok(bug) = err.downcast::<CompilerBug>() {
            panic_any(bug.description());
        }
        todo!()
        // resume_unwind(err)
    })
}
