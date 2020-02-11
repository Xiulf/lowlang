use crate::{FunctionCtx, Backend};

impl<'a, 't, 'l, B: Backend> FunctionCtx<'a, 't, 'l, B> {
    pub fn trans_stmt(&mut self, stmt: &syntax::Stmt<'t>) {
        match stmt {
            syntax::Stmt::Assign(place, value) => {
                let place = self.trans_place(place);

                self.trans_value(place, value);
            },
        }
    }
}
