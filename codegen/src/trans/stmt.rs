use crate::{FunctionCtx, Backend};

impl<'a, B: Backend> FunctionCtx<'a, B> {
    pub fn trans_stmt(&mut self, stmt: &syntax::Stmt) {
        match stmt {
            syntax::Stmt::Assign(place, value) => {
                let place = self.trans_place(place);

                self.trans_value(place, value);
            },
        }
    }
}
