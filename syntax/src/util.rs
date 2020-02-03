use crate::*;

impl Body {
    pub fn args(&self) -> Vec<&Local> {
        self.locals.iter().map(|l| l.1).filter(|l| l.kind == LocalKind::Arg).collect()
    }

    pub fn rets(&self) -> Vec<&Local> {
        self.locals.iter().map(|l| l.1).filter(|l| l.kind == LocalKind::Ret).collect()
    }
}
