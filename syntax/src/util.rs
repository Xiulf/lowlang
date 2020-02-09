use crate::*;

impl<'t> Package<'t> {
    pub fn insintric_gcd(&self) -> ItemId {
        self.bodies.iter().find(|(_, body)| body.attributes.lang && body.name == "__insintric_gcd").map(|e| *e.0).unwrap()
    }

    pub fn insintric_lcm(&self) -> ItemId {
        self.bodies.iter().find(|(_, body)| body.attributes.lang && body.name == "__insintric_lcm").map(|e| *e.0).unwrap()
    }
}

impl<'t> Body<'t> {
    pub fn args(&self) -> Vec<&Local<'t>> {
        self.locals.iter().map(|l| l.1).filter(|l| l.kind == LocalKind::Arg).collect()
    }

    pub fn rets(&self) -> Vec<&Local<'t>> {
        self.locals.iter().map(|l| l.1).filter(|l| l.kind == LocalKind::Ret).collect()
    }
}
