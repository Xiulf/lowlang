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
    pub fn is_polymorphic(&self) -> bool {
        !self.generics.is_empty()
    }

    pub fn args(&self) -> Vec<&Local<'t>> {
        self.locals.iter().map(|l| l.1).filter(|l| l.kind == LocalKind::Arg).collect()
    }

    pub fn rets(&self) -> Vec<&Local<'t>> {
        self.locals.iter().map(|l| l.1).filter(|l| l.kind == LocalKind::Ret).collect()
    }
}

pub struct Mut<'a, T>{
    ptr: *mut T,
    _marker: std::marker::PhantomData<&'a mut T>,
}

#[allow(non_snake_case)]
pub fn Mut<'a, T>(ptr: &'a mut T) -> Mut<'a, T> {
    Mut { ptr, _marker: std::marker::PhantomData }
}

impl<'a, T> Mut<'a, T> {
    pub fn get(&self) -> &'a T {
        unsafe { & *self.ptr }
    }
    
    pub fn get_mut(&self) -> &'a mut T {
        unsafe { &mut *self.ptr }
    }
}

impl<'a, T> std::ops::Deref for Mut<'a, T> {
    type Target = T;
    
    fn deref(&self) -> &T {
        self.get()
    }
}

impl<'a, T> std::ops::DerefMut for Mut<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.get_mut()
    }
}
