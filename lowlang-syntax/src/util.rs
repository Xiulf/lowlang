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

    pub fn max_local_id(&self) -> LocalId {
        let mut max = LocalId(0);

        for (id, _) in &self.locals {
            max = std::cmp::max(max, *id);
        }

        max
    }

    pub fn max_block_id(&self) -> BlockId {
        let mut max = BlockId(0);

        for (id, _) in &self.blocks {
            max = std::cmp::max(max, *id);
        }

        max
    }
}

impl Place {
    pub fn merge(&self, other: &Place) -> Place {
        let mut elems = self.elems.clone();

        elems.extend(other.elems.clone());

        Place {
            base: other.base.clone(),
            elems,
        }
    }
}

impl BlockId {
    pub const FIRST: BlockId = BlockId(0);
}

impl Addr {
    pub fn id(&self) -> ItemId {
        match self {
            Addr::Id(id) => *id,
            Addr::Name(_) => unreachable!(),
        }
    }

    pub fn id_mut(&mut self) -> &mut ItemId {
        match self {
            Addr::Id(id) => id,
            Addr::Name(_) => unreachable!(),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Addr::Name(name) => name,
            Addr::Id(_) => unreachable!(),
        }
    }
}

impl std::ops::Add for LocalId {
    type Output = LocalId;

    fn add(self, rhs: LocalId) -> LocalId {
        LocalId(self.0 + rhs.0)
    }
}

impl std::ops::Add for BlockId {
    type Output = BlockId;

    fn add(self, rhs: BlockId) -> BlockId {
        BlockId(self.0 + rhs.0)
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
