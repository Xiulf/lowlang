use super::{clif, pass::PassMode, CodegenCtx};
use clif::{types, Module};
use ir::layout::{Abi, Integer, Primitive, Scalar, TyAndLayout};

impl<'ctx> CodegenCtx<'ctx> {
    pub fn ir_type(&self, layout: &TyAndLayout) -> Option<clif::Type> {
        if let Abi::Scalar(s) = &layout.abi {
            Some(self.scalar_type(s))
        } else {
            None
        }
    }

    pub fn ir_pair_type(&self, layout: &TyAndLayout) -> Option<(clif::Type, clif::Type)> {
        if let Abi::ScalarPair(a, b) = &layout.abi {
            Some((self.scalar_type(a), self.scalar_type(b)))
        } else {
            None
        }
    }

    pub fn scalar_type(&self, scalar: &Scalar) -> clif::Type {
        match &scalar.value {
            | Primitive::Int(Integer::I8, _) => types::I8,
            | Primitive::Int(Integer::I16, _) => types::I16,
            | Primitive::Int(Integer::I32, _) => types::I32,
            | Primitive::Int(Integer::I64, _) => types::I64,
            | Primitive::Int(Integer::I128, _) => types::I128,
            | Primitive::Int(Integer::ISize, _) => self.module.target_config().pointer_type(),
            | Primitive::F32 => types::F32,
            | Primitive::F64 => types::F64,
            | Primitive::Pointer => self.module.target_config().pointer_type(),
        }
    }

    pub fn vector_type(&self, scalar: &Scalar, lanes: u64) -> clif::Type {
        self.scalar_type(scalar).by(lanes as u16).unwrap()
    }

    pub fn ty_as_sig(&self, ty: ir::ty::Ty) -> clif::Signature {
        let (generics, sig) = ty.get_sig(self.db);

        self.mk_signature(&generics, &sig)
    }

    pub fn mk_signature(&self, generics: &[ir::ty::GenericParam], sig: &ir::ty::Signature) -> clif::Signature {
        let mut res = self.module.make_signature();
        let ptr_type = self.module.target_config().pointer_type();

        for ret in &sig.rets {
            match self.pass_mode(&self.db.layout_of(ret.ty)) {
                | PassMode::NoPass => {},
                | PassMode::ByVal(t) => {
                    res.returns.push(clif::AbiParam::new(t));
                },
                | PassMode::ByValPair(a, b) => {
                    res.returns.push(clif::AbiParam::new(a));
                    res.returns.push(clif::AbiParam::new(b));
                },
                | PassMode::ByRef { .. } => {
                    res.params.push(clif::AbiParam::new(ptr_type));
                },
            }
        }

        for param in &sig.params {
            match self.pass_mode(&self.db.layout_of(param.ty)) {
                | PassMode::NoPass => {},
                | PassMode::ByVal(t) => {
                    res.params.push(clif::AbiParam::new(t));
                },
                | PassMode::ByValPair(a, b) => {
                    res.params.push(clif::AbiParam::new(a));
                    res.params.push(clif::AbiParam::new(b));
                },
                | PassMode::ByRef { .. } => {
                    res.params.push(clif::AbiParam::new(ptr_type));
                },
            }
        }

        for _gen in generics {
            res.params.push(clif::AbiParam::new(ptr_type));
        }

        res
    }
}
