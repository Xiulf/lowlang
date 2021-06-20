use crate::*;
use std::fmt::{self, Write as _};

impl fmt::Display for FuncId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let id: u32 = self.0.into_raw().into();

        write!(f, "fn{}", id)
    }
}

impl fmt::Display for BodyId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let id: u32 = self.0.into_raw().into();

        write!(f, "body{}", id)
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let id: u32 = self.0.into_raw().into();

        write!(f, "v{}", id)
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let id: u32 = self.0.into_raw().into();

        write!(f, "block{}", id)
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "module {} {{", self.name)?;
        writeln!(f)?;

        for (id, func) in self.funcs.iter() {
            writeln!(f, "{}: {}", FuncId(id), func)?;
        }

        writeln!(f)?;

        for (id, body) in self.bodies.iter() {
            writeln!(f, "{}: {}", BodyId(id), body)?;
            writeln!(f)?;
        }

        write!(f, "}}")
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {:?} : ${}", self.linkage, self.name, self.sig)
    }
}

impl fmt::Display for Linkage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Linkage::Import => write!(f, "import"),
            | Linkage::Export => write!(f, "export"),
            | Linkage::Local => write!(f, "local "),
        }
    }
}

impl fmt::Display for Body {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.generic_params.is_empty() {
            write!(f, "<")?;
            list(f, &self.generic_params, GenericParam::fmt)?;
            write!(f, "> ")?;
        }

        writeln!(f, "{{")?;

        // for (id, var) in self.vars.iter() {
        //     writeln!(f, "    {} : ${}", Var(id), var.ty)?;
        // }

        for (id, block) in self.blocks.iter() {
            writeln!(f)?;
            write!(f, "{}{}", Block(id), block.display(self))?;
        }

        write!(f, "}}")
    }
}

pub struct BodyDisplay<'a, T> {
    body: &'a Body,
    t: &'a T,
}

impl BlockData {
    pub fn display<'a>(&'a self, body: &'a Body) -> BodyDisplay<'a, Self> {
        BodyDisplay { body, t: self }
    }
}

impl Term {
    pub fn display<'a>(&'a self, body: &'a Body) -> BodyDisplay<'a, Self> {
        BodyDisplay { body, t: self }
    }
}

impl Instr {
    pub fn display<'a>(&'a self, body: &'a Body) -> BodyDisplay<'a, Self> {
        BodyDisplay { body, t: self }
    }
}

impl fmt::Display for BodyDisplay<'_, BlockData> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.t.params.is_empty() {
            write!(f, "(")?;
            list(f, &self.t.params, Var::fmt)?;
            write!(f, ")")?;
        }

        writeln!(f, ":")?;

        for instr in &self.t.instrs {
            writeln!(f, "    {}", instr.display(self.body))?;
        }

        if let Some(term) = &self.t.term {
            writeln!(f, "    {}", term.display(self.body))
        } else {
            Ok(())
        }
    }
}

impl fmt::Display for BodyDisplay<'_, Term> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.t {
            | Term::Unreachable => write!(f, "unreachable"),
            | Term::Return { ops } => {
                write!(f, "return ")?;
                list(f, ops, Var::fmt)
            },
            | Term::Br { to } => write!(f, "br {}", to),
            | Term::Switch { pred, cases, default } => {
                write!(f, "switch {}", pred)?;
                list2(f, cases)?;
                write!(f, ", default {}", default)
            },
        }
    }
}

impl fmt::Display for BodyDisplay<'_, Instr> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.t {
            | Instr::StackAlloc { ret, ty } => write!(f, "{} = stack_alloc ${}", ret, ty),
            | Instr::StackFree { addr } => write!(f, "stack_free {}", addr),
            | Instr::BoxAlloc { ret, ty } => write!(f, "{} = box_alloc ${}", ret, ty),
            | Instr::BoxFree { boxed } => write!(f, "box_free {}", boxed),
            | Instr::BoxAddr { ret, boxed } => write!(f, "{} = box_addr {}", ret, boxed),
            | Instr::Load { ret, addr } => write!(f, "{} = load {}", ret, addr),
            | Instr::Store { val, addr } => write!(f, "store {}, {}", val, addr),
            | Instr::CopyAddr { old, new, flags } if flags.is_set(Flags::TAKE) => write!(f, "copy_addr {}, {} [take]", old, new),
            | Instr::CopyAddr { old, new, flags } if flags.is_set(Flags::INIT) => write!(f, "copy_addr {}, {} [init]", old, new),
            | Instr::CopyAddr { old, new, flags } => write!(f, "copy_addr {}, {}", old, new),
            | Instr::ConstInt { ret, val } => write!(f, "{} = const_int {} : ${}", ret, val, self.body[*ret].ty),
            | Instr::FuncRef { ret, func } => write!(f, "{} = func_ref {}", ret, func),
            | Instr::Apply { rets, func, args, subst } => {
                if !rets.is_empty() {
                    list(f, rets, Var::fmt)?;
                    write!(f, " = ")?;
                }

                write!(f, "apply {}", func)?;

                if !subst.is_empty() {
                    write!(f, "<")?;
                    list(f, subst, Subst::fmt)?;
                    write!(f, ">")?;
                }

                write!(f, "(")?;
                list(f, args, Var::fmt)?;
                write!(f, ")")
            },
            | Instr::Intrinsic { rets, name, args } => {
                if !rets.is_empty() {
                    list(f, rets, Var::fmt)?;
                    write!(f, " = ")?;
                }

                write!(f, "intrinsic {:?}(", name)?;
                list(f, args, Var::fmt)?;
                write!(f, ")")
            },
        }
    }
}

impl fmt::Display for SwitchCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.val, self.to)
    }
}

impl fmt::Display for BrTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.args.is_empty() {
            self.block.fmt(f)
        } else {
            write!(f, "{}(", self.block)?;
            list(f, &self.args, Var::fmt)?;
            write!(f, ")")
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use crate::layout::Primitive;

        match &self.kind {
            | typ::Unit => match self.repr.scalar {
                | Some(prim) => match prim {
                    | Primitive::Int(Integer::I8, true) => write!(f, "i8"),
                    | Primitive::Int(Integer::I16, true) => write!(f, "i16"),
                    | Primitive::Int(Integer::I32, true) => write!(f, "i32"),
                    | Primitive::Int(Integer::I64, true) => write!(f, "i64"),
                    | Primitive::Int(Integer::I128, true) => write!(f, "i128"),
                    | Primitive::Int(Integer::I8, false) => write!(f, "u8"),
                    | Primitive::Int(Integer::I16, false) => write!(f, "u16"),
                    | Primitive::Int(Integer::I32, false) => write!(f, "u32"),
                    | Primitive::Int(Integer::I64, false) => write!(f, "u64"),
                    | Primitive::Int(Integer::I128, false) => write!(f, "u128"),
                    | Primitive::F32 => write!(f, "f32"),
                    | Primitive::F64 => write!(f, "f64"),
                    | Primitive::Pointer => write!(f, "ptr"),
                },
                | None => write!(f, "()"),
            },
            | typ::Ptr(to) => write!(f, "*{}", to),
            | typ::Box(to) => write!(f, "box {}", to),
            | typ::Var(tv) => tv.fmt(f),
            | typ::Func(sig) => sig.fmt(f),
            | typ::Generic(params, ty) => {
                write!(f, "<")?;
                list(f, params.iter(), GenericParam::fmt)?;
                write!(f, "> {}", ty)
            },
        }
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        list(f, self.params.iter(), SigParam::fmt)?;
        write!(f, ") -> ")?;

        if self.rets.len() == 1 {
            self.rets[0].fmt(f)
        } else {
            write!(f, "(")?;
            list(f, self.rets.iter(), SigParam::fmt)?;
            write!(f, ")")
        }
    }
}

impl fmt::Display for SigParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.flags.is_set(Flags::IN) {
            write!(f, "@in ")?;
        }

        if self.flags.is_set(Flags::OUT) {
            write!(f, "@out ")?;
        }

        self.ty.fmt(f)
    }
}

impl fmt::Display for GenericVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}'{}", self.0, self.1)
    }
}

impl fmt::Display for GenericParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | GenericParam::Type => write!(f, "type"),
            | GenericParam::Figure => write!(f, "figure"),
            | GenericParam::Symbol => write!(f, "symbol"),
        }
    }
}

impl fmt::Display for Subst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Subst::Type(t) => write!(f, "${}", t),
            | Subst::Figure(i) => write!(f, "{}", i),
            | Subst::Symbol(s) => write!(f, "{:?}", s),
        }
    }
}

fn list<'a, T: 'a>(
    f: &mut fmt::Formatter<'_>,
    ts: impl IntoIterator<Item = &'a T>,
    fmt: impl Fn(&'a T, &mut fmt::Formatter<'_>) -> fmt::Result,
) -> fmt::Result {
    for (i, t) in ts.into_iter().enumerate() {
        if i != 0 {
            write!(f, ", ")?;
        }

        fmt(t, f)?;
    }

    Ok(())
}

fn list2<T: fmt::Display>(f: &mut fmt::Formatter<'_>, ts: impl IntoIterator<Item = T>) -> fmt::Result {
    for t in ts {
        write!(f, ", ")?;
        t.fmt(f)?;
    }

    Ok(())
}
