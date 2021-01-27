use crate::*;
use std::fmt::{Display, Formatter, Result};

impl Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for (i, decl) in self.defs.iter().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }

            decl.fmt(f)?;
        }

        if !self.defs.is_empty() && !self.bodies.is_empty() {
            write!(f, "\n\n")?;
        }

        for (i, (id, body)) in self.bodies.iter().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }

            write!(f, "def {} {}", id, body)?;
        }

        Ok(())
    }
}

impl Display for DefId {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "@{}", self.index())
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.kind {
            DeclKind::Def(ty) => write!(
                f,
                "{} def {} {} :: {}",
                self.linkage, self.id, self.name, ty
            ),
            DeclKind::Type => write!(f, "{} type {} {}", self.linkage, self.id, self.name),
        }
    }
}

impl Display for Linkage {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Linkage::Import => write!(f, "import"),
            Linkage::Export => write!(f, "export"),
            Linkage::Local => write!(f, "local"),
        }
    }
}

impl Display for Body {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{{")?;

        for block in &self.blocks {
            writeln!(f, "{}", block)?;
        }

        write!(f, "}}")
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "var {} :: {}", self.id, self.ty)
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "v{}", self.index())
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "%{}", self.index())
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for inst in &self.instrs {
            writeln!(f, "    {}", inst)?;
        }

        write!(f, "    {}", self.term)
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Instr::Const(var, c, ty) => write!(f, "{} = const {} {}", var, c, ty),
            Instr::Load(var, op) => write!(f, "{} = load {}", var, op),
            Instr::Store(op, var) => write!(f, "store {} {}", op, var),
            Instr::Call(vars, func, args) => {
                write!(f, "{} = {}({})", list(vars, ", "), func, list(args, ", "))
            }
            Instr::Offset(o, l, r) => write!(f, "{} = offset {} {}", o, l, r),
            Instr::Add(o, l, r) => write!(f, "{} = add {} {}", o, l, r),
            Instr::Sub(o, l, r) => write!(f, "{} = sub {} {}", o, l, r),
            Instr::Mul(o, l, r) => write!(f, "{} = mul {} {}", o, l, r),
            Instr::Div(o, l, r) => write!(f, "{} = div {} {}", o, l, r),
            Instr::Rem(o, l, r) => write!(f, "{} = rem {} {}", o, l, r),
            Instr::Eq(o, l, r) => write!(f, "{} = eq {} {}", o, l, r),
            Instr::Ne(o, l, r) => write!(f, "{} = ne {} {}", o, l, r),
            Instr::Lt(o, l, r) => write!(f, "{} = lt {} {}", o, l, r),
            Instr::Le(o, l, r) => write!(f, "{} = le {} {}", o, l, r),
            Instr::Gt(o, l, r) => write!(f, "{} = gt {} {}", o, l, r),
            Instr::Ge(o, l, r) => write!(f, "{} = ge {} {}", o, l, r),
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Term::Unset => write!(f, "unset"),
            Term::Abort => write!(f, "abort"),
            Term::Return => write!(f, "return"),
            Term::Br(to) => write!(f, "br {}", to),
            Term::BrNz(var, to) => write!(f, "brnz {} {}", var, to),
            Term::BrZ(var, to) => write!(f, "brz {} {}", var, to),
        }
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Const::Undefined => write!(f, "undefined"),
            Const::Scalar(s) => s.fmt(f),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Type::Int(bits, true) => write!(f, "i{}", bits),
            Type::Int(bits, false) => write!(f, "u{}", bits),
            Type::Float(bits) => write!(f, "f{}", bits),
            Type::Ptr(to) => write!(f, "*{}", to),
            Type::Box(to) => write!(f, "&{}", to),
            Type::Func(sig) => sig.fmt(f),
            Type::Def(id) => id.fmt(f),
            Type::Var(v) => v.fmt(f),
            Type::Forall(vars, ty) => write!(f, "forall {}. {}", list(vars, " "), ty),
        }
    }
}

impl Display for TypeVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "'{}", self.0)
    }
}

impl Display for Signature {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "({}) -> ({})",
            list(&self.params, ", "),
            list(&self.rets, ", ")
        )
    }
}

fn list(it: impl IntoIterator<Item = impl Display>, sep: &str) -> String {
    it.into_iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>()
        .join(sep)
}
