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
                write!(f, "\n\n")?;
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
            | DeclKind::Def(ty) => write!(f, "{} def {} {} :: {}", self.linkage, self.id, self.name, ty),
            | DeclKind::Type => write!(f, "{} type {} {}", self.linkage, self.id, self.name),
        }
    }
}

impl Display for Linkage {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            | Linkage::Import => write!(f, "import"),
            | Linkage::Export => write!(f, "export"),
            | Linkage::Local => write!(f, "local"),
        }
    }
}

impl Display for Body {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "{{")?;

        for block in &self.blocks {
            writeln!(f, "\n{}", block)?;
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
        write!(f, "block{}", self.index())
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.params.is_empty() {
            writeln!(f, "{}:", self.id)?;
        } else {
            writeln!(f, "{}({}):", self.id, list(&self.params, ", "))?;
        }

        for inst in &self.instrs {
            writeln!(f, "    {}", inst)?;
        }

        write!(f, "    {}", self.term)
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.kind {
            | InstrKind::Const { res, const_ } => write!(f, "{} = const {}", res, const_),
            | InstrKind::Load { res, ptr } => write!(f, "{} = load {}", res, ptr),
            | InstrKind::Store { ptr, val } => write!(f, "store {}, {}", ptr, val),
            | InstrKind::LoadField { res, val, field } => write!(f, "{} = load_field {}.{}", res, val, field),
            | InstrKind::StoreField { val, field, new_val } => write!(f, "store_field {}.{}, {}", val, field, new_val),
            | InstrKind::Call { rets, func, args } if rets.is_empty() => write!(f, "call {}({})", func, list(args, ", ")),
            | InstrKind::Call { rets, func, args } => write!(f, "{} = call {}({})", list(rets, ", "), func, list(args, ", ")),
            | InstrKind::Offset { res, ptr, by } => write!(f, "{} = offset {}, {}", res, ptr, by),
            | InstrKind::Add { res, lhs, rhs } => write!(f, "{} = add {}, {}", res, lhs, rhs),
            | InstrKind::Sub { res, lhs, rhs } => write!(f, "{} = sub {}, {}", res, lhs, rhs),
            | InstrKind::Mul { res, lhs, rhs } => write!(f, "{} = mul {}, {}", res, lhs, rhs),
            | InstrKind::Div { res, lhs, rhs } => write!(f, "{} = div {}, {}", res, lhs, rhs),
            | InstrKind::Rem { res, lhs, rhs } => write!(f, "{} = rem {}, {}", res, lhs, rhs),
            | InstrKind::Shl { res, lhs, rhs } => write!(f, "{} = shl {}, {}", res, lhs, rhs),
            | InstrKind::Shr { res, lhs, rhs } => write!(f, "{} = shr {}, {}", res, lhs, rhs),
            | InstrKind::And { res, lhs, rhs } => write!(f, "{} = and {}, {}", res, lhs, rhs),
            | InstrKind::Or { res, lhs, rhs } => write!(f, "{} = or {}, {}", res, lhs, rhs),
            | InstrKind::Xor { res, lhs, rhs } => write!(f, "{} = xor {}, {}", res, lhs, rhs),
            | InstrKind::Cmp { res, cc, lhs, rhs } => write!(f, "{} = cmp.{} {}, {}", cc, res, lhs, rhs),
        }
    }
}

impl Display for CondCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            | CondCode::Equal => write!(f, "eq"),
            | CondCode::NotEqual => write!(f, "ne"),
            | CondCode::Less => write!(f, "lt"),
            | CondCode::LessEqual => write!(f, "le"),
            | CondCode::Greater => write!(f, "gt"),
            | CondCode::GreaterEqual => write!(f, "ge"),
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            | Term::Unset => write!(f, "unset"),
            | Term::Abort => write!(f, "abort"),
            | Term::Return(vals) => write!(f, "return{}", list2(vals)),
            | Term::Br(to, args) if args.is_empty() => write!(f, "br {}", to),
            | Term::Br(to, args) => write!(f, "br {}({})", to, list(args, ", ")),
            | Term::BrIf(cond, then, else_, args) if args.is_empty() => write!(f, "brif {}, {}, {}", cond, then, else_),
            | Term::BrIf(cond, then, else_, args) => write!(f, "brif {}, {}({}), {}({})", cond, then, list(args, ", "), else_, list(args, ", ")),
        }
    }
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            | Operand::Var(v) => v.fmt(f),
            | Operand::Const(c) => c.fmt(f),
        }
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            | Const::Undefined => write!(f, "undefined"),
            | Const::Scalar(s) => s.fmt(f),
            | Const::Addr(id) => id.fmt(f),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.kind {
            | Type::Int(bits, true) => write!(f, "i{}", bits),
            | Type::Int(bits, false) => write!(f, "u{}", bits),
            | Type::Float(bits) => write!(f, "f{}", bits),
            | Type::Ptr(to) => write!(f, "ptr {}", to),
            | Type::Box(to) => write!(f, "box {}", to),
            | Type::Tuple(tys) => write!(f, "({})", list(tys, ", ")),
            | Type::Func(sig) => sig.fmt(f),
            | Type::Def(id) => id.fmt(f),
            | Type::Var(v) => v.fmt(f),
            | Type::Forall(vars, ty) => write!(f, "forall {}. {}", list(vars, " "), ty),
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
        write!(f, "({}) -> ({})", list(&self.params, ", "), list(&self.rets, ", "))
    }
}

fn list(it: impl IntoIterator<Item = impl Display>, sep: &str) -> String {
    it.into_iter().map(|s| s.to_string()).collect::<Vec<_>>().join(sep)
}

fn list2(it: impl IntoIterator<Item = impl Display>) -> String {
    it.into_iter().map(|s| format!(" {}", s)).collect::<Vec<_>>().join("")
}
