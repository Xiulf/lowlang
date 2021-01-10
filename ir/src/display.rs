use crate::*;
use std::fmt::{Display, Formatter, Result};

impl Display for Module {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for (i, decl) in self.decls.iter().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }

            write!(f, "{}", decl)?;
        }

        let fmt_impl = |impl_: &Impl, f: &mut Formatter| -> Result {
            write!(f, "\x1B[0;31mimpl\x1B[0;34m {}\x1B[0m {{", impl_.name)?;

            for entry in &impl_.entries {
                write!(f, "\n    ")?;

                match entry {
                    ImplEntry::Base(id) => {
                        let impl_ = &self.impls[*id].name;

                        write!(f, "\x1B[0;31mbase \x1B[0;34m{}\x1B[0m", impl_)?;
                    }
                    ImplEntry::Func(name, id) => {
                        let decl = &self.decls[*id].name;

                        write!(
                            f,
                            "\x1B[0;31mfn\x1B[0;32m {}\x1B[0m :: \x1B[0;34m@{}\x1B[0m",
                            name, decl
                        )?;
                    }
                }
            }

            if !impl_.entries.is_empty() {
                writeln!(f)?;
            }

            write!(f, "}}")
        };

        fn fmt_place(place: &Place, f: &mut Formatter, this: &Module) -> Result {
            for elem in &place.elems {
                match elem {
                    PlaceElem::Deref => write!(f, "(*")?,
                    PlaceElem::Field(_) => {}
                    PlaceElem::Index(_) => {}
                    PlaceElem::Downcast(_) => write!(f, "(")?,
                }
            }

            write!(f, "{}", place.local)?;

            for elem in &place.elems {
                match elem {
                    PlaceElem::Deref => write!(f, ")")?,
                    PlaceElem::Field(idx) => write!(f, ".{}", idx)?,
                    PlaceElem::Index(idx) => {
                        write!(f, "[")?;
                        fmt_op(idx, f, this)?;
                        write!(f, "]")?;
                    }
                    PlaceElem::Downcast(i) => write!(f, " as {})", i)?,
                }
            }

            Ok(())
        }

        fn fmt_const(c: &Const, f: &mut Formatter, this: &Module) -> Result {
            match c {
                Const::Undefined(ty) => write!(f, "\x1B[0;32mundefined\x1B[0m :: {}", ty),
                Const::Scalar(s, ty) => write!(f, "\x1B[0;32m{}\x1B[0m :: {}", s, ty),
                Const::Addr(decl) => write!(f, "\x1B[0;34m@{}\x1B[0m", this.decls[*decl].name),
                Const::Ptr(to) => {
                    write!(f, "&")?;
                    fmt_const(to, f, this)
                }
                Const::Tuple(cs) => {
                    write!(f, "(")?;

                    for (i, c) in cs.iter().enumerate() {
                        if i != 0 {
                            write!(f, ", ")?;
                        }

                        fmt_const(c, f, this)?;
                    }

                    write!(f, ")")
                }
                Const::Variant(i, cs, ty) => {
                    write!(f, "{}(", i)?;

                    for (i, c) in cs.iter().enumerate() {
                        if i != 0 {
                            write!(f, ", ")?;
                        }

                        fmt_const(c, f, this)?;
                    }

                    write!(f, ") :: {}", ty)
                }
            }
        }

        fn fmt_op(op: &Operand, f: &mut Formatter, this: &Module) -> Result {
            match op {
                Operand::Place(place) => fmt_place(place, f, this),
                Operand::Const(c) => fmt_const(c, f, this),
            }
        }

        let fmt_rvalue = |rvalue: &RValue, f: &mut Formatter| -> Result {
            match rvalue {
                RValue::Use(op) => fmt_op(op, f, self),
                RValue::AddrOf(place) => {
                    write!(f, "\x1B[0;31maddrof\x1B[0m ")?;
                    fmt_place(place, f, self)
                }
                RValue::GetDiscr(place) => {
                    write!(f, "\x1B[0;31mget_discr\x1B[0m ")?;
                    fmt_place(place, f, self)
                }
                RValue::Cast(place, ty) => {
                    write!(f, "\x1B[0;31mcast\x1B[0m ")?;
                    fmt_place(place, f, self)?;
                    write!(f, ", {}", ty)
                }
                RValue::Intrinsic(name, args) => {
                    write!(f, "\x1B[0;32m#{}\x1B[0m(", name)?;

                    for (i, arg) in args.iter().enumerate() {
                        if i != 0 {
                            write!(f, ", ")?;
                        }

                        fmt_op(arg, f, self)?;
                    }

                    write!(f, ")")
                }
            }
        };

        let fmt_stmt = |stmt: &Stmt, f: &mut Formatter| -> Result {
            match stmt {
                Stmt::Init(local) => write!(f, "\x1B[0;31minit {}", local),
                Stmt::Drop(local) => write!(f, "\x1B[0;31mdrop {}", local),
                Stmt::Assign(place, rvalue) => {
                    fmt_place(place, f, self)?;
                    write!(f, " = ")?;
                    fmt_rvalue(rvalue, f)
                }
                Stmt::SetDiscr(place, val) => {
                    write!(f, "\x1B[0;31mset_discr\x1B[0m ")?;
                    fmt_place(place, f, self)?;
                    write!(f, ", \x1B[0;32m{}\x1B[0m", val)
                }
                Stmt::Call(rets, func, args) => {
                    write!(f, "\x1B[0;31mcall\x1B[0m ")?;
                    fmt_op(func, f, self)?;
                    write!(f, "(")?;

                    for (i, arg) in args.iter().enumerate() {
                        if i != 0 {
                            write!(f, ", ")?;
                        }

                        fmt_op(arg, f, self)?;
                    }

                    write!(f, ")")?;

                    if !rets.is_empty() {
                        write!(f, " -> ")?;

                        for (i, place) in rets.iter().enumerate() {
                            if i != 0 {
                                write!(f, ", ")?;
                            }

                            fmt_place(place, f, self)?;
                        }
                    }

                    Ok(())
                }
            }
        };

        let fmt_term = |term: &Term, f: &mut Formatter| -> Result {
            match term {
                Term::Abort => write!(f, "\x1B[0;31mabort\x1B[0m"),
                Term::Return => write!(f, "\x1B[0;31mreturn\x1B[0m"),
                Term::Jump(to) => write!(f, "\x1B[0;31mjump\x1B[0m {}", to),
                Term::Switch(op, vals, blocks) => {
                    write!(f, "\x1B[0;31mswitch\x1B[0m ")?;
                    fmt_op(op, f, self)?;
                    write!(f, " [")?;

                    for (val, block) in vals.iter().zip(blocks) {
                        write!(f, "{}: {}, ", val, block)?;
                    }

                    write!(
                        f,
                        "\x1B[0;31motherwise\x1B[0m {}]",
                        blocks[blocks.len() - 1]
                    )
                }
            }
        };

        let fmt_block = |block: &BlockData, f: &mut Formatter| -> Result {
            write!(f, "{}:", block.id)?;

            for stmt in &block.stmts {
                write!(f, "\n    ")?;
                fmt_stmt(stmt, f)?;
            }

            write!(f, "\n    ")?;
            fmt_term(&block.term, f)?;

            Ok(())
        };

        let fmt_body = |body: &Body, f: &mut Formatter| -> Result {
            write!(
                f,
                "\x1B[0;31mfn\x1B[0;34m @{}\x1B[0m {{",
                self.decls[body.decl].name
            )?;

            for local in &body.locals {
                write!(f, "\n    ")?;
                local.fmt(f)?;
            }

            for block in &body.blocks {
                writeln!(f)?;
                fmt_block(block, f)?;
            }

            if !body.locals.is_empty() || !body.blocks.is_empty() {
                writeln!(f)?;
            }

            write!(f, "}}")
        };

        if !self.decls.is_empty() && !self.impls.is_empty() {
            write!(f, "\n\n")?;
        }

        for (i, impl_) in self.impls.iter().enumerate() {
            if i != 0 {
                write!(f, "\n\n")?;
            }

            fmt_impl(impl_, f)?;
        }

        if (!self.decls.is_empty() || !self.impls.is_empty()) && !self.bodies.is_empty() {
            write!(f, "\n\n")?;
        }

        for (i, body) in self.bodies.iter().enumerate() {
            if i != 0 {
                write!(f, "\n\n")?;
            }

            fmt_body(body, f)?;
        }

        Ok(())
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "{}\x1B[0;31m{}\x1B[0;34m @{}\x1B[0m :: {}",
            self.attrs, self.linkage, self.name, self.ty
        )
    }
}

impl Display for Linkage {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Linkage::Import => write!(f, "import"),
            Linkage::Export => write!(f, "export"),
            Linkage::Hidden => write!(f, "hidden"),
            Linkage::Local => write!(f, "local"),
        }
    }
}

impl Display for Attrs {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.c_abi {
            writeln!(f, "\x1B[0;35m@c_abi")?;
        }

        Ok(())
    }
}

impl Display for LocalData {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "\x1B[0;31m{} {} :: {}", self.kind, self.id, self.ty)
    }
}

impl Display for Local {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "\x1B[0;36m_{}\x1B[0m", self.index())
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "\x1B[0;35m%{}\x1B[0m", self.index())
    }
}

impl Display for LocalKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            LocalKind::Ret => write!(f, "ret"),
            LocalKind::Arg => write!(f, "arg"),
            LocalKind::Var => write!(f, "var"),
            LocalKind::Tmp => write!(f, "tmp"),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.kind.fmt(f)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Type::U8 => write!(f, "\x1B[0;33mu8\x1B[0m"),
            Type::U16 => write!(f, "\x1B[0;33mu16\x1B[0m"),
            Type::U32 => write!(f, "\x1B[0;33mu32\x1B[0m"),
            Type::U64 => write!(f, "\x1B[0;33mu64\x1B[0m"),
            Type::U128 => write!(f, "\x1B[0;33mu128\x1B[0m"),
            Type::I8 => write!(f, "\x1B[0;33mi8\x1B[0m"),
            Type::I16 => write!(f, "\x1B[0;33mi16\x1B[0m"),
            Type::I32 => write!(f, "\x1B[0;33mi32\x1B[0m"),
            Type::I64 => write!(f, "\x1B[0;33mi64\x1B[0m"),
            Type::I128 => write!(f, "\x1B[0;33mi128\x1B[0m"),
            Type::F32 => write!(f, "\x1B[0;33mf32\x1B[0m"),
            Type::F64 => write!(f, "\x1B[0;33mf64\x1B[0m"),
            Type::Type(t) => write!(f, "\x1B[0;33mtype {}\x1B[0m", t),
            Type::Vwt(t) => write!(f, "\x1B[0;33mvwt {}\x1B[0m", t),
            Type::Ptr(to) => write!(f, "*{}", to),
            Type::Box(to) => write!(f, "&{}", to),
            Type::Tuple(tys) => write!(
                f,
                "({})",
                tys.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::Union(tys) => write!(
                f,
                "({})",
                tys.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" | ")
            ),
            Type::Tagged(tys) => write!(
                f,
                "({})",
                tys.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" / ")
            ),
            Type::Opaque(name) => write!(f, "\x1B[0;33m{}\x1B[0m", name),
            Type::Func(sig) => sig.fmt(f),
            Type::Discr(ty) => write!(f, "\x1B[0;33mdiscr\x1B[0m {}", ty),
            Type::Recurse(i) => write!(f, "\x1B[0;33m\\{}\x1B[0m", i),
        }
    }
}

impl Display for Signature {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let params = self
            .params
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        let rets = self
            .rets
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "({}) -> ({})", params, rets)
    }
}
