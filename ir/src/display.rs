use crate::db::IrDatabase;
use crate::*;
use std::fmt;

pub struct IrDisplay<'a, T>(&'a T, &'a dyn IrDatabase);

impl fmt::Display for TypeDefId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let id = self.0.as_u32();

        write!(f, "type{}", id)
    }
}

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

impl Module {
    pub fn display<'a>(&'a self, db: &'a dyn IrDatabase) -> IrDisplay<'a, Self> {
        IrDisplay(self, db)
    }
}

impl fmt::Display for IrDisplay<'_, Module> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let IrDisplay(this, db) = *self;

        writeln!(f, "module {} {{", this.name)?;
        writeln!(f)?;

        for (id, func) in this.funcs.iter() {
            writeln!(f, "{}: {}", FuncId(id), func.display(db))?;
        }

        writeln!(f)?;

        for (id, body) in this.bodies.iter() {
            writeln!(f, "{}: {}", BodyId(id), body.display(db))?;
            writeln!(f)?;
        }

        write!(f, "}}")
    }
}

impl Func {
    pub fn display<'a>(&'a self, db: &'a dyn IrDatabase) -> IrDisplay<'a, Self> {
        IrDisplay(self, db)
    }
}

impl fmt::Display for IrDisplay<'_, Func> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let IrDisplay(this, db) = *self;

        write!(f, "{} {:?} : ${}", this.linkage, this.name, this.sig.display(db))
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

impl TypeDef {
    pub fn display<'a>(&'a self, db: &'a dyn IrDatabase) -> IrDisplay<'a, Self> {
        IrDisplay(self, db)
    }
}

impl fmt::Display for IrDisplay<'_, TypeDef> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let IrDisplay(this, db) = *self;

        write!(f, "type {:?}", this.name)?;

        if !this.generic_params.is_empty() {
            write!(f, "<")?;

            list(f, this.generic_params.iter().enumerate(), |(i, p), f| {
                p.fmt(f)?;
                write!(f, " {}", (b'A' + i as u8) as char)
            })?;

            write!(f, ">")?;
        }

        if let Some(body) = &this.body {
            write!(f, " = {}", body.display(db))?;
        }

        Ok(())
    }
}

impl TypeDefBody {
    pub fn display<'a>(&'a self, db: &'a dyn IrDatabase) -> IrDisplay<'a, Self> {
        IrDisplay(self, db)
    }
}

impl fmt::Display for IrDisplay<'_, TypeDefBody> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let IrDisplay(this, db) = *self;

        match this {
            | TypeDefBody::Struct { fields } => {
                writeln!(f, "struct {{")?;

                for field in fields {
                    writeln!(f, "    {},", field.display(db))?;
                }

                write!(f, "}}")
            },
            | TypeDefBody::Union { fields } => {
                writeln!(f, "union {{")?;

                for field in fields {
                    writeln!(f, "    {},", field.display(db))?;
                }

                write!(f, "}}")
            },
            | TypeDefBody::Enum { variants } => {
                writeln!(f, "enum {{")?;

                for variant in variants {
                    writeln!(f, "    {},", variant.display(db))?;
                }

                write!(f, "}}")
            },
        }
    }
}

impl TypeDefField {
    pub fn display<'a>(&'a self, db: &'a dyn IrDatabase) -> IrDisplay<'a, Self> {
        IrDisplay(self, db)
    }
}

impl fmt::Display for IrDisplay<'_, TypeDefField> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let IrDisplay(this, db) = *self;

        write!(f, "{} : ${}", this.name, this.ty.display(db))
    }
}

impl TypeDefVariant {
    pub fn display<'a>(&'a self, db: &'a dyn IrDatabase) -> IrDisplay<'a, Self> {
        IrDisplay(self, db)
    }
}

impl fmt::Display for IrDisplay<'_, TypeDefVariant> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let IrDisplay(this, db) = *self;

        write!(f, "{}", this.name)?;

        if let Some(payload) = &this.payload {
            write!(f, " : ${}", payload.display(db))?;
        }

        Ok(())
    }
}

impl Body {
    pub fn display<'a>(&'a self, db: &'a dyn IrDatabase) -> IrDisplay<'_, Self> {
        IrDisplay(self, db)
    }
}

impl fmt::Display for IrDisplay<'_, Body> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let IrDisplay(this, db) = *self;

        if !this.generic_params.is_empty() {
            write!(f, "<")?;

            list(f, this.generic_params.iter().enumerate(), |(i, p), f| {
                p.fmt(f)?;
                write!(f, " {}", (b'A' + i as u8) as char)
            })?;

            write!(f, "> ")?;
        }

        writeln!(f, "{{")?;

        // for (id, var) in self.vars.iter() {
        //     writeln!(f, "    {} : ${}", Var(id), var.ty)?;
        // }

        for (id, block) in this.blocks.iter() {
            writeln!(f)?;
            write!(f, "{}{}", Block(id), block.display(db, this))?;
        }

        write!(f, "}}")
    }
}

pub struct BodyDisplay<'a, T> {
    db: &'a dyn IrDatabase,
    body: &'a Body,
    t: &'a T,
}

impl BlockData {
    pub fn display<'a>(&'a self, db: &'a dyn IrDatabase, body: &'a Body) -> BodyDisplay<'a, Self> {
        BodyDisplay { body, db, t: self }
    }
}

impl Term {
    pub fn display<'a>(&'a self, db: &'a dyn IrDatabase, body: &'a Body) -> BodyDisplay<'a, Self> {
        BodyDisplay { body, db, t: self }
    }
}

impl Instr {
    pub fn display<'a>(&'a self, db: &'a dyn IrDatabase, body: &'a Body) -> BodyDisplay<'a, Self> {
        BodyDisplay { body, db, t: self }
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
            writeln!(f, "    {}", instr.display(self.db, self.body))?;
        }

        if let Some(term) = &self.t.term {
            writeln!(f, "    {}", term.display(self.db, self.body))
        } else {
            Ok(())
        }
    }
}

impl fmt::Display for BodyDisplay<'_, Term> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.t {
            | Term::Unreachable => write!(f, "unreachable"),
            | Term::Return { vals: ops } => {
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
            | Instr::StackAlloc { ret, ty } => write!(f, "{} = stack_alloc ${}", ret, ty.display(self.db)),
            | Instr::StackFree { addr } => write!(f, "stack_free {}", addr),
            | Instr::BoxAlloc { ret, ty } => write!(f, "{} = box_alloc ${}", ret, ty.display(self.db)),
            | Instr::BoxFree { boxed } => write!(f, "box_free {}", boxed),
            | Instr::BoxAddr { ret, boxed } => write!(f, "{} = box_addr {}", ret, boxed),
            | Instr::Load { ret, addr } => write!(f, "{} = load {}", ret, addr),
            | Instr::Store { val, addr } => write!(f, "store {}, {}", val, addr),
            | Instr::CopyAddr { old, new, flags } if flags.is_set(Flags::TAKE) => write!(f, "copy_addr {}, {} [take]", old, new),
            | Instr::CopyAddr { old, new, flags } if flags.is_set(Flags::INIT) => write!(f, "copy_addr {}, {} [init]", old, new),
            | Instr::CopyAddr { old, new, .. } => write!(f, "copy_addr {}, {}", old, new),
            | Instr::CopyValue { ret, val } => write!(f, "{} = copy_value {}", ret, val),
            | Instr::DropAddr { addr } => write!(f, "drop_addr {}", addr),
            | Instr::DropValue { val } => write!(f, "drop_value {}", val),
            | Instr::ConstInt { ret, val } => write!(f, "{} = const_int {}, ${}", ret, val, self.body[*ret].ty.display(self.db)),
            | Instr::ConstStr { ret, val } => write!(f, "{} = const_str {:?}", ret, val),
            | Instr::FuncRef { ret, func } => write!(f, "{} = func_ref {}", ret, func),
            | Instr::Tuple { ret, vals } => {
                write!(f, "{} = tuple ", ret)?;
                list(f, vals, Var::fmt)
            },
            | Instr::TupleExtract { ret, tuple, field } => write!(f, "{} = tuple_extract {}, {}", ret, tuple, field),
            | Instr::TupleInsert { tuple, field, val } => write!(f, "tuple_field {}, {}, {}", tuple, field, val),
            | Instr::TupleAddr { ret, tuple, field } => write!(f, "{} = tuple_addr {}, {}", ret, tuple, field),
            | Instr::Apply { rets, func, args, subst } => {
                if !rets.is_empty() {
                    list(f, rets, Var::fmt)?;
                    write!(f, " = ")?;
                }

                write!(f, "apply {}", func)?;

                if !subst.is_empty() {
                    write!(f, "<")?;
                    list(f, subst, |s, f| s.display(self.db).fmt(f))?;
                    write!(f, ">")?;
                }

                write!(f, "(")?;
                list(f, args, Var::fmt)?;
                write!(f, ")")
            },
            | Instr::Intrinsic { rets, name, args, subst } => {
                if !rets.is_empty() {
                    list(f, rets, Var::fmt)?;
                    write!(f, " = ")?;
                }

                write!(f, "intrinsic {:?}", name)?;

                if !subst.is_empty() {
                    write!(f, "<")?;
                    list(f, subst, |s, f| s.display(self.db).fmt(f))?;
                    write!(f, ">")?;
                }

                write!(f, "(")?;
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

impl Ty {
    pub fn display<'a>(&'a self, db: &'a dyn IrDatabase) -> IrDisplay<'a, Self> {
        IrDisplay(self, db)
    }
}

impl fmt::Display for IrDisplay<'_, Ty> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use crate::layout::Primitive;
        let IrDisplay(ty, db) = *self;
        let ty = ty.lookup(db);

        if ty.flags.is_set(Flags::OWNED) {
            write!(f, "[owned] ")?;
        }

        if ty.flags.is_set(Flags::C_REPR) {
            write!(f, "[c_repr] ")?;
        }

        match ty.kind {
            | typ::Unit => match ty.repr.scalar {
                | Some(prim) => match prim {
                    | Primitive::Int(Integer::I8, true) => write!(f, "i8"),
                    | Primitive::Int(Integer::I16, true) => write!(f, "i16"),
                    | Primitive::Int(Integer::I32, true) => write!(f, "i32"),
                    | Primitive::Int(Integer::I64, true) => write!(f, "i64"),
                    | Primitive::Int(Integer::I128, true) => write!(f, "i128"),
                    | Primitive::Int(Integer::ISize, true) => write!(f, "isize"),
                    | Primitive::Int(Integer::I8, false) => write!(f, "u8"),
                    | Primitive::Int(Integer::I16, false) => write!(f, "u16"),
                    | Primitive::Int(Integer::I32, false) => write!(f, "u32"),
                    | Primitive::Int(Integer::I64, false) => write!(f, "u64"),
                    | Primitive::Int(Integer::I128, false) => write!(f, "u128"),
                    | Primitive::Int(Integer::ISize, false) => write!(f, "usize"),
                    | Primitive::F32 => write!(f, "f32"),
                    | Primitive::F64 => write!(f, "f64"),
                    | Primitive::Pointer => write!(f, "ptr"),
                },
                | None => write!(f, "()"),
            },
            | typ::Def(id, Some(ref subst)) => {
                write!(f, "{}<", id)?;
                list(f, subst, |s, f| s.display(db).fmt(f))?;
                write!(f, ">")
            },
            | typ::Def(id, None) => write!(f, "{}", id),
            | typ::Ptr(to) => write!(f, "*{}", to.display(db)),
            | typ::Box(to) => write!(f, "box {}", to.display(db)),
            | typ::Tuple(ref ts) => {
                write!(f, "(")?;
                list(f, ts, |t, f| t.display(db).fmt(f))?;
                write!(f, ")")
            },
            | typ::Array(of, len) => write!(f, "[{}]{}", len, of.display(db)),
            | typ::Var(tv) => tv.fmt(f),
            | typ::Func(ref sig) => sig.display(db).fmt(f),
            | typ::Generic(ref params, ty) => {
                write!(f, "<")?;

                list(f, params.iter().enumerate(), |(i, p), f| {
                    p.fmt(f)?;
                    write!(f, " {}", (b'A' + i as u8) as char)
                })?;

                write!(f, "> {}", ty.display(db))
            },
        }
    }
}

impl Signature {
    pub fn display<'a>(&'a self, db: &'a dyn IrDatabase) -> IrDisplay<'a, Self> {
        IrDisplay(self, db)
    }
}

impl fmt::Display for IrDisplay<'_, Signature> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let IrDisplay(this, db) = *self;

        write!(f, "(")?;
        list(f, this.params.iter(), |s, f| s.display(db).fmt(f))?;
        write!(f, ") -> ")?;

        if this.rets.len() == 1 {
            this.rets[0].display(db).fmt(f)
        } else {
            write!(f, "(")?;
            list(f, this.rets.iter(), |s, f| s.display(db).fmt(f))?;
            write!(f, ")")
        }
    }
}

impl SigParam {
    pub fn display<'a>(&'a self, db: &'a dyn IrDatabase) -> IrDisplay<'a, Self> {
        IrDisplay(self, db)
    }
}

impl fmt::Display for IrDisplay<'_, SigParam> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let IrDisplay(this, db) = *self;

        if this.flags.is_set(Flags::IN) {
            write!(f, "[in] ")?;
        }

        if this.flags.is_set(Flags::OUT) {
            write!(f, "[out] ")?;
        }

        this.ty.display(db).fmt(f)
    }
}

impl fmt::Display for GenericVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ch = (b'A' + self.1) as char;
        write!(f, "{}'{}", ch, self.0)
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

impl Subst {
    pub fn display<'a>(&'a self, db: &'a dyn IrDatabase) -> IrDisplay<'a, Self> {
        IrDisplay(self, db)
    }
}

impl fmt::Display for IrDisplay<'_, Subst> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let IrDisplay(this, db) = *self;

        match this {
            | Subst::Type(t) => write!(f, "${}", t.display(db)),
            | Subst::Figure(i) => write!(f, "{}", i),
            | Subst::Symbol(s) => write!(f, "{:?}", s),
        }
    }
}

fn list<'a, T>(f: &mut fmt::Formatter<'_>, ts: impl IntoIterator<Item = T>, fmt: impl Fn(T, &mut fmt::Formatter<'_>) -> fmt::Result) -> fmt::Result {
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
