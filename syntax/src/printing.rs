use crate::*;
use std::fmt::{Display, Formatter, Result};

impl<'t> Display for Package<'t> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "package {}", self.name)?;

        for (id, ext) in &self.externs {
            writeln!(f, "\n{}:\n{}", id, ext)?;
        }

        for (id, glob) in &self.globals {
            writeln!(f, "\n{}:\n{}", id, glob)?;
        }

        for (id, body) in &self.bodies {
            writeln!(f, "\n{}:\n{}", id, body)?;
        }

        Ok(())
    }
}

impl Display for ItemId {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "#{}", self.0)
    }
}

impl<'t> Display for Signature<'t> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "fn {} ({}) -> ({})", self.0, list(&self.1, ", "), list(&self.2, ", "))
    }
}

impl<'t> Display for Extern<'t> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Extern::Proc(name, sig) => write!(f, "extern {}: {};", name, sig),
            Extern::Global(name, ty) => write!(f, "extern {}: {};", name, ty),
        }
    }
}

impl<'t> Display for Global<'t> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.export {
            write!(f, "export ")?;
        }
        
        write!(f, "global {}: {};", self.name, self.ty)
    }
}

impl<'t> Display for Body<'t> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.export {
            write!(f, "export ")?;
        }
        
        writeln!(f, "fn {} {} ({}) -> ({}) {{", self.name, self.conv, list(self.args(), ", "), list(self.rets(), ", "))?;
        
        let mut printed = 0;
        
        for (_, local) in &self.locals {
            match &local.kind {
                LocalKind::Var | LocalKind::Tmp => {
                    writeln!(f, "{}", indent(local.to_string()))?;
                    printed += 1;
                },
                _ => {},
            }
        }
        
        if printed > 0 {
            writeln!(f)?;
        }
        
        for (_, block) in &self.blocks {
            writeln!(f, "{}", indent(block.to_string()))?;
        }
        
        write!(f, "}}")
    }
}

impl Display for LocalId {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "_{}", self.0)
    }
}

impl<'t> Display for Local<'t> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}: {}", self.id, self.ty)
    }
}

impl Display for BlockId {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "%{}", self.0)
    }
}

impl<'t> Display for Block<'t> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "{} {{", self.id)?;

        for stmt in &self.stmts {
            writeln!(f, "{}", indent(stmt.to_string()))?;
        }

        writeln!(f, "{}", indent(self.term.to_string()))?;
        write!(f, "}}")
    }
}

impl<'t> Display for Stmt<'t> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Stmt::Assign(place, value) => write!(f, "{} = {}", place, value)
        }
    }
}

impl<'t> Display for Terminator<'t> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Terminator::Unset => write!(f, ""),
            Terminator::Return => write!(f, "return"),
            Terminator::Jump(target) => write!(f, "jump {}", target),
            Terminator::Call(places, proc, args, target) => {
                if places.is_empty() {
                    write!(f, "call {}({}), {}", proc, list(args, ", "), target)
                } else {
                    write!(f, "call {} = {}({}), {}", list(places, ", "), proc, list(args, ", "), target)
                }
            },
            Terminator::Switch(pred, values, targets) => {
                write!(f, "switch {} [", pred)?;

                let mut values = values.iter();
                let mut targets = targets.iter();

                loop {
                    if let Some(value) = values.next() {
                        let target = targets.next().unwrap();

                        write!(f, "{:X}: {}, ", value, target)?;
                    } else {
                        break;
                    }
                }

                write!(f, "otherwise {}]", targets.next().unwrap())
            },
        }
    }
}

impl Display for Place {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for elem in &self.elems {
            if let PlaceElem::Deref = elem {
                write!(f, "(*")?;
            }
        }

        match &self.base {
            PlaceBase::Local(id) => <LocalId as Display>::fmt(id, f)?,
            PlaceBase::Global(id) => write!(f, "#!{}", id.0)?,
        }

        for elem in &self.elems {
            match elem {
                PlaceElem::Deref => write!(f, ")")?,
                PlaceElem::Field(n) => write!(f, ".{}", n)?,
                PlaceElem::Index(n) => write!(f, "[{}]", n)?,
                PlaceElem::ConstIndex(n) => write!(f, "[{}]", n)?,
            }
        }

        Ok(())
    }
}

impl<'t> Display for Operand<'t> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Operand::Place(place) => <Place as Display>::fmt(place, f),
            Operand::Constant(c) => <Const as Display>::fmt(c, f),
        }
    }
}

impl<'t> Display for Const<'t> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Const::Unit => write!(f, "unit"),
            Const::Scalar(value, ty) => write!(f, "{} {}", value, ty),
            Const::Bytes(bytes) => <str as Display>::fmt(std::str::from_utf8(&bytes).unwrap(), f),
            Const::FuncAddr(id) => <ItemId as Display>::fmt(id, f),
        }
    }
}

impl<'t> Display for Value<'t> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Value::Use(op) => <Operand as Display>::fmt(op, f),
            Value::Ref(place) => write!(f, "&{}", place),
            Value::Slice(arr, lo, hi) => write!(f, "{}[{}..{}]", arr, lo, hi),
            Value::Cast(ty, op) => write!(f, "cast {}, {}", ty, op),
            Value::BinOp(op, lhs, rhs) => write!(f, "{} {} {}", op, lhs, rhs),
            Value::UnOp(op, val) => write!(f, "{} {}", op, val),
            Value::NullOp(op, ty) => write!(f, "{} {}", op, ty),
            Value::Init(ty, ops) => write!(f, "{} {{ {} }}", ty, list(ops, ", ")),
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            BinOp::Add => write!(f, "add"),
            BinOp::Sub => write!(f, "sub"),
            BinOp::Mul => write!(f, "mul"),
            BinOp::Div => write!(f, "div"),
            BinOp::Rem => write!(f, "rem"),
            BinOp::Eq => write!(f, "eq"),
            BinOp::Ne => write!(f, "ne"),
            BinOp::Lt => write!(f, "lt"),
            BinOp::Le => write!(f, "le"),
            BinOp::Gt => write!(f, "gt"),
            BinOp::Ge => write!(f, "ge"),
            BinOp::BitAnd => write!(f, "band"),
            BinOp::BitOr => write!(f, "bor"),
            BinOp::BitXOr => write!(f, "bxor"),
            BinOp::Shl => write!(f, "shl"),
            BinOp::Shr => write!(f, "shr"),
        }
    }
}

impl Display for UnOp {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            UnOp::Neg => write!(f, "negate"),
            UnOp::Not => write!(f, "not"),
        }
    }
}

impl Display for NullOp {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            NullOp::SizeOf => write!(f, "sizeof"),
            NullOp::AlignOf => write!(f, "alignof"),
        }
    }
}

impl<'t> Display for Ty<'t> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        <Type as Display>::fmt(&*self, f)
    }
}

impl<'t> Display for Type<'t> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::Str => write!(f, "str"),
            Type::Ratio => write!(f, "ratio"),
            Type::Int(size) => match size {
                IntSize::Bits8 => write!(f, "i8"),
                IntSize::Bits16 => write!(f, "i16"),
                IntSize::Bits32 => write!(f, "i32"),
                IntSize::Bits64 => write!(f, "i64"),
                IntSize::Bits128 => write!(f, "i128"),
                IntSize::Size => write!(f, "isize"),
            },
            Type::UInt(size) => match size {
                IntSize::Bits8 => write!(f, "u8"),
                IntSize::Bits16 => write!(f, "u16"),
                IntSize::Bits32 => write!(f, "u32"),
                IntSize::Bits64 => write!(f, "u64"),
                IntSize::Bits128 => write!(f, "u128"),
                IntSize::Size => write!(f, "usize"),
            },
            Type::Float(size) => match size {
                FloatSize::Bits32 => write!(f, "f32"),
                FloatSize::Bits64 => write!(f, "f64"),
                FloatSize::Size => write!(f, "fsize"),
            },
            Type::Ref(to) => write!(f, "&{}", to),
            Type::Array(of, len) => write!(f, "[{}; {}]", of, len),
            Type::Slice(of) => write!(f, "[{}]", of),
            Type::Vector(of, len) => write!(f, "{{{}; {}}}", of, len),
            Type::Proc(sig) => <Signature as Display>::fmt(sig, f),
            Type::Tuple(packed, types) => if *packed {
                write!(f, "<{}>", list(types, ", "))
            } else {
                write!(f, "({})", list(types, ", "))
            },
            Type::Union(tagged, types) => if *tagged {
                write!(f, "{}", list(types, " / "))
            } else {
                write!(f, "{}", list(types, " | "))
            },
            Type::Tagged(idx, ty) => write!(f, "/{} {}", idx, ty),
        }
    }
}

impl Display for CallConv {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            CallConv::C => write!(f, r#""C""#),
            CallConv::Fluix => write!(f, r#""Fluix""#),
        }
    }
}

fn list<T: Display, I: IntoIterator<Item=T>>(i: I, sep: &str) -> String {
    i.into_iter().map(|t| t.to_string()).collect::<Vec<_>>().join(sep)
}

fn indent(s: String) -> String {
    s.lines().map(|l| format!("    {}", l)).collect::<Vec<_>>().join("\n")
}
