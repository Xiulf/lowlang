mod parsing;

pub use parser::ident::Ident;
pub use parser::parse;
use fluix_encode::{Encodable, Decodable};
use std::fmt;

#[derive(Encodable, Decodable, Debug, Clone)]
pub struct Program {
    pub fns: Vec<Function>
}

#[derive(Encodable, Decodable, Debug, Clone)]
pub struct Function {
    pub name: Ident,
    pub params: Vec<(LocalId, Type)>,
    pub ret: Type,
    pub bindings: Vec<(LocalId, Type)>,
    pub blocks: Vec<BasicBlock>,
}

#[derive(Encodable, Decodable, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Encodable, Decodable, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub usize);

#[derive(Encodable, Decodable, Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}

#[derive(Encodable, Decodable, Debug, Clone)]
pub enum Statement {
    Assign(Place, RValue),
    StorageLive(LocalId),
    StorageDead(LocalId),
}

#[derive(Encodable, Decodable, Debug, Clone)]
pub enum Terminator {
    Goto(BlockId),
    Resume,
    Abort,
    Return,
    Unreachable,
    Call(Operand, Vec<Operand>, Option<(Place, BlockId)>, Option<BlockId>),
    Assert(Operand, bool, BlockId, Option<BlockId>),
}

#[derive(Encodable, Decodable, Debug, Clone)]
pub struct Place {
    pub base: PlaceBase,
    pub projection: Vec<PlaceElem>,
}

#[derive(Encodable, Decodable, Debug, Clone)]
pub enum PlaceBase {
    Local(LocalId),
}

#[derive(Encodable, Decodable, Debug, Clone)]
pub enum PlaceElem {
    Deref,
    Field(usize),
}

#[derive(Encodable, Decodable, Debug, Clone)]
pub enum Operand {
    Copy(Place),
    Move(Place),
    Constant(Constant),
}

#[derive(Encodable, Decodable, Debug, Clone)]
pub enum RValue {
    Use(Operand),
    Ref(Place),
    Binary(BinOp, Operand, Operand),
    Unary(UnOp, Operand),
    Tuple(Vec<Operand>),
    Box(Type),
}

#[derive(Encodable, Decodable, Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

#[derive(Encodable, Decodable, Debug, Clone)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Encodable, Decodable, Debug, Clone)]
pub enum Constant {
    Int(i64, IntTy),
    UInt(u64, UIntTy),
    Float(f64, FloatTy),
    Bool(bool),
    Bytes(Box<[u8]>),
    Item(Ident),
    Tuple(Vec<Constant>),
}

#[derive(Encodable, Decodable, Debug, Clone)]
pub enum Type {
    Unit,
    Tuple(Vec<Type>),
    Int(IntTy),
    UInt(UIntTy),
    Float(FloatTy),
    Bool,
}

#[derive(Encodable, Decodable, Debug, Clone)]
pub enum IntTy {
    I8, I16, I32, I64
}

#[derive(Encodable, Decodable, Debug, Clone)]
pub enum UIntTy {
    U8, U16, U32, U64
}

#[derive(Encodable, Decodable, Debug, Clone)]
pub enum FloatTy {
    F32, F64,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Unit => 0,
            Type::Tuple(t) => t.iter().fold(0, |acc, t| acc + t.size()),
            Type::Int(IntTy::I8) => 1,
            Type::Int(IntTy::I16) => 2,
            Type::Int(IntTy::I32) => 4,
            Type::Int(IntTy::I64) => 8,
            Type::UInt(UIntTy::U8) => 1,
            Type::UInt(UIntTy::U16) => 2,
            Type::UInt(UIntTy::U32) => 4,
            Type::UInt(UIntTy::U64) => 8,
            Type::Float(FloatTy::F32) => 1,
            Type::Float(FloatTy::F64) => 1,
            Type::Bool => 1,
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for i in 0..self.fns.len() {
            if i == self.fns.len() - 1 {
                write!(f, "{}", self.fns[i])?;
            } else {
                writeln!(f, "{}\n", self.fns[i])?;
            }
        }
        
        Ok(())
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params = self.params.iter().map(|p| format!("{}: {}", p.0, p.1)).collect::<Vec<_>>().join(", ");
        
        writeln!(f, "fn {}({}) -> {} {{", self.name, params, self.ret)?;
        
        for binding in &self.bindings {
            writeln!(f, "    let {}: {};", binding.0, binding.1)?;
        }
        
        writeln!(f)?;
        
        fn indent(s: String) -> String {
            s.lines().map(|l| format!("    {}", l)).collect::<Vec<_>>().join("\n")
        }
        
        for i in 0..self.blocks.len() {
            if i != self.blocks.len() - 1 {
                writeln!(f, "{}\n", indent(self.blocks[i].to_string()))?;
            } else {
                writeln!(f, "{}", indent(self.blocks[i].to_string()))?;
            }
        }
        
        write!(f, "}}")
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl fmt::Display for LocalId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}: {{", self.id)?;
        
        for stmt in &self.statements {
            writeln!(f, "    {}", stmt)?;
        }
        
        writeln!(f, "    {}", self.terminator)?;
        write!(f, "}}")
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Assign(l, r) => write!(f, "{} = {};", l, r),
            Statement::StorageLive(l) => write!(f, "StorageLive({});", l),
            Statement::StorageDead(l) => write!(f, "StorageDead({});", l),
        }
    }
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Terminator::Goto(i) => write!(f, "goto({})", i),
            Terminator::Return => write!(f, "return"),
            Terminator::Resume => write!(f, "resume"),
            Terminator::Abort => write!(f, "abort"),
            Terminator::Unreachable => write!(f, "unreachable"),
            Terminator::Call(func, args, a, b) => {
                write!(f, "call(")?;
                
                if let Some((dest, _)) = a {
                    write!(f, "{} = ", dest)?;
                }
                
                let args = args.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", ");
                
                write!(f, "{}({})", func, args)?;
                
                if let Some((_, next)) = a {
                    write!(f, ", goto {}", next)?;
                }
                
                if let Some(fail) = b {
                    write!(f, ", unwind {}", fail)?;
                }
                
                write!(f, ")")
            },
            Terminator::Assert(cond, expected, next, fail) => {
                write!(f, "assert({}{}, goto {}", if *expected { "" } else { "!" }, cond, next)?;
                
                if let Some(fail) = fail {
                    write!(f, ", unwind {})", fail)
                } else {
                    write!(f, ")")
                }
            },
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Move(p) => write!(f, "move {}", p),
            Operand::Copy(p) => write!(f, "{}", p),
            Operand::Constant(c) => write!(f, "const {}", c),
        }
    }
}

impl fmt::Display for Place {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for elem in self.projection.iter().rev() {
            match elem {
                PlaceElem::Deref => write!(f, "(*")?,
                PlaceElem::Field(_) => write!(f, "(")?,
            }
        }
        
        write!(f, "{}", self.base)?;
        
        for elem in self.projection.iter() {
            match elem {
                PlaceElem::Deref => write!(f, ")")?,
                PlaceElem::Field(i) => write!(f, ">{})", i)?,
            }
        }
        
        Ok(())
    }
}

impl fmt::Display for PlaceBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PlaceBase::Local(l) => l.fmt(f),
        }
    }
}

impl fmt::Display for RValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RValue::Use(v) => v.fmt(f),
            RValue::Ref(v) => write!(f, "&{}", v),
            RValue::Box(t) => write!(f, "box {}", t),
            RValue::Binary(o, l, r) => write!(f, "{:?}({}, {})", o, l, r),
            RValue::Unary(o, v) => write!(f, "{:?}({})", o, v),
            RValue::Tuple(t) => {
                let t = t.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ");
                
                write!(f, "({})", t)
            }
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Int(i, t) => write!(f, "{}{}", i, t),
            Constant::UInt(i, t) => write!(f, "{}{}", i, t),
            Constant::Float(i, t) => write!(f, "{}{}", i, t),
            Constant::Bool(b) => b.fmt(f),
            Constant::Bytes(b) => write!(f, "{}", std::str::from_utf8(b).unwrap()),
            Constant::Item(i) => i.fmt(f),
            Constant::Tuple(t) => {
                let t = t.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ");
                
                write!(f, "({})", t)
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Int(i) => i.fmt(f),
            Type::UInt(i) => i.fmt(f),
            Type::Float(i) => i.fmt(f),
            Type::Unit => write!(f, "()"),
            Type::Bool => write!(f, "bool"),
            Type::Tuple(t) => {
                let t = t.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ");
                
                write!(f, "({})", t)
            }
        }
    }
}

impl fmt::Display for IntTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IntTy::I8 => write!(f, "i8"),
            IntTy::I16 => write!(f, "i16"),
            IntTy::I32 => write!(f, "i32"),
            IntTy::I64 => write!(f, "i64"),
        }
    }
}

impl fmt::Display for UIntTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UIntTy::U8 => write!(f, "u8"),
            UIntTy::U16 => write!(f, "u16"),
            UIntTy::U32 => write!(f, "u32"),
            UIntTy::U64 => write!(f, "u64"),
        }
    }
}

impl fmt::Display for FloatTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FloatTy::F32 => write!(f, "f32"),
            FloatTy::F64 => write!(f, "f64"),
        }
    }
}