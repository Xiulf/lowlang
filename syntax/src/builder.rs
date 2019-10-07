use crate::*;
use diagnostics::Span;
use std::collections::BTreeMap;

pub struct Builder {
    functions: BTreeMap<String, Function>,
}

pub struct FunctionBuilder<'a> {
    local_id: usize,
    function: &'a mut Function,
    block: Option<BlockId>,
}

pub struct Ins<'a> {
    block: &'a mut BasicBlock,
}

#[macro_export]
macro_rules! place {
    (@rec [* $($rest:tt)+]; $proj:expr) => {{
        $proj.push($crate::PlaceElem::Deref(Default::default()));
        place!(@rec [$($rest)+]; $proj)
    }};
    
    (@rec [$($rest:tt)+ > $num:literal]; $proj:expr) => {{
        $proj.push($crate::PlaceElem::Field($num, Default::default()));
        place!(@rec [$($rest)+]; $proj)
    }};
    
    (@rec [$name:ident]; $proj:expr) => {
        $crate::Place {
            base: $crate::PlaceBase::Local($name, Default::default()),
            projection: $proj,
            span: Default::default(),
        }
    };
    
    ($($input:tt)+) => {
        place!(@rec [$($input)+]; Vec::new())
    };
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            functions: BTreeMap::new()
        }
    }
    
    pub fn create_function<'a>(&'a mut self, name: String, span: Span, ret: impl Into<Type>) -> FunctionBuilder<'a> {
        let ret = ret.into();
        
        self.functions.insert(name.clone(), Function {
            name: Ident {
                text: name.clone(),
                span,
            },
            params: Vec::new(),
            ret: ret.clone(),
            bindings: Vec::new(),
            blocks: Vec::new()
        });
        
        FunctionBuilder {
            local_id: 0,
            function: self.functions.get_mut(&name).unwrap(),
            block: None
        }
    }
    
    pub fn get_function(&self, name: &str) -> Option<&Function> {
        self.functions.get(name)
    }
}

impl<'a> FunctionBuilder<'a> {
    pub fn add_param(&mut self, ty: impl Into<Type>) -> LocalId {
        let id = LocalId(self.local_id);
        
        self.local_id += 1;
        self.function.params.push((id, ty.into()));
        
        id
    }
    
    pub fn add_local(&mut self, ty: impl Into<Type>) -> LocalId {
        let id = LocalId(self.local_id);
        
        self.local_id += 1;
        self.function.bindings.push((id, ty.into()));
        
        id
    }
    
    pub fn create_bb(&mut self) -> BlockId {
        let id = BlockId(self.function.blocks.len());
        
        self.function.blocks.push(BasicBlock {
            id,
            statements: Vec::new(),
            terminator: Terminator::Unreachable,
        });
        
        id
    }
    
    pub fn use_bb(&mut self, id: BlockId) {
        self.block = Some(id);
    }
    
    pub fn ins<'b>(&'b mut self) -> Ins<'b> {
        let mut block = None;
        
        for bb in &mut self.function.blocks {
            if Some(bb.id) == self.block {
                block = Some(bb);
                
                break;
            }
        }
        
        Ins {
            block: block.unwrap()
        }
    }
    
    pub fn copy(&self, p: Place, span: Span) -> Operand {
        Operand::Copy(p, span)
    }
    
    pub fn move_(&self, p: Place, span: Span) -> Operand {
        Operand::Move(p, span)
    }
    
    pub fn const_(&self, c: Constant, span: Span) -> Operand {
        Operand::Constant(c, span)
    }
    
    pub fn tuple(&self, vals: Vec<Constant>) -> Constant {
        Constant::Tuple(vals)
    }
    
    pub fn item(&self, name: Ident) -> Constant {
        Constant::Item(name)
    }
    
    pub fn u8(&self, val: u8) -> Constant {
        Constant::UInt(val as u64, UIntTy::U8)
    }
    
    pub fn u16(&self, val: u16) -> Constant {
        Constant::UInt(val as u64, UIntTy::U16)
    }
    
    pub fn u32(&self, val: u32) -> Constant {
        Constant::UInt(val as u64, UIntTy::U32)
    }
    
    pub fn u64(&self, val: u64) -> Constant {
        Constant::UInt(val as u64, UIntTy::U64)
    }
    
    pub fn i8(&self, val: i8) -> Constant {
        Constant::Int(val as i64, IntTy::I8)
    }
    
    pub fn i16(&self, val: i16) -> Constant {
        Constant::Int(val as i64, IntTy::I16)
    }
    
    pub fn i32(&self, val: i32) -> Constant {
        Constant::Int(val as i64, IntTy::I32)
    }
    
    pub fn i64(&self, val: i64) -> Constant {
        Constant::Int(val as i64, IntTy::I64)
    }
    
    pub fn f32(&self, val: f32) -> Constant {
        Constant::Float(val as f64, FloatTy::F32)
    }
    
    pub fn f64(&self, val: f64) -> Constant {
        Constant::Float(val as f64, FloatTy::F64)
    }
    
    pub fn bool(&self, val: bool) -> Constant {
        Constant::Bool(val)
    }
}

impl<'a> Ins<'a> {
    pub fn return_(&mut self) {
        self.block.terminator = Terminator::Return;
    }
    
    pub fn abort(&mut self) {
        self.block.terminator = Terminator::Abort;
    }
    
    pub fn unreachable(&mut self) {
        self.block.terminator = Terminator::Unreachable;
    }
    
    pub fn goto(&mut self, block: BlockId) {
        self.block.terminator = Terminator::Goto(block);
    }
    
    pub fn call(&mut self, func: Operand, args: Vec<Operand>, unwind: Option<BlockId>) {
        self.block.terminator = Terminator::Call(func, args, None, unwind);
    }
    
    pub fn call_assign(&mut self, func: Operand, args: Vec<Operand>, to: Place, goto: BlockId, unwind: Option<BlockId>) {
        self.block.terminator = Terminator::Call(func, args, Some((to, goto)), unwind);
    }
    
    pub fn assert(&mut self, condition: Operand, expected: bool, goto: BlockId, unwind: Option<BlockId>) {
        self.block.terminator = Terminator::Assert(condition, expected, goto, unwind);
    }
    
    pub fn init(&mut self, var: LocalId) {
        self.block.statements.push(Statement::StorageLive(var));
    }
    
    pub fn drop(&mut self, var: LocalId) {
        self.block.statements.push(Statement::StorageDead(var));
    }
    
    pub fn free(&mut self, p: Place) {
        self.block.statements.push(Statement::Free(p));
    }
    
    pub fn use_(&mut self, tmp: Place, op: Operand) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Use(op)));
    }
    
    pub fn ref_(&mut self, tmp: Place, to: Place) {
        let span = to.span;
        
        self.block.statements.push(Statement::Assign(tmp, RValue::Ref(to, span)));
    }
    
    pub fn tuple(&mut self, tmp: Place, ops: Vec<Operand>, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Tuple(ops, span)));
    }
    
    pub fn alloc(&mut self, tmp: Place, len: Operand, ty: Type, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Alloc(len, ty, span)));
    }
    
    pub fn add(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::Add, lhs, rhs, span)));
    }
    
    pub fn sub(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::Sub, lhs, rhs, span)));
    }
    
    pub fn mul(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::Mul, lhs, rhs, span)));
    }
    
    pub fn div(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::Div, lhs, rhs, span)));
    }
    
    pub fn rem(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::Mod, lhs, rhs, span)));
    }
    
    pub fn lt(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::Lt, lhs, rhs, span)));
    }
    
    pub fn le(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::Le, lhs, rhs, span)));
    }
    
    pub fn gt(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::Gt, lhs, rhs, span)));
    }
    
    pub fn ge(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::Ge, lhs, rhs, span)));
    }
    
    pub fn eq(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::Eq, lhs, rhs, span)));
    }
    
    pub fn ne(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::Ne, lhs, rhs, span)));
    }
    
    pub fn bit_and(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::BitAnd, lhs, rhs, span)));
    }
    
    pub fn bit_or(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::BitOr, lhs, rhs, span)));
    }
    
    pub fn bit_xor(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::BitXor, lhs, rhs, span)));
    }
    
    pub fn shl(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::Shl, lhs, rhs, span)));
    }
    
    pub fn shr(&mut self, tmp: Place, lhs: Operand, rhs: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Binary(BinOp::Shr, lhs, rhs, span)));
    }
    
    pub fn not(&mut self, tmp: Place, op: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Unary(UnOp::Not, op, span)));
    }
    
    pub fn neg(&mut self, tmp: Place, op: Operand, span: Span) {
        self.block.statements.push(Statement::Assign(tmp, RValue::Unary(UnOp::Neg, op, span)));
    }
}