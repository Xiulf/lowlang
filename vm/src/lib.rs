pub mod memory;

use syntax::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct VM {
    pub memory: memory::Memory,
    frames: Vec<StackFrame>,
    fns: HashMap<String, Function>,
}

#[derive(Debug)]
struct StackFrame {
    sizes: HashMap<LocalId, Type>,
    locals: HashMap<LocalId, usize>,
    block: BlockId,
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Unit,
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Ptr(u32, u32),
    Tuple(Vec<Value>),
}

impl VM {
    pub fn new(program: Program) -> VM {
        let memory = memory::Memory::new();
        let frames = Vec::new();
        let fns: HashMap<String, Function> = program.fns.into_iter().map(|f| (f.name.text.clone(), f)).collect();
        
        VM {
            memory,
            frames,
            fns,
        }
    }
    
    pub fn run(&mut self) -> Option<usize> {
        let f = self.fns["main"].clone();
        
        self.frames.push(StackFrame {
            locals: HashMap::new(),
            sizes: f.bindings.iter().cloned().collect(),
            block: BlockId(0),
        });
        
        if let Some(val) = self.run_fn(f) {
            Some(val as usize)
        } else {
            None
        }
    }
    
    fn run_fn(&mut self, f: Function) -> Option<usize> {
        // init return memory
        let loc = self.memory.alloc(self.frame().sizes[&LocalId(0)].size());
        
        self.frame_mut().locals.insert(LocalId(0), loc);
        
        loop {
            let block = self.block(&f);
            
            for stmt in block.statements {
                match stmt {
                    Statement::StorageLive(id) => {
                        let loc = self.memory.alloc(self.frame().sizes[&id].size());
                        
                        self.frame_mut().locals.insert(id, loc);
                    },
                    Statement::StorageDead(id) => {
                        self.drop(id);
                        self.frame_mut().locals.remove(&id);
                        self.frame_mut().sizes.remove(&id);
                    },
                    Statement::Free(p) => {
                        let (loc, _) = self.place(p);
                        let size = self.memory.read_u32(loc + 4);
                        let loc = self.memory.read_u32(loc);
                        
                        self.memory.free(loc as usize, size as usize);
                    },
                    Statement::Assign(place, value) => {
                        let (loc, _) = self.place(place.clone());
                        let val = self.rvalue(value);
                        
                        self.memory.write(loc, val);
                    },
                }
            }
            
            match block.terminator {
                Terminator::Return => return Some(self.memory.read(
                    loc,
                    self.frame().sizes[&f.bindings[0].0].size()
                ) as usize),
                Terminator::Unreachable => unreachable!(),
                Terminator::Goto(id) => self.frame_mut().block = id,
                Terminator::Abort => {
                    self.drop(LocalId(0));
                    
                    return None;
                },
                Terminator::Resume => {
                    
                },
                Terminator::Call(f, args, goto, fail) => {
                    let f = if let Value::U32(v) = self.operand(f) { v as usize } else { panic!("type error"); };
                    let f = self.fns.iter().nth(f).unwrap().1.clone();
                    let mut frame = StackFrame {
                        locals: HashMap::new(),
                        sizes: f.bindings.iter().cloned().collect(),
                        block: BlockId(0),
                    };
                    
                    // init params
                    for ((id, ty), arg) in f.params.iter().zip(args.iter()) {
                        let size = ty.size();
                        let loc = self.memory.alloc(size);
                        
                        frame.sizes.insert(*id, ty.clone());
                        frame.locals.insert(*id, loc);
                        
                        let val = self.operand(arg.clone());
                        
                        self.memory.write(loc, val);
                    }
                    
                    let params_iter = f.params.clone();
                    
                    self.frames.push(frame);
                    
                    let val = self.run_fn(f);
                    let frame = self.frames.pop().unwrap();
                    
                    let drop_params = |this: &mut Self| {
                        for (id, _) in params_iter.into_iter().rev() {
                            this.drop_frame(id, &frame);
                        }
                    };
                    
                    match (goto, fail) {
                        (Some((place, next)), Some(fail)) => {
                            if let Some(val) = val {
                                let (loc, ty) = self.place(place);
                                let bytes = val.to_le_bytes();
                                
                                for i in 0..ty.size() { self.memory.data[loc + i] = bytes[i]; } 
                                
                                self.drop_frame(LocalId(0), &frame);
                                drop_params(self);
                                self.frame_mut().block = next;
                            } else {
                                drop_params(self);
                                self.frame_mut().block = fail;
                            }
                        },
                        (Some((place, next)), None) => {
                            if let Some(val) = val {
                                let (loc, ty) = self.place(place);
                                let bytes = val.to_le_bytes();
                                
                                for i in 0..ty.size() { self.memory.data[loc + i] = bytes[i]; } 
                                
                                self.drop_frame(LocalId(0), &frame);
                                drop_params(self);
                                self.frame_mut().block = next;
                            } else {
                                drop_params(self);
                                
                                return None;
                            }
                        },
                        (None, Some(fail)) => {
                            if let None = val {
                                drop_params(self);
                                self.frame_mut().block = fail;
                            } else {
                                drop_params(self);
                                
                                return Some(loc);
                            }
                        },
                        (None, None) => {
                            if let None = val {
                                drop_params(self);
                                
                                return None;
                            } else {
                                drop_params(self);
                                
                                return Some(loc);
                            }
                        }
                    }
                },
                Terminator::Assert(op, expected, success, fail) => {
                    let op = self.operand(op);
                    
                    if let Value::U8(v) = op {
                        if (v != 0) == expected {
                            self.frame_mut().block = success;
                        } else {
                            if let Some(fail) = fail {
                                self.frame_mut().block = fail;
                            } else {
                                return None;
                            }
                        }
                    } else {
                        panic!("type error");
                    }
                }
            }
        }
    }
    
    fn place(&mut self, p: Place) -> (usize, Type) {
        let (mut loc, mut ty) = match p.base {
            PlaceBase::Local(id) => (self.frame().locals[&id], self.frame().sizes[&id].clone())
        };
        
        for proj in p.projection.into_iter().rev() {
            match proj {
                PlaceElem::Field(i) => match &ty {
                    Type::Tuple(tys) => {
                        for j in 0..i { loc += tys[j].size(); }
                        
                        ty = tys[i].clone();
                    },
                    _ => panic!("type error")
                },
                PlaceElem::Deref => match &ty {
                    Type::Ptr(t) => {
                        loc = self.memory.read_u32(loc) as usize;
                        ty = *t.clone();
                    },
                    _ => panic!("type error")
                },
            }
        }
        
        (loc, ty)
    }
    
    fn rvalue(&mut self, v: RValue) -> Value {
        match v {
            RValue::Use(op) => self.operand(op),
            RValue::Binary(op, lhs, rhs) => {
                let lhs = self.operand(lhs);
                let rhs = self.operand(rhs);
                
                match (lhs, rhs, op) {
                    (lhs, rhs, BinOp::Eq) => Value::U8((lhs == rhs) as u8),
                    (lhs, rhs, BinOp::Ne) => Value::U8((lhs != rhs) as u8),
                    // #region binary operations
                    (Value::U8(lhs), Value::U8(rhs), BinOp::Add) => Value::U8(lhs + rhs),
                    (Value::U8(lhs), Value::U8(rhs), BinOp::Sub) => Value::U8(lhs - rhs),
                    (Value::U8(lhs), Value::U8(rhs), BinOp::Mul) => Value::U8(lhs * rhs),
                    (Value::U8(lhs), Value::U8(rhs), BinOp::Div) => Value::U8(lhs / rhs),
                    (Value::U8(lhs), Value::U8(rhs), BinOp::Mod) => Value::U8(lhs % rhs),
                    (Value::U8(lhs), Value::U8(rhs), BinOp::Lt) => Value::U8((lhs < rhs) as u8),
                    (Value::U8(lhs), Value::U8(rhs), BinOp::Le) => Value::U8((lhs <= rhs) as u8),
                    (Value::U8(lhs), Value::U8(rhs), BinOp::Gt) => Value::U8((lhs > rhs) as u8),
                    (Value::U8(lhs), Value::U8(rhs), BinOp::Ge) => Value::U8((lhs >= rhs) as u8),
                    (Value::U8(lhs), Value::U8(rhs), BinOp::BitAnd) => Value::U8(lhs & rhs),
                    (Value::U8(lhs), Value::U8(rhs), BinOp::BitOr) => Value::U8(lhs | rhs),
                    (Value::U8(lhs), Value::U8(rhs), BinOp::BitXor) => Value::U8(lhs ^ rhs),
                    (Value::U8(lhs), Value::U8(rhs), BinOp::Shl) => Value::U8(lhs << rhs),
                    (Value::U8(lhs), Value::U8(rhs), BinOp::Shr) => Value::U8(lhs >> rhs),
                    (Value::U16(lhs), Value::U16(rhs), BinOp::Add) => Value::U16(lhs + rhs),
                    (Value::U16(lhs), Value::U16(rhs), BinOp::Sub) => Value::U16(lhs - rhs),
                    (Value::U16(lhs), Value::U16(rhs), BinOp::Mul) => Value::U16(lhs * rhs),
                    (Value::U16(lhs), Value::U16(rhs), BinOp::Div) => Value::U16(lhs / rhs),
                    (Value::U16(lhs), Value::U16(rhs), BinOp::Mod) => Value::U16(lhs % rhs),
                    (Value::U16(lhs), Value::U16(rhs), BinOp::Lt) => Value::U8((lhs < rhs) as u8),
                    (Value::U16(lhs), Value::U16(rhs), BinOp::Le) => Value::U8((lhs <= rhs) as u8),
                    (Value::U16(lhs), Value::U16(rhs), BinOp::Gt) => Value::U8((lhs > rhs) as u8),
                    (Value::U16(lhs), Value::U16(rhs), BinOp::Ge) => Value::U8((lhs >= rhs) as u8),
                    (Value::U16(lhs), Value::U16(rhs), BinOp::BitAnd) => Value::U16(lhs & rhs),
                    (Value::U16(lhs), Value::U16(rhs), BinOp::BitOr) => Value::U16(lhs | rhs),
                    (Value::U16(lhs), Value::U16(rhs), BinOp::BitXor) => Value::U16(lhs ^ rhs),
                    (Value::U16(lhs), Value::U16(rhs), BinOp::Shl) => Value::U16(lhs << rhs),
                    (Value::U16(lhs), Value::U16(rhs), BinOp::Shr) => Value::U16(lhs >> rhs),
                    (Value::U32(lhs), Value::U32(rhs), BinOp::Add) => Value::U32(lhs + rhs),
                    (Value::U32(lhs), Value::U32(rhs), BinOp::Sub) => Value::U32(lhs - rhs),
                    (Value::U32(lhs), Value::U32(rhs), BinOp::Mul) => Value::U32(lhs * rhs),
                    (Value::U32(lhs), Value::U32(rhs), BinOp::Div) => Value::U32(lhs / rhs),
                    (Value::U32(lhs), Value::U32(rhs), BinOp::Mod) => Value::U32(lhs % rhs),
                    (Value::U32(lhs), Value::U32(rhs), BinOp::Lt) => Value::U8((lhs < rhs) as u8),
                    (Value::U32(lhs), Value::U32(rhs), BinOp::Le) => Value::U8((lhs <= rhs) as u8),
                    (Value::U32(lhs), Value::U32(rhs), BinOp::Gt) => Value::U8((lhs > rhs) as u8),
                    (Value::U32(lhs), Value::U32(rhs), BinOp::Ge) => Value::U8((lhs >= rhs) as u8),
                    (Value::U32(lhs), Value::U32(rhs), BinOp::BitAnd) => Value::U32(lhs & rhs),
                    (Value::U32(lhs), Value::U32(rhs), BinOp::BitOr) => Value::U32(lhs | rhs),
                    (Value::U32(lhs), Value::U32(rhs), BinOp::BitXor) => Value::U32(lhs ^ rhs),
                    (Value::U32(lhs), Value::U32(rhs), BinOp::Shl) => Value::U32(lhs << rhs),
                    (Value::U32(lhs), Value::U32(rhs), BinOp::Shr) => Value::U32(lhs >> rhs),
                    (Value::U64(lhs), Value::U64(rhs), BinOp::Add) => Value::U64(lhs + rhs),
                    (Value::U64(lhs), Value::U64(rhs), BinOp::Sub) => Value::U64(lhs - rhs),
                    (Value::U64(lhs), Value::U64(rhs), BinOp::Mul) => Value::U64(lhs * rhs),
                    (Value::U64(lhs), Value::U64(rhs), BinOp::Div) => Value::U64(lhs / rhs),
                    (Value::U64(lhs), Value::U64(rhs), BinOp::Mod) => Value::U64(lhs % rhs),
                    (Value::U64(lhs), Value::U64(rhs), BinOp::Lt) => Value::U8((lhs < rhs) as u8),
                    (Value::U64(lhs), Value::U64(rhs), BinOp::Le) => Value::U8((lhs <= rhs) as u8),
                    (Value::U64(lhs), Value::U64(rhs), BinOp::Gt) => Value::U8((lhs > rhs) as u8),
                    (Value::U64(lhs), Value::U64(rhs), BinOp::Ge) => Value::U8((lhs >= rhs) as u8),
                    (Value::U64(lhs), Value::U64(rhs), BinOp::BitAnd) => Value::U64(lhs & rhs),
                    (Value::U64(lhs), Value::U64(rhs), BinOp::BitOr) => Value::U64(lhs | rhs),
                    (Value::U64(lhs), Value::U64(rhs), BinOp::BitXor) => Value::U64(lhs ^ rhs),
                    (Value::U64(lhs), Value::U64(rhs), BinOp::Shl) => Value::U64(lhs << rhs),
                    (Value::U64(lhs), Value::U64(rhs), BinOp::Shr) => Value::U64(lhs >> rhs),
                    (Value::I8(lhs), Value::I8(rhs), BinOp::Add) => Value::I8(lhs + rhs),
                    (Value::I8(lhs), Value::I8(rhs), BinOp::Sub) => Value::I8(lhs - rhs),
                    (Value::I8(lhs), Value::I8(rhs), BinOp::Mul) => Value::I8(lhs * rhs),
                    (Value::I8(lhs), Value::I8(rhs), BinOp::Div) => Value::I8(lhs / rhs),
                    (Value::I8(lhs), Value::I8(rhs), BinOp::Mod) => Value::I8(lhs % rhs),
                    (Value::I8(lhs), Value::I8(rhs), BinOp::Lt) => Value::U8((lhs < rhs) as u8),
                    (Value::I8(lhs), Value::I8(rhs), BinOp::Le) => Value::U8((lhs <= rhs) as u8),
                    (Value::I8(lhs), Value::I8(rhs), BinOp::Gt) => Value::U8((lhs > rhs) as u8),
                    (Value::I8(lhs), Value::I8(rhs), BinOp::Ge) => Value::U8((lhs >= rhs) as u8),
                    (Value::I8(lhs), Value::I8(rhs), BinOp::BitAnd) => Value::I8(lhs & rhs),
                    (Value::I8(lhs), Value::I8(rhs), BinOp::BitOr) => Value::I8(lhs | rhs),
                    (Value::I8(lhs), Value::I8(rhs), BinOp::BitXor) => Value::I8(lhs ^ rhs),
                    (Value::I8(lhs), Value::I8(rhs), BinOp::Shl) => Value::I8(lhs << rhs),
                    (Value::I8(lhs), Value::I8(rhs), BinOp::Shr) => Value::I8(lhs >> rhs),
                    (Value::I16(lhs), Value::I16(rhs), BinOp::Add) => Value::I16(lhs + rhs),
                    (Value::I16(lhs), Value::I16(rhs), BinOp::Sub) => Value::I16(lhs - rhs),
                    (Value::I16(lhs), Value::I16(rhs), BinOp::Mul) => Value::I16(lhs * rhs),
                    (Value::I16(lhs), Value::I16(rhs), BinOp::Div) => Value::I16(lhs / rhs),
                    (Value::I16(lhs), Value::I16(rhs), BinOp::Mod) => Value::I16(lhs % rhs),
                    (Value::I16(lhs), Value::I16(rhs), BinOp::Lt) => Value::U8((lhs < rhs) as u8),
                    (Value::I16(lhs), Value::I16(rhs), BinOp::Le) => Value::U8((lhs <= rhs) as u8),
                    (Value::I16(lhs), Value::I16(rhs), BinOp::Gt) => Value::U8((lhs > rhs) as u8),
                    (Value::I16(lhs), Value::I16(rhs), BinOp::Ge) => Value::U8((lhs >= rhs) as u8),
                    (Value::I16(lhs), Value::I16(rhs), BinOp::BitAnd) => Value::I16(lhs & rhs),
                    (Value::I16(lhs), Value::I16(rhs), BinOp::BitOr) => Value::I16(lhs | rhs),
                    (Value::I16(lhs), Value::I16(rhs), BinOp::BitXor) => Value::I16(lhs ^ rhs),
                    (Value::I16(lhs), Value::I16(rhs), BinOp::Shl) => Value::I16(lhs << rhs),
                    (Value::I16(lhs), Value::I16(rhs), BinOp::Shr) => Value::I16(lhs >> rhs),
                    (Value::I32(lhs), Value::I32(rhs), BinOp::Add) => Value::I32(lhs + rhs),
                    (Value::I32(lhs), Value::I32(rhs), BinOp::Sub) => Value::I32(lhs - rhs),
                    (Value::I32(lhs), Value::I32(rhs), BinOp::Mul) => Value::I32(lhs * rhs),
                    (Value::I32(lhs), Value::I32(rhs), BinOp::Div) => Value::I32(lhs / rhs),
                    (Value::I32(lhs), Value::I32(rhs), BinOp::Mod) => Value::I32(lhs % rhs),
                    (Value::I32(lhs), Value::I32(rhs), BinOp::Lt) => Value::U8((lhs < rhs) as u8),
                    (Value::I32(lhs), Value::I32(rhs), BinOp::Le) => Value::U8((lhs <= rhs) as u8),
                    (Value::I32(lhs), Value::I32(rhs), BinOp::Gt) => Value::U8((lhs > rhs) as u8),
                    (Value::I32(lhs), Value::I32(rhs), BinOp::Ge) => Value::U8((lhs >= rhs) as u8),
                    (Value::I32(lhs), Value::I32(rhs), BinOp::BitAnd) => Value::I32(lhs & rhs),
                    (Value::I32(lhs), Value::I32(rhs), BinOp::BitOr) => Value::I32(lhs | rhs),
                    (Value::I32(lhs), Value::I32(rhs), BinOp::BitXor) => Value::I32(lhs ^ rhs),
                    (Value::I32(lhs), Value::I32(rhs), BinOp::Shl) => Value::I32(lhs << rhs),
                    (Value::I32(lhs), Value::I32(rhs), BinOp::Shr) => Value::I32(lhs >> rhs),
                    (Value::I64(lhs), Value::I64(rhs), BinOp::Add) => Value::I64(lhs + rhs),
                    (Value::I64(lhs), Value::I64(rhs), BinOp::Sub) => Value::I64(lhs - rhs),
                    (Value::I64(lhs), Value::I64(rhs), BinOp::Mul) => Value::I64(lhs * rhs),
                    (Value::I64(lhs), Value::I64(rhs), BinOp::Div) => Value::I64(lhs / rhs),
                    (Value::I64(lhs), Value::I64(rhs), BinOp::Mod) => Value::I64(lhs % rhs),
                    (Value::I64(lhs), Value::I64(rhs), BinOp::Lt) => Value::U8((lhs < rhs) as u8),
                    (Value::I64(lhs), Value::I64(rhs), BinOp::Le) => Value::U8((lhs <= rhs) as u8),
                    (Value::I64(lhs), Value::I64(rhs), BinOp::Gt) => Value::U8((lhs > rhs) as u8),
                    (Value::I64(lhs), Value::I64(rhs), BinOp::Ge) => Value::U8((lhs >= rhs) as u8),
                    (Value::I64(lhs), Value::I64(rhs), BinOp::BitAnd) => Value::I64(lhs & rhs),
                    (Value::I64(lhs), Value::I64(rhs), BinOp::BitOr) => Value::I64(lhs | rhs),
                    (Value::I64(lhs), Value::I64(rhs), BinOp::BitXor) => Value::I64(lhs ^ rhs),
                    (Value::I64(lhs), Value::I64(rhs), BinOp::Shl) => Value::I64(lhs << rhs),
                    (Value::I64(lhs), Value::I64(rhs), BinOp::Shr) => Value::I64(lhs >> rhs),
                    (Value::F32(lhs), Value::F32(rhs), BinOp::Add) => Value::F32(lhs + rhs),
                    (Value::F32(lhs), Value::F32(rhs), BinOp::Sub) => Value::F32(lhs - rhs),
                    (Value::F32(lhs), Value::F32(rhs), BinOp::Mul) => Value::F32(lhs * rhs),
                    (Value::F32(lhs), Value::F32(rhs), BinOp::Div) => Value::F32(lhs / rhs),
                    (Value::F32(lhs), Value::F32(rhs), BinOp::Mod) => Value::F32(lhs % rhs),
                    (Value::F32(lhs), Value::F32(rhs), BinOp::Lt) => Value::U8((lhs < rhs) as u8),
                    (Value::F32(lhs), Value::F32(rhs), BinOp::Le) => Value::U8((lhs <= rhs) as u8),
                    (Value::F32(lhs), Value::F32(rhs), BinOp::Gt) => Value::U8((lhs > rhs) as u8),
                    (Value::F32(lhs), Value::F32(rhs), BinOp::Ge) => Value::U8((lhs >= rhs) as u8),
                    (Value::F64(lhs), Value::F64(rhs), BinOp::Add) => Value::F64(lhs + rhs),
                    (Value::F64(lhs), Value::F64(rhs), BinOp::Sub) => Value::F64(lhs - rhs),
                    (Value::F64(lhs), Value::F64(rhs), BinOp::Mul) => Value::F64(lhs * rhs),
                    (Value::F64(lhs), Value::F64(rhs), BinOp::Div) => Value::F64(lhs / rhs),
                    (Value::F64(lhs), Value::F64(rhs), BinOp::Mod) => Value::F64(lhs % rhs),
                    (Value::F64(lhs), Value::F64(rhs), BinOp::Lt) => Value::U8((lhs < rhs) as u8),
                    (Value::F64(lhs), Value::F64(rhs), BinOp::Le) => Value::U8((lhs <= rhs) as u8),
                    (Value::F64(lhs), Value::F64(rhs), BinOp::Gt) => Value::U8((lhs > rhs) as u8),
                    (Value::F64(lhs), Value::F64(rhs), BinOp::Ge) => Value::U8((lhs >= rhs) as u8),
                    (Value::Ptr(loc, size), Value::U32(rhs), BinOp::Add) => Value::Ptr(loc + rhs, size),
                    (Value::Ptr(loc, size), Value::U32(rhs), BinOp::Sub) => Value::Ptr(loc - rhs, size),
                    // #endregion
                    (lhs, rhs, op) => panic!("type error: {:?}({:?}, {:?})", op, lhs, rhs)
                }
            },
            RValue::Unary(op, val) => {
                let val = self.operand(val);
                
                match (val, op) {
                    (Value::U8(v), UnOp::Not) => Value::U8(if v != 0 { 0 } else { 1 }),
                    (Value::I8(v), UnOp::Neg) => Value::I8(-v),
                    (Value::I16(v), UnOp::Neg) => Value::I16(-v),
                    (Value::I32(v), UnOp::Neg) => Value::I32(-v),
                    (Value::I64(v), UnOp::Neg) => Value::I64(-v),
                    (Value::F32(v), UnOp::Neg) => Value::F32(-v),
                    (Value::F64(v), UnOp::Neg) => Value::F64(-v),
                    _ => panic!("type error")
                }
            },
            RValue::Ref(p) => {
                let p = self.place(p);
                
                Value::Ptr(p.0 as u32, p.1.size() as u32)
            },
            RValue::Tuple(ops) => {
                let values = ops.into_iter().map(|op| self.operand(op)).collect();
                
                Value::Tuple(values)
            },
            RValue::Alloc(op, ty) => {
                let op = self.operand(op);
                let len = match op {
                    Value::U8(v) => v as usize,
                    Value::U16(v) => v as usize,
                    Value::U32(v) => v as usize,
                    Value::U64(v) => v as usize,
                    _ => panic!("type error")
                };
                
                let size = len * ty.size();
                let loc = self.memory.alloc(size);
                
                Value::Ptr(loc as u32, size as u32)
            }
        }
    }
    
    fn operand(&mut self, o: Operand) -> Value {
        match o {
            Operand::Constant(c) => self.constant(c),
            Operand::Copy(p) => {
                let (loc, ty) = self.place(p);
                
                match ty {
                    Type::Int(IntTy::I8) => Value::I8(self.memory.read_i8(loc)),
                    Type::Int(IntTy::I16) => Value::I16(self.memory.read_i16(loc)),
                    Type::Int(IntTy::I32) => Value::I32(self.memory.read_i32(loc)),
                    Type::Int(IntTy::I64) => Value::I64(self.memory.read_i64(loc)),
                    Type::UInt(UIntTy::U8) => Value::U8(self.memory.read_u8(loc)),
                    Type::UInt(UIntTy::U16) => Value::U16(self.memory.read_u16(loc)),
                    Type::UInt(UIntTy::U32) => Value::U32(self.memory.read_u32(loc)),
                    Type::UInt(UIntTy::U64) => Value::U64(self.memory.read_u64(loc)),
                    Type::Float(FloatTy::F32) => Value::F32(self.memory.read_f32(loc)),
                    Type::Float(FloatTy::F64) => Value::F64(self.memory.read_f64(loc)),
                    Type::Bool => Value::U8(self.memory.read_u8(loc)),
                    Type::Unit => Value::Unit,
                    Type::Ptr(_) => Value::Ptr(self.memory.read_u32(loc), self.memory.read_u32(loc + 4)),
                    Type::Tuple(..) => unimplemented!()
                }
            },
            Operand::Move(p) => {
                let (loc, ty) = self.place(p);
                
                let r = match &ty {
                    Type::Int(IntTy::I8) => Value::I8(self.memory.read_i8(loc)),
                    Type::Int(IntTy::I16) => Value::I16(self.memory.read_i16(loc)),
                    Type::Int(IntTy::I32) => Value::I32(self.memory.read_i32(loc)),
                    Type::Int(IntTy::I64) => Value::I64(self.memory.read_i64(loc)),
                    Type::UInt(UIntTy::U8) => Value::U8(self.memory.read_u8(loc)),
                    Type::UInt(UIntTy::U16) => Value::U16(self.memory.read_u16(loc)),
                    Type::UInt(UIntTy::U32) => Value::U32(self.memory.read_u32(loc)),
                    Type::UInt(UIntTy::U64) => Value::U64(self.memory.read_u64(loc)),
                    Type::Float(FloatTy::F32) => Value::F32(self.memory.read_f32(loc)),
                    Type::Float(FloatTy::F64) => Value::F64(self.memory.read_f64(loc)),
                    Type::Bool => Value::U8(self.memory.read_u8(loc)),
                    Type::Unit => Value::Unit,
                    Type::Ptr(_) => Value::Ptr(self.memory.read_u32(loc), self.memory.read_u32(loc + 4)),
                    Type::Tuple(..) => unimplemented!()
                };
                
                self.memory.clear(loc, ty.size());
                
                r
            }
        }
    }
    
    fn constant(&mut self, c: Constant) -> Value {
        match c {
            Constant::Int(v, IntTy::I8) => Value::I8(v as i8),
            Constant::Int(v, IntTy::I16) => Value::I16(v as i16),
            Constant::Int(v, IntTy::I32) => Value::I32(v as i32),
            Constant::Int(v, IntTy::I64) => Value::I64(v as i64),
            Constant::UInt(v, UIntTy::U8) => Value::U8(v as u8),
            Constant::UInt(v, UIntTy::U16) => Value::U16(v as u16),
            Constant::UInt(v, UIntTy::U32) => Value::U32(v as u32),
            Constant::UInt(v, UIntTy::U64) => Value::U64(v as u64),
            Constant::Float(v, FloatTy::F32) => Value::F32(v as f32),
            Constant::Float(v, FloatTy::F64) => Value::F64(v as f64),
            Constant::Bool(b) => Value::U8(b as u8),
            Constant::Item(id) => {
                for (i, (name, _)) in self.fns.iter().enumerate() {
                    if name == &id.text { return Value::U32(i as u32); }
                }
                
                panic!("unknown symbol")
            },
            Constant::Tuple(cs) => {
                let values = cs.into_iter().map(|c| self.constant(c)).collect();
                
                Value::Tuple(values)
            },
            _ => unimplemented!()
        }
    }
    
    fn drop(&mut self, id: LocalId) {
        let loc = self.frame().locals[&id];
        let size = self.frame().sizes[&id].size();
        
        self.memory.free(loc, size);
    }
    
    fn drop_frame(&mut self, id: LocalId, frame: &StackFrame) {
        let loc = frame.locals[&id];
        let size = frame.sizes[&id].size();
        
        self.memory.free(loc, size);
    }
    
    fn block(&self, f: &Function) -> BasicBlock {
        if let Some(b) = f.blocks.iter().find(|b| b.id == self.frame().block) {
            b.clone()
        } else {
            panic!("undefined block {}", self.frame().block);
        }
    }
    
    fn frame(&self) -> &StackFrame {
        self.frames.last().unwrap()
    }
    
    fn frame_mut(&mut self) -> &mut StackFrame {
        self.frames.last_mut().unwrap()
    }
}