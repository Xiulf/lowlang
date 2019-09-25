use crate::*;
use parser::parse::{Parse, ParseStream};
use parser::error::Result;
use parser::literal::*;
use parser::token;

token![ident "i8" I8];
token![ident "i16" I16];
token![ident "i32" I32];
token![ident "i64" I64];
token![ident "u8" U8];
token![ident "u16" U16];
token![ident "u32" U32];
token![ident "u64" U64];
token![ident "f32" F32];
token![ident "f64" F64];
token![ident "bool" Bool];

token![ident "copy" Copy];
token![ident "move" Move];
token![ident "const" Const];
token![ident "box" TBox];
token![ident "let" Let];
token![ident "fn" Fn];
token![ident "true" True];
token![ident "false" False];

token![ident "StorageLive" StorageLive];
token![ident "StorageDead" StorageDead];
token![ident "goto" Goto];
token![ident "resume" Resume];
token![ident "abort" Abort];
token![ident "return" Return];
token![ident "unreachable" Unreachable];
token![ident "call" Call];
token![ident "assert" Assert];
token![ident "unwind" Unwind];

token![ident "Add" Add];
token![ident "Sub" Sub];
token![ident "Mul" Mul];
token![ident "Div" Div];
token![ident "Mod" Mod];
token![ident "Lt" Lt];
token![ident "Le" Le];
token![ident "Gt" Gt];
token![ident "Ge" Ge];
token![ident "Eq" Eq];
token![ident "Ne" Ne];
token![ident "BitAnd" BitAnd];
token![ident "BitOr" BitOr];
token![ident "BitXor" BitXor];
token![ident "Shl" Shl];
token![ident "Shr" Shr];
token![ident "Not" Not];
token![ident "Neg" Neg];

token![punct ">" Proj/1];
token![punct "," Comma/1];
token![punct ":" Colon/1];
token![punct ";" Semi/1];
token![punct "%" Pct/1];
token![punct "$" Pound/1];
token![punct "!" Bang/1];
token![punct "*" Deref/1];
token![punct "&" Ref/1];
token![punct "=" Equals/1];
token![punct "(" LParen/1];
token![punct ")" RParen/1];
token![punct "{" LBrace/1];
token![punct "}" RBrace/1];
token![punct "[" LBracket/1];
token![punct "]" RBracket/1];
token![punct "->" Arrow/2];

impl Parse for Program {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut fns = Vec::new();
        
        while !input.is_empty() {
            match input.parse() {
                Ok(f) => fns.push(f),
                Err(e) => {
                    input.reporter.add(e);
                    
                    while !input.is_empty() && !input.peek::<Fn>() {
                        input.bump();
                    }
                }
            }
        }
        
        Ok(Program {
            fns,
        })
    }
}

impl Parse for Function {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Fn>()?;
        
        let name = input.parse()?;
        
        input.parse::<LParen>()?;
        
        let mut params = Vec::new();
        
        while !input.is_empty() && !input.peek::<RParen>() {
            let name = input.parse()?;
            
            input.parse::<Colon>()?;
            
            let ty = input.parse()?;
            
            params.push((name, ty));
            
            if !input.peek::<RParen>() {
                input.parse::<Comma>()?;
            }
        }
        
        input.parse::<RParen>()?;
        input.parse::<Arrow>()?;
        
        let ret = input.parse()?;
        
        input.parse::<LBrace>()?;
        
        let mut bindings = Vec::new();
        let mut blocks = Vec::new();
        
        while !input.is_empty() && input.peek::<Let>() {
            input.parse::<Let>()?;
            
            let name = input.parse()?;
            
            input.parse::<Colon>()?;
            
            let ty = input.parse()?;
            
            input.parse::<Semi>()?;
            bindings.push((name, ty));
        }
        
        while !input.is_empty() && input.peek::<Pct>() {
            blocks.push(input.parse()?);
        }
        
        input.parse::<RBrace>()?;
        
        Ok(Function {
            name,
            params,
            ret,
            bindings,
            blocks,
        })
    }
}

impl Parse for BlockId {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Pct>()?;
        
        let lit = input.parse::<IntLiteral>()?;
        
        Ok(BlockId(lit.int as usize))
    }
}

impl Parse for LocalId {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Pound>()?;
        
        let lit = input.parse::<IntLiteral>()?;
        
        Ok(LocalId(lit.int as usize))
    }
}

impl Parse for BasicBlock {
    fn parse(input: ParseStream) -> Result<Self> {
        let id = input.parse()?;
        
        input.parse::<Colon>()?;
        input.parse::<LBrace>()?;
        
        let mut statements = Vec::new();
        
        while let Ok(_) = input.fork().parse::<Statement>() {
            statements.push(input.parse()?);
        }
        
        let terminator = input.parse()?;
        
        input.parse::<RBrace>()?;
        
        Ok(BasicBlock {
            id,
            statements,
            terminator,
        })
    }
}

impl Parse for Statement {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek::<StorageLive>() {
            input.parse::<StorageLive>()?;
            input.parse::<LParen>()?;
            
            let v = input.parse()?;
            
            input.parse::<RParen>()?;
            input.parse::<Semi>()?;
            
            Ok(Statement::StorageLive(v))
        } else if input.peek::<StorageDead>() {
            input.parse::<StorageDead>()?;
            input.parse::<LParen>()?;
            
            let v = input.parse()?;
            
            input.parse::<RParen>()?;
            input.parse::<Semi>()?;
            
            Ok(Statement::StorageDead(v))
        } else {
            let l = input.parse()?;
            
            input.parse::<Equals>()?;
            
            let r = input.parse()?;
            
            input.parse::<Semi>()?;
            
            Ok(Statement::Assign(l, r))
        }
    }
}

impl Parse for Terminator {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek::<Goto>() {
            input.parse::<Goto>()?;
            input.parse::<LParen>()?;
            
            let to = input.parse()?;
            
            input.parse::<RParen>()?;
            
            Ok(Terminator::Goto(to))
        } else if input.peek::<Resume>() {
            input.parse::<Resume>()?;
            
            Ok(Terminator::Resume)
        } else if input.peek::<Abort>() {
            input.parse::<Abort>()?;
            
            Ok(Terminator::Abort)
        } else if input.peek::<Return>() {
            input.parse::<Return>()?;
            
            Ok(Terminator::Return)
        } else if input.peek::<Unreachable>() {
            input.parse::<Unreachable>()?;
            
            Ok(Terminator::Unreachable)
        } else if input.peek::<Call>() {
            input.parse::<Call>()?;
            input.parse::<LParen>()?;
            
            let goto = if let Ok(p) = input.parse::<Place>() {
                input.parse::<Equals>()?;
                
                Some(p)
            } else {
                None
            };
            
            let op = input.parse()?;
            
            input.parse::<LParen>()?;
            
            let mut args = Vec::new();
            
            while !input.is_empty() && !input.peek::<RParen>() {
                args.push(input.parse()?);
                
                if !input.peek::<RParen>() {
                    input.parse::<Comma>()?;
                }
            }
            
            input.parse::<RParen>()?;
            
            let (goto, fail) = if let Some(goto) = goto {
                input.parse::<Comma>()?;
                input.parse::<Goto>()?;
                
                let fail = if let Ok(_) = input.parse::<Comma>() {
                    input.parse::<Unwind>()?;
                    
                    Some(input.parse()?)
                } else {
                    None
                };
                
                (Some((goto, input.parse()?)), fail)
            } else if let Ok(_) = input.parse::<Comma>() {
                input.parse::<Unwind>()?;
                
                (None, Some(input.parse()?))
            } else {
                (None, None)
            };
            
            input.parse::<RParen>()?;
            
            Ok(Terminator::Call(op, args, goto, fail))
        } else if input.peek::<Assert>() {
            input.parse::<Assert>()?;
            input.parse::<LParen>()?;
            
            let expected = input.parse::<Option<Bang>>()?.is_some();
            let op = input.parse()?;
            
            input.parse::<Comma>()?;
            input.parse::<Goto>()?;
            
            let success = input.parse()?;
            let fail = if let Ok(_) = input.parse::<Comma>() {
                input.parse::<Unwind>()?;
                
                Some(input.parse()?)
            } else {
                None
            };
            
            input.parse::<RParen>()?;
            
            Ok(Terminator::Assert(op, expected, success, fail))
        } else {
            input.error("invalid terminator")
        }
    }
}

impl Parse for Place {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut projection = Vec::new();
        
        while !input.is_empty() && input.peek::<Deref>() {
            input.parse::<Deref>()?;
            projection.push(PlaceElem::Deref);
        }
        
        let base = input.parse()?;
        
        while !input.is_empty() && input.peek::<Proj>() {
            input.parse::<Proj>()?;
            
            let lit = input.parse::<IntLiteral>()?;
            
            projection.push(PlaceElem::Field(lit.int as usize));
        }
        
        Ok(Place {
            base,
            projection,
        })
    }
}

impl Parse for PlaceBase {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek::<Pound>() {
            Ok(PlaceBase::Local(input.parse()?))
        } else {
            input.error("invalid place")
        }
    }
}

impl Parse for Operand {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek::<Copy>() {
            input.parse::<Copy>()?;
            
            Ok(Operand::Copy(input.parse()?))
        } else if input.peek::<Move>() {
            input.parse::<Move>()?;
            
            Ok(Operand::Move(input.parse()?))
        } else if input.peek::<Const>() {
            Ok(Operand::Constant(input.parse()?))
        } else {
            input.error("invalid operand")
        }
    }
}

impl Parse for RValue {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek::<Ref>() {
            input.parse::<Ref>()?;
            
            Ok(RValue::Ref(input.parse()?))
        } else if let Ok(op) = input.parse::<BinOp>() {
            input.parse::<LParen>()?;
            
            let lhs = input.parse()?;
            
            input.parse::<Comma>()?;
            
            let rhs = input.parse()?;
            
            input.parse::<RParen>()?;
            
            Ok(RValue::Binary(op, lhs, rhs))
        } else if let Ok(op) = input.parse::<UnOp>() {
            input.parse::<LParen>()?;
            
            let lhs = input.parse()?;
            
            input.parse::<RParen>()?;
            
            Ok(RValue::Unary(op, lhs))
        } else if input.peek::<LParen>() {
            input.parse::<LParen>()?;
            
            let mut ops = Vec::new();
            
            while !input.is_empty() && !input.peek::<RParen>() {
                ops.push(input.parse()?);
                
                if !input.peek::<RParen>() {
                    input.parse::<Comma>()?;
                }
            }
            
            input.parse::<RParen>()?;
            
            if ops.len() == 1 {
                input.error("tuple expects 0 or 2 or more elements")
            } else {
                Ok(RValue::Tuple(ops))
            }
        } else if input.peek::<TBox>() {
            input.parse::<TBox>()?;
            
            Ok(RValue::Box(input.parse()?))
        } else {
            Ok(RValue::Use(input.parse()?))
        }
    }
}

impl Parse for BinOp {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(_) = input.parse::<Add>() { Ok(BinOp::Add) }
        else if let Ok(_) = input.parse::<Sub>() { Ok(BinOp::Sub) }
        else if let Ok(_) = input.parse::<Mul>() { Ok(BinOp::Mul) }
        else if let Ok(_) = input.parse::<Div>() { Ok(BinOp::Div) }
        else if let Ok(_) = input.parse::<Mod>() { Ok(BinOp::Mod) }
        else if let Ok(_) = input.parse::<Lt>() { Ok(BinOp::Lt) }
        else if let Ok(_) = input.parse::<Le>() { Ok(BinOp::Le) }
        else if let Ok(_) = input.parse::<Gt>() { Ok(BinOp::Gt) }
        else if let Ok(_) = input.parse::<Ge>() { Ok(BinOp::Ge) }
        else if let Ok(_) = input.parse::<Eq>() { Ok(BinOp::Eq) }
        else if let Ok(_) = input.parse::<Ne>() { Ok(BinOp::Ne) }
        else if let Ok(_) = input.parse::<BitAnd>() { Ok(BinOp::BitAnd) }
        else if let Ok(_) = input.parse::<BitOr>() { Ok(BinOp::BitOr) }
        else if let Ok(_) = input.parse::<BitXor>() { Ok(BinOp::BitXor) }
        else if let Ok(_) = input.parse::<Shl>() { Ok(BinOp::Shl) }
        else if let Ok(_) = input.parse::<Shr>() { Ok(BinOp::Shr) }
        else { input.error("invalid binary operation") }
    }
}

impl Parse for UnOp {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(_) = input.parse::<Not>() { Ok(UnOp::Not) }
        else if let Ok(_) = input.parse::<Neg>() { Ok(UnOp::Neg) }
        else { input.error("invalid unary operation") }
    }
}

impl Parse for Constant {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Const>()?;
        
        if input.peek::<IntLiteral>() {
            let lit = input.parse::<IntLiteral>()?;
            
            if input.peek::<I8>() || input.peek::<I16>() ||
                input.peek::<I32>() || input.peek::<I64>()
            {
                Ok(Constant::Int(lit.int as i64, input.parse()?))
            } else {
                Ok(Constant::UInt(lit.int, input.parse()?))
            }
        } else if input.peek::<FloatLiteral>() {
            let lit = input.parse::<FloatLiteral>()?;
            
            Ok(Constant::Float(lit.float, input.parse()?))
        } else if input.peek::<True>() {
            input.parse::<True>()?;
            
            Ok(Constant::Bool(true))
        } else if input.peek::<False>() {
            input.parse::<False>()?;
            
            Ok(Constant::Bool(false))
        } else if input.peek::<Ident>() {
            Ok(Constant::Item(input.parse()?))
        } else if input.peek::<LParen>() {
            input.parse::<LParen>()?;
            
            let mut consts = Vec::new();
            
            while !input.is_empty() && !input.peek::<RParen>() {
                consts.push(input.parse()?);
                
                if !input.peek::<RParen>() {
                    input.parse::<Comma>()?;
                }
            }
            
            input.parse::<RParen>()?;
            
            if consts.len() == 1 {
                Ok(consts.into_iter().next().unwrap())
            } else {
                Ok(Constant::Tuple(consts))
            }
        } else {
            input.error("expected a constant")
        }
    }
}

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek::<LParen>() {
            input.parse::<LParen>()?;
            
            let mut tys = Vec::new();
            
            while !input.is_empty() && !input.peek::<RParen>() {
                tys.push(input.parse()?);
                
                if !input.peek::<RParen>() {
                    input.parse::<Comma>()?;
                }
            }
            
            input.parse::<RParen>()?;
            
            if tys.is_empty() {
                Ok(Type::Unit)
            } else if tys.len() == 1 {
                Ok(tys.into_iter().next().unwrap())
            } else {
                Ok(Type::Tuple(tys))
            }
        } else if input.peek::<Bool>() {
            input.parse::<Bool>()?;
            
            Ok(Type::Bool)
        } else if input.peek::<I8>() || input.peek::<I16>() ||
            input.peek::<I32>() || input.peek::<I64>()
        {
            Ok(Type::Int(input.parse()?))
        } else if input.peek::<I8>() || input.peek::<I16>() ||
            input.peek::<I32>() || input.peek::<I64>()
        {
            Ok(Type::UInt(input.parse()?))
        } else if input.peek::<F32>() || input.peek::<F64>() {
            Ok(Type::Float(input.parse()?))
        } else {
            input.error("expected a type")
        }
    }
}

impl Parse for IntTy {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek::<I8>() {
            input.parse::<I8>()?;
            
            Ok(IntTy::I8)
        } else if input.peek::<I16>() {
            input.parse::<I16>()?;
            
            Ok(IntTy::I16)
        } else if input.peek::<I32>() {
            input.parse::<I32>()?;
            
            Ok(IntTy::I32)
        } else if input.peek::<I64>() {
            input.parse::<I64>()?;
            
            Ok(IntTy::I64)
        } else {
            input.error("expected 'i8', 'i16', 'i32' or 'i64'")
        }
    }
}

impl Parse for UIntTy {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek::<U8>() {
            input.parse::<U8>()?;
            
            Ok(UIntTy::U8)
        } else if input.peek::<U16>() {
            input.parse::<U16>()?;
            
            Ok(UIntTy::U16)
        } else if input.peek::<U32>() {
            input.parse::<U32>()?;
            
            Ok(UIntTy::U32)
        } else if input.peek::<U64>() {
            input.parse::<U64>()?;
            
            Ok(UIntTy::U64)
        } else {
            input.error("expected 'u8', 'u16', 'u32' or 'u64'")
        }
    }
}

impl Parse for FloatTy {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek::<F32>() {
            input.parse::<F32>()?;
            
            Ok(FloatTy::F32)
        } else if input.peek::<F64>() {
            input.parse::<F64>()?;
            
            Ok(FloatTy::F64)
        } else {
            input.error("expected 'f32' or 'f64'")
        }
    }
}