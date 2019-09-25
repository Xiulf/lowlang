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

token![ident "const" Const];
token![ident "let" Let];
token![ident "fn" Fn];
token![ident "true" True];
token![ident "false" False];

token![punct "." Dot/1];
token![punct "," Comma/1];
token![punct ":" Colon/1];
token![punct ";" Semi/1];
token![punct "%" Pct/1];
token![punct "$" Pound/1];
token![punct "!" Not/1];
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
            fns.push(input.parse()?);
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
        
        while let Ok(statement) = input.parse() {
            statements.push(statement);
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
        
    }
}

impl Parse for Terminator {
    fn parse(input: ParseStream) -> Result<Self> {
        
    }
}

impl Parse for Place {
    fn parse(input: ParseStream) -> Result<Self> {
        
    }
}

impl Parse for PlaceBase {
    fn parse(input: ParseStream) -> Result<Self> {
        
    }
}

impl Parse for Operand {
    fn parse(input: ParseStream) -> Result<Self> {
        
    }
}

impl Parse for RValue {
    fn parse(input: ParseStream) -> Result<Self> {
        
    }
}

impl Parse for BinOp {
    fn parse(input: ParseStream) -> Result<Self> {
        
    }
}

impl Parse for UnOp {
    fn parse(input: ParseStream) -> Result<Self> {
        
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
            Ok(IntTy::I8)
        } else if input.peek::<I16>() {
            Ok(IntTy::I16)
        } else if input.peek::<I32>() {
            Ok(IntTy::I32)
        } else if input.peek::<I64>() {
            Ok(IntTy::I64)
        } else {
            input.error("expected 'i8', 'i16', 'i32' or 'i64'")
        }
    }
}

impl Parse for UIntTy {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek::<U8>() {
            Ok(UIntTy::U8)
        } else if input.peek::<U16>() {
            Ok(UIntTy::U16)
        } else if input.peek::<U32>() {
            Ok(UIntTy::U32)
        } else if input.peek::<U64>() {
            Ok(UIntTy::U64)
        } else {
            input.error("expected 'u8', 'u16', 'u32' or 'u64'")
        }
    }
}

impl Parse for FloatTy {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek::<F32>() {
            Ok(FloatTy::F32)
        } else if input.peek::<F64>() {
            Ok(FloatTy::F64)
        } else {
            input.error("expected 'f32' or 'f64'")
        }
    }
}