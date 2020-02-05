use crate::*;
use parser::parse::{Parse, ParseStream};
use parser::ident::Ident;
use parser::literal::StringLiteral;
use parser::literal::IntLiteral;
use parser::error::Result;
use std::collections::BTreeMap;

parser::token![ident "package" TPackage];
parser::token![ident "extern" TExtern];
parser::token![ident "global" TGlobal];
parser::token![ident "fn" TFn];
parser::token![ident "export" TExport];
parser::token![ident "return" TReturn];
parser::token![ident "jump" TJump];
parser::token![ident "call" TCall];
parser::token![ident "switch" TSwitch];
parser::token![ident "otherwise" TOtherwise];
parser::token![ident "cast" TCast];
parser::token![ident "unit" TUnit];
parser::token![ident "add" TAdd];
parser::token![ident "sub" TSub];
parser::token![ident "mul" TMul];
parser::token![ident "div" TDiv];
parser::token![ident "rem" TRem];
parser::token![ident "eq" TEq];
parser::token![ident "ne" TNe];
parser::token![ident "lt" TLt];
parser::token![ident "le" TLe];
parser::token![ident "gt" TGt];
parser::token![ident "ge" TGe];
parser::token![ident "band" TBand];
parser::token![ident "bor" TBor];
parser::token![ident "bxor" TBxor];
parser::token![ident "shl" TShl];
parser::token![ident "shr" TShr];
parser::token![ident "neg" TNeg];
parser::token![ident "not" TNot];
parser::token![ident "sizeof" TSizeof];
parser::token![ident "alignof" TAlignof];
parser::token![ident "bool" TBool];
parser::token![ident "char" TChar];
parser::token![ident "str" TStr];
parser::token![ident "ratio" TRatio];
parser::token![ident "i8" TI8];
parser::token![ident "i16" TI16];
parser::token![ident "i32" TI32];
parser::token![ident "i64" TI64];
parser::token![ident "i128" TI128];
parser::token![ident "isize" TIsize];
parser::token![ident "u8" TU8];
parser::token![ident "u16" TU16];
parser::token![ident "u32" TU32];
parser::token![ident "u64" TU64];
parser::token![ident "u128" TU128];
parser::token![ident "usize" TUsize];
parser::token![ident "f32" TF32];
parser::token![ident "f64" TF64];
parser::token![ident "fsize" TFsize];

parser::token![punct "(" TLParen/1];
parser::token![punct ")" TRParen/1];
parser::token![punct "{" TLBrace/1];
parser::token![punct "}" TRBrace/1];
parser::token![punct "[" TLBracket/1];
parser::token![punct "]" TRBracket/1];
parser::token![punct "." TDot/1];
parser::token![punct "," TComma/1];
parser::token![punct ":" TColon/1];
parser::token![punct ";" TSemi/1];
parser::token![punct "=" TEquals/1];
parser::token![punct "#" THash/1];
parser::token![punct "!" TBang/1];
parser::token![punct "|" TBar/1];
parser::token![punct "/" TSlash/1];
parser::token![punct "<" TLeft/1];
parser::token![punct ">" TRight/1];
parser::token![punct "%" TPct/1];
parser::token![punct "&" TAnd/1];
parser::token![punct "*" TStar/1];
parser::token![punct "->" TArrow/2];
parser::token![punct ".." TDots/2];

impl Parse for Package {
    fn parse(input: ParseStream) -> Result<Package> {
        input.parse::<TPackage>()?;

        let name = input.parse::<Ident>()?.name;
        let mut externs = BTreeMap::new();
        let mut globals = BTreeMap::new();
        let mut bodies = BTreeMap::new();
        
        while !input.is_empty() {
            let id = input.parse()?;

            input.parse::<TColon>()?;

            if input.peek::<TExtern>() {
                match input.parse() {
                    Ok(v) => { externs.insert(id, v); },
                    Err(e) => {
                        input.reporter.add(e);
                                                
                        while !input.is_empty() && !input.peek::<THash>() {
                            input.bump();
                        }
                    },
                }
            } else if input.peek::<TGlobal>() || (input.peek::<TExport>() && input.peek2::<TGlobal>()) {
                match input.parse() {
                    Ok(v) => { globals.insert(id, v); },
                    Err(e) => {
                        input.reporter.add(e);
                                                
                        while !input.is_empty() && !input.peek::<THash>() {
                            input.bump();
                        }
                    },
                }
            } else {
                match input.parse() {
                    Ok(v) => { bodies.insert(id, v); },
                    Err(e) => {
                        input.reporter.add(e);
                        
                        while !input.is_empty() && !input.peek::<THash>() {
                            input.bump();
                        }
                    },
                }
            }
        }

        Ok(Package {
            name: name,
            externs,
            globals,
            bodies,
        })
    }
}

impl Parse for Signature {
    fn parse(input: ParseStream) -> Result<Signature> {
        input.parse::<TFn>()?;

        let conv = input.parse()?;
        let mut params = Vec::new();

        input.parse::<TLParen>()?;

        while !input.is_empty() && !input.peek::<TRParen>() {
            params.push(input.parse()?);

            if !input.peek::<TRParen>() {
                input.parse::<TComma>()?;
            }
        }

        input.parse::<TRParen>()?;
        input.parse::<TArrow>()?;

        let mut rets = Vec::new();

        input.parse::<TLParen>()?;

        while !input.is_empty() && !input.peek::<TRParen>() {
            rets.push(input.parse()?);

            if !input.peek::<TRParen>() {
                input.parse::<TComma>()?;
            }
        }

        input.parse::<TRParen>()?;

        Ok(Signature(conv, params, rets))
    }
}

impl Parse for Extern {
    fn parse(input: ParseStream) -> Result<Extern> {
        input.parse::<TExtern>()?;

        let name = input.parse::<Ident>()?.name;

        input.parse::<TColon>()?;

        let ext = if input.peek::<TFn>() {
            Extern::Proc(name, input.parse()?)
        } else {
            Extern::Global(name, input.parse()?)
        };

        input.parse::<TSemi>()?;

        Ok(ext)
    }
}

impl Parse for Global {
    fn parse(input: ParseStream) -> Result<Global> {
        let export = input.parse::<TExport>().is_ok();
        
        input.parse::<TGlobal>()?;

        let name = input.parse::<Ident>()?.name;

        input.parse::<TColon>()?;

        let ty = input.parse()?;

        input.parse::<TSemi>()?;

        Ok(Global {
            export,
            name,
            ty,
            init: None,
        })
    }
}

impl Parse for Body {
    fn parse(input: ParseStream) -> Result<Body> {
        let export = input.parse::<TExport>().is_ok();
        
        input.parse::<TFn>()?;

        let name = input.parse::<Ident>()?.name;
        let conv = input.parse()?;
        let mut locals = BTreeMap::new();

        input.parse::<TLParen>()?;

        while !input.is_empty() && !input.peek::<TRParen>() {
            let id = input.parse()?;

            input.parse::<TColon>()?;

            let ty = input.parse()?;

            locals.insert(id, Local {
                id,
                kind: LocalKind::Arg,
                ty,
            });
            
            if !input.peek::<TRParen>() {
                input.parse::<TComma>()?;
            }
        }

        input.parse::<TRParen>()?;
        input.parse::<TArrow>()?;
        input.parse::<TLParen>()?;

        while !input.is_empty() && !input.peek::<TRParen>() {
            let id = input.parse()?;

            input.parse::<TColon>()?;

            let ty = input.parse()?;

            locals.insert(id, Local {
                id,
                kind: LocalKind::Ret,
                ty,
            });
            
            if !input.peek::<TRParen>() {
                input.parse::<TComma>()?;
            }
        }

        input.parse::<TRParen>()?;
        input.parse::<TLBrace>()?;

        let mut blocks = BTreeMap::new();

        while !input.is_empty() && input.peek::<LocalId>() {
            let id = input.parse()?;

            input.parse::<TColon>()?;

            let ty = input.parse()?;

            locals.insert(id, Local {
                id,
                kind: LocalKind::Var,
                ty,
            });
        }

        while !input.is_empty() && !input.peek::<TRBrace>() {
            let id = input.parse()?;
            let block = Block::parse(input, id)?;

            blocks.insert(id, block);
        }

        input.parse::<TRBrace>()?;

        Ok(Body {
            export,
            name,
            conv,
            locals,
            blocks,
        })
    }
}

impl Block {
    fn parse(input: ParseStream, id: BlockId) -> Result<Block> {
        let mut stmts = Vec::new();
        let mut term = Terminator::Unset;

        input.parse::<TLBrace>()?;

        while !input.is_empty() && !input.peek::<TRBrace>() {
            if input.peek::<TJump>() ||
                input.peek::<TReturn>() ||
                input.peek::<TCall>() ||
                input.peek::<TSwitch>()
            {
                term = input.parse()?;

                break;
            } else {
                stmts.push(input.parse()?);
            }
        }

        input.parse::<TRBrace>()?;

        Ok(Block {
            id,
            stmts,
            term,
        })
    }
}

impl Parse for Stmt {
    fn parse(input: ParseStream) -> Result<Stmt> {
        let place = input.parse()?;

        input.parse::<TEquals>()?;

        let value = input.parse()?;

        Ok(Stmt::Assign(place, value))
    }
}

impl Parse for Terminator {
    fn parse(input: ParseStream) -> Result<Terminator> {
        if let Ok(_) = input.parse::<TReturn>() {
            Ok(Terminator::Return)
        } else if let Ok(_) = input.parse::<TJump>() {
            Ok(Terminator::Jump(input.parse()?))
        } else if let Ok(_) = input.parse::<TCall>() {
            let mut places = Vec::new();

            if { let fork = input.fork(); fork.parse::<Place>().is_ok() && !fork.peek::<TLParen>() } {
                places.push(input.parse()?);
            }

            while !input.is_empty() && input.peek::<TComma>() {
                input.parse::<TComma>()?;
                places.push(input.parse()?);
            }

            if !places.is_empty() {
                input.parse::<TEquals>()?;
            }

            let proc = input.parse()?;
            let mut args = Vec::new();

            input.parse::<TLParen>()?;

            while !input.is_empty() && !input.peek::<TRParen>() {
                args.push(input.parse()?);

                if !input.peek::<TRParen>() {
                    input.parse::<TComma>()?;
                }
            }

            input.parse::<TRParen>()?;
            input.parse::<TComma>()?;

            let target = input.parse()?;

            Ok(Terminator::Call(places, proc, args, target))
        } else {
            input.parse::<TSwitch>()?;

            let pred = input.parse()?;
            let mut values = Vec::new();
            let mut targets = Vec::new();

            input.parse::<TLBracket>()?;

            while !input.is_empty() && !input.peek::<TOtherwise>() {
                values.push(input.parse::<IntLiteral>()?.int as u128);
                input.parse::<TColon>()?;
                targets.push(input.parse()?);
                input.parse::<TComma>()?;
            }

            input.parse::<TOtherwise>()?;
            targets.push(input.parse()?);
            input.parse::<TRBracket>()?;

            Ok(Terminator::Switch(pred, values, targets))
        }
    }
}

impl Parse for Place {
    fn parse(input: ParseStream) -> Result<Place> {
        let mut elems = Vec::new();

        while !input.is_empty() && input.peek::<TStar>() {
            input.parse::<TStar>()?;
            elems.push(PlaceElem::Deref);
        }

        let mut place = if let Ok(_) = input.parse::<TLParen>() {
            let place = input.parse()?;

            input.parse::<TRParen>()?;
            place
        } else {
            Place {
                base: input.parse()?,
                elems: Vec::new(),
            }
        };

        while !input.is_empty() {
            if input.peek::<TDot>() && !input.peek::<TDots>() {
                input.parse::<TDot>()?;

                let n = input.parse::<IntLiteral>()?.int as usize;

                place.elems.push(PlaceElem::Field(n));
            } else {
                let fork = input.fork();

                if let Ok(_) = fork.parse::<TLBracket>() {
                    if fork.parse::<Place>().is_ok() || fork.parse::<IntLiteral>().is_ok() {
                        if !fork.peek::<TDots>() && !fork.peek::<TColon>() {
                            input.parse::<TLBracket>()?;

                            if let Ok(lit) = input.parse::<IntLiteral>() {
                                place.elems.push(PlaceElem::ConstIndex(lit.int as usize));
                            } else {
                                place.elems.push(PlaceElem::Index(input.parse()?));
                            }

                            input.parse::<TRBracket>()?;

                            continue;
                        }
                    }
                }

                break;
            }
        }

        place.elems.append(&mut elems);

        Ok(place)
    }
}

impl Parse for PlaceBase {
    fn parse(input: ParseStream) -> Result<PlaceBase> {
        if input.peek::<THash>() && input.peek2::<TBang>() {
            let id = input.parse::<IntLiteral>()
                .map(|l| ItemId(l.int as usize))?;

            Ok(PlaceBase::Global(id))
        } else {
            Ok(PlaceBase::Local(input.parse()?))
        }
    }
}

impl Parse for Operand {
    fn parse(input: ParseStream) -> Result<Operand> {
        if let Ok(c) = input.parse() {
            Ok(Operand::Constant(c))
        } else {
            Ok(Operand::Place(input.parse()?))
        }
    }
}

impl Parse for Const {
    fn parse(input: ParseStream) -> Result<Const> {
        if let Ok(_) = input.parse::<TUnit>() {
            Ok(Const::Unit)
        } else if let Ok(lit) = input.parse::<IntLiteral>() {
            Ok(Const::Scalar(lit.int, input.parse()?))
        } else if let Ok(lit) = input.parse::<StringLiteral>() {
            Ok(Const::Bytes(lit.text.into_bytes().into_boxed_slice()))
        } else {
            Ok(Const::FuncAddr(input.parse()?))
        }
    }
}

impl Parse for Value {
    fn parse(input: ParseStream) -> Result<Value> {
        if let Ok(_) = input.parse::<TAnd>() {
            Ok(Value::Ref(input.parse()?))
        } else if let Ok(_) = input.parse::<TAdd>() {
            Ok(Value::BinOp(BinOp::Add, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TSub>() {
            Ok(Value::BinOp(BinOp::Sub, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TMul>() {
            Ok(Value::BinOp(BinOp::Mul, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TDiv>() {
            Ok(Value::BinOp(BinOp::Div, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TRem>() {
            Ok(Value::BinOp(BinOp::Rem, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TEq>() {
            Ok(Value::BinOp(BinOp::Eq, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TNe>() {
            Ok(Value::BinOp(BinOp::Ne, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TLt>() {
            Ok(Value::BinOp(BinOp::Lt, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TLe>() {
            Ok(Value::BinOp(BinOp::Le, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TGt>() {
            Ok(Value::BinOp(BinOp::Gt, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TGe>() {
            Ok(Value::BinOp(BinOp::Ge, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TBand>() {
            Ok(Value::BinOp(BinOp::BitAnd, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TBor>() {
            Ok(Value::BinOp(BinOp::BitOr, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TBxor>() {
            Ok(Value::BinOp(BinOp::BitXOr, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TShl>() {
            Ok(Value::BinOp(BinOp::Shl, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TShr>() {
            Ok(Value::BinOp(BinOp::Shr, input.parse()?, input.parse()?))
        } else if let Ok(_) = input.parse::<TNeg>() {
            Ok(Value::UnOp(UnOp::Neg, input.parse()?))
        } else if let Ok(_) = input.parse::<TNot>() {
            Ok(Value::UnOp(UnOp::Not, input.parse()?))
        } else if let Ok(_) = input.parse::<TSizeof>() {
            Ok(Value::NullOp(NullOp::SizeOf, input.parse()?))
        } else if let Ok(_) = input.parse::<TAlignof>() {
            Ok(Value::NullOp(NullOp::AlignOf, input.parse()?))
        } else if let Ok(_) = input.parse::<TUnit>() {
            Ok(Value::Use(Operand::Constant(Const::Unit)))
        } else if let Ok(_) = input.parse::<TCast>() {
            let ty = input.parse()?;

            input.parse::<TComma>()?;

            let op = input.parse()?;

            Ok(Value::Cast(ty, op))
        } else if let Ok(ty) = input.parse() {
            let mut ops = Vec::new();

            input.parse::<TLBrace>()?;

            while !input.is_empty() && !input.peek::<TRBrace>() {
                ops.push(input.parse()?);

                if !input.peek::<TRBrace>() {
                    input.parse::<TComma>()?;
                }
            }

            input.parse::<TRBrace>()?;

            Ok(Value::Init(ty, ops))
        } else {
            let op = input.parse::<Operand>()?;

            if let Operand::Place(arr) = op {
                if let Ok(_) = input.parse::<TLBracket>() {
                    let lo = input.parse()?;

                    input.parse::<TDots>()?;

                    let hi = input.parse()?;

                    input.parse::<TRBracket>()?;

                    Ok(Value::Slice(arr, lo, hi))
                } else {
                    Ok(Value::Use(Operand::Place(arr)))
                }
            } else {
                Ok(Value::Use(op))
            }
        }
    }
}

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Type> {
        let mut types = vec![input.call(Type::parse_base)?];
        let mut tagged = false;

        while !input.is_empty() && (input.peek::<TBar>() || input.peek::<TSlash>()) {
            let tag = input.peek::<TSlash>();

            if types.len() == 1 {
                tagged = tag;
            } else {
                if tagged != tag {
                    return input.error("all tags must be equal");
                }
            }

            if !input.parse::<TSlash>().is_ok() {
                input.parse::<TBar>()?;
            }

            types.push(input.call(Type::parse_base)?);
        }

        if types.len() == 1 {
            Ok(types.into_iter().next().unwrap())
        } else {
            Ok(Type::Union(tagged, types))
        }
    }
}

impl Type {
    fn parse_base(input: ParseStream) -> Result<Type> {
        if let Ok(_) = input.parse::<TUnit>() {
            Ok(Type::Unit)
        } else if let Ok(_) = input.parse::<TBool>() {
            Ok(Type::Bool)
        } else if let Ok(_) = input.parse::<TChar>() {
            Ok(Type::Char)
        } else if let Ok(_) = input.parse::<TStr>() {
            Ok(Type::Str)
        } else if let Ok(_) = input.parse::<TRatio>() {
            Ok(Type::Ratio)
        } else if let Ok(_) = input.parse::<TU8>() {
            Ok(Type::UInt(IntSize::Bits8))
        } else if let Ok(_) = input.parse::<TU16>() {
            Ok(Type::UInt(IntSize::Bits16))
        } else if let Ok(_) = input.parse::<TU32>() {
            Ok(Type::UInt(IntSize::Bits32))
        } else if let Ok(_) = input.parse::<TU64>() {
            Ok(Type::UInt(IntSize::Bits64))
        } else if let Ok(_) = input.parse::<TU128>() {
            Ok(Type::UInt(IntSize::Bits128))
        } else if let Ok(_) = input.parse::<TUsize>() {
            Ok(Type::UInt(IntSize::Size))
        } else if let Ok(_) = input.parse::<TI8>() {
            Ok(Type::Int(IntSize::Bits8))
        } else if let Ok(_) = input.parse::<TI16>() {
            Ok(Type::Int(IntSize::Bits16))
        } else if let Ok(_) = input.parse::<TI32>() {
            Ok(Type::Int(IntSize::Bits32))
        } else if let Ok(_) = input.parse::<TI64>() {
            Ok(Type::Int(IntSize::Bits64))
        } else if let Ok(_) = input.parse::<TI128>() {
            Ok(Type::Int(IntSize::Bits128))
        } else if let Ok(_) = input.parse::<TIsize>() {
            Ok(Type::Int(IntSize::Size))
        } else if let Ok(_) = input.parse::<TF32>() {
            Ok(Type::Float(FloatSize::Bits32))
        } else if let Ok(_) = input.parse::<TF64>() {
            Ok(Type::Float(FloatSize::Bits64))
        } else if let Ok(_) = input.parse::<TFsize>() {
            Ok(Type::Float(FloatSize::Size))
        } else if let Ok(_) = input.parse::<TAnd>() {
            Ok(Type::Ref(input.parse()?))
        } else if let Ok(_) = input.parse::<TSlash>() {
            let tag = input.parse::<IntLiteral>()?.int as usize;
            let ty = input.parse()?;

            Ok(Type::Tagged(tag, ty))
        } else if let Ok(_) = input.parse::<TLBracket>() {
            let of = input.parse()?;
            let kind = if let Ok(_) = input.parse::<TSemi>() {
                let len = input.parse::<IntLiteral>()?.int as usize;

                Type::Array(of, len)
            } else {
                Type::Slice(of)
            };

            input.parse::<TRBracket>()?;

            Ok(kind)
        } else if let Ok(_) = input.parse::<TLParen>() {
            let mut types = vec![input.parse()?];

            while !input.is_empty() && input.peek::<TComma>() {
                input.parse::<TComma>()?;
                types.push(input.parse()?);
            }

            input.parse::<TRParen>()?;

            if types.len() == 1 {
                Ok(types.into_iter().next().unwrap())
            } else {
                Ok(Type::Tuple(false, types))
            }
        } else if let Ok(_) = input.parse::<TLeft>() {
            let mut types = vec![input.parse()?];

            while !input.is_empty() && input.peek::<TComma>() {
                input.parse::<TComma>()?;
                types.push(input.parse()?);
            }

            input.parse::<TRight>()?;

            if types.len() == 1 {
                Ok(types.into_iter().next().unwrap())
            } else {
                Ok(Type::Tuple(true, types))
            }
        } else {
            Ok(Type::Proc(input.parse()?))
        }
    }
}

impl Parse for CallConv {
    fn parse(input: ParseStream) -> Result<CallConv> {
        let s = input.parse::<StringLiteral>()?.text;

        match s.as_str() {
            "C" => Ok(CallConv::C),
            "Fluix" => Ok(CallConv::Fluix),
            _ => input.error("invalid call convention"),
        }
    }
}

impl Parse for ItemId {
    fn parse(input: ParseStream) -> Result<ItemId> {
        input.parse::<THash>()?;
        input.parse::<IntLiteral>()
            .map(|l| ItemId(l.int as usize))
    }
}

impl Parse for LocalId {
    fn parse(input: ParseStream) -> Result<LocalId> {
        let name = input.parse::<Ident>()?.name;
        let num = name.chars().skip(1).collect::<String>();
        let id = num.parse::<usize>().unwrap();

        Ok(LocalId(id))
    }
}

impl Parse for BlockId {
    fn parse(input: ParseStream) -> Result<BlockId> {
        input.parse::<TPct>()?;
        input.parse::<IntLiteral>()
            .map(|l| BlockId(l.int as usize))
    }
}

impl parser::token::Token for LocalId {
    fn peek(cursor: parser::buffer::Cursor) -> bool {
        match cursor.ident() {
            Some((ident, _)) => {
                ident.name.starts_with('_') &&
                ident.name.chars().skip(1).all(|c| matches!(c, '0'..='9'))
            },
            None => false,
        }
    }

    fn display() -> &'static str {
        "local id"
    }
}
