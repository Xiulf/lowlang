use crate::*;
use lowlang_parser::parse::{Parse, ParseStream};
use lowlang_parser::ident::Ident;
use lowlang_parser::literal::StringLiteral;
use lowlang_parser::literal::IntLiteral;
use lowlang_parser::error::Result;
use lowlang_parser::diagnostics::{Diagnostic, Severity};
use std::collections::BTreeMap;

type TI<'a> = &'a crate::ty::TyCtx<'a>;

lowlang_parser::token![ident "package" TPackage];
lowlang_parser::token![ident "extern" TExtern];
lowlang_parser::token![ident "global" TGlobal];
lowlang_parser::token![ident "fn" TFn];
lowlang_parser::token![ident "export" TExport];
lowlang_parser::token![ident "type" TType];
lowlang_parser::token![ident "const" TConst];
lowlang_parser::token![ident "return" TReturn];
lowlang_parser::token![ident "jump" TJump];
lowlang_parser::token![ident "call" TCall];
lowlang_parser::token![ident "switch" TSwitch];
lowlang_parser::token![ident "otherwise" TOtherwise];
lowlang_parser::token![ident "cast" TCast];
lowlang_parser::token![ident "unit" TUnit];
lowlang_parser::token![ident "add" TAdd];
lowlang_parser::token![ident "sub" TSub];
lowlang_parser::token![ident "mul" TMul];
lowlang_parser::token![ident "div" TDiv];
lowlang_parser::token![ident "rem" TRem];
lowlang_parser::token![ident "eq" TEq];
lowlang_parser::token![ident "ne" TNe];
lowlang_parser::token![ident "lt" TLt];
lowlang_parser::token![ident "le" TLe];
lowlang_parser::token![ident "gt" TGt];
lowlang_parser::token![ident "ge" TGe];
lowlang_parser::token![ident "band" TBand];
lowlang_parser::token![ident "bor" TBor];
lowlang_parser::token![ident "bxor" TBxor];
lowlang_parser::token![ident "shl" TShl];
lowlang_parser::token![ident "shr" TShr];
lowlang_parser::token![ident "neg" TNeg];
lowlang_parser::token![ident "not" TNot];
lowlang_parser::token![ident "sizeof" TSizeof];
lowlang_parser::token![ident "alignof" TAlignof];
lowlang_parser::token![ident "bool" TBool];
lowlang_parser::token![ident "char" TChar];
lowlang_parser::token![ident "str" TStr];
lowlang_parser::token![ident "ratio" TRatio];
lowlang_parser::token![ident "i8" TI8];
lowlang_parser::token![ident "i16" TI16];
lowlang_parser::token![ident "i32" TI32];
lowlang_parser::token![ident "i64" TI64];
lowlang_parser::token![ident "i128" TI128];
lowlang_parser::token![ident "isize" TIsize];
lowlang_parser::token![ident "u8" TU8];
lowlang_parser::token![ident "u16" TU16];
lowlang_parser::token![ident "u32" TU32];
lowlang_parser::token![ident "u64" TU64];
lowlang_parser::token![ident "u128" TU128];
lowlang_parser::token![ident "usize" TUsize];
lowlang_parser::token![ident "f32" TF32];
lowlang_parser::token![ident "f64" TF64];
lowlang_parser::token![ident "fsize" TFsize];

lowlang_parser::token![punct "(" TLParen/1];
lowlang_parser::token![punct ")" TRParen/1];
lowlang_parser::token![punct "{" TLBrace/1];
lowlang_parser::token![punct "}" TRBrace/1];
lowlang_parser::token![punct "[" TLBracket/1];
lowlang_parser::token![punct "]" TRBracket/1];
lowlang_parser::token![punct "." TDot/1];
lowlang_parser::token![punct "," TComma/1];
lowlang_parser::token![punct ":" TColon/1];
lowlang_parser::token![punct ";" TSemi/1];
lowlang_parser::token![punct "=" TEquals/1];
lowlang_parser::token![punct "#" THash/1];
lowlang_parser::token![punct "$" TDollar/1];
lowlang_parser::token![punct "!" TBang/1];
lowlang_parser::token![punct "|" TBar/1];
lowlang_parser::token![punct "/" TSlash/1];
lowlang_parser::token![punct "<" TLeft/1];
lowlang_parser::token![punct ">" TRight/1];
lowlang_parser::token![punct "%" TPct/1];
lowlang_parser::token![punct "@" TAt/1];
lowlang_parser::token![punct "&" TAnd/1];
lowlang_parser::token![punct "*" TStar/1];
lowlang_parser::token![punct "->" TArrow/2];
lowlang_parser::token![punct ".." TDots/2];

impl<'t> Parse<TI<'t>> for Package<'t> {
    fn parse(input: ParseStream<TI<'t>>) -> Result<Package<'t>> {
        input.parse::<TPackage>()?;

        let name = input.parse::<Ident>()?.name;
        let mut package = Package {
            name,
            externs: BTreeMap::new(),
            globals: BTreeMap::new(),
            bodies: BTreeMap::new(),
        };
        
        while !input.is_empty() {
            match package.parse_item(input) {
                Ok(_) => {},
                Err(e) => {
                    input.reporter.add(e);

                    while !input.is_empty() && !(
                        input.peek::<TAt>() || input.peek::<TExport>() || input.peek::<TExtern>() ||
                        input.peek::<TGlobal>() || input.peek::<TFn>()
                    ) {
                        input.bump();
                    }
                },
            }
        }

        Ok(package)
    }
}

impl<'t> Package<'t> {
    fn parse_item(&mut self, input: ParseStream<TI<'t>>) -> Result<()> {
        let id = self.next_id();

        if input.peek::<TExtern>() {
            self.externs.insert(id, input.parse()?);
        } else {
            let fork = input.fork();
            let _ = fork.parse::<Attributes>();

            if fork.peek::<TGlobal>() || (fork.peek::<TExport>() && fork.peek2::<TGlobal>()) {
                self.globals.insert(id, input.parse()?);
            } else {
                let mut body = input.parse::<Body>()?;

                body.id = id;

                self.bodies.insert(id, body);
            }
        }

        Ok(())
    }
}

impl<'t> Parse<TI<'t>> for Signature<'t> {
    fn parse(input: ParseStream<TI<'t>>) -> Result<Signature<'t>> {
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

impl<'t> Parse<TI<'t>> for Extern<'t> {
    fn parse(input: ParseStream<TI<'t>>) -> Result<Extern<'t>> {
        input.parse::<TExtern>()?;

        let ext = if let Ok(_) = input.parse::<TFn>() {
            let name = input.parse::<Ident>()?.name;
            let sig = input.parse()?;

            Extern::Proc(name, sig)
        } else {
            input.parse::<Global>()?;

            let name = input.parse::<Ident>()?.name;
            
            input.parse::<TColon>()?;

            Extern::Global(name, input.parse()?)
        };

        Ok(ext)
    }
}

impl<'t> Parse<TI<'t>> for Global<'t> {
    fn parse(input: ParseStream<TI<'t>>) -> Result<Global<'t>> {
        let attributes = input.parse()?;
        let export = input.parse::<TExport>().is_ok();
        
        input.parse::<TGlobal>()?;

        let name = input.parse::<Ident>()?.name;

        input.parse::<TColon>()?;

        let ty = input.parse()?;

        input.parse::<TSemi>()?;

        Ok(Global {
            attributes,
            export,
            name,
            ty,
            init: None,
        })
    }
}

impl<'t> Parse<TI<'t>> for Body<'t> {
    fn parse(input: ParseStream<TI<'t>>) -> Result<Body<'t>> {
        let attributes = input.parse()?;
        let export = input.parse::<TExport>().is_ok();
        
        input.parse::<TFn>()?;

        let name = input.parse::<Ident>()?.name;
        let conv = input.parse()?;
        let mut generics = BTreeMap::new();
        let mut locals = BTreeMap::new();

        if let Ok(_) = input.parse::<TLeft>() {
            while !input.is_empty() && !input.peek::<TRight>() {
                if let Ok(_) = input.parse::<TType>() {
                    let name = input.parse::<Ident>()?.name;
                    let def = if let Ok(_) = input.parse::<TEquals>() {
                        Some(input.parse()?)
                    } else {
                        None
                    };

                    generics.insert(name, GenParam::Type(def));
                } else {
                    input.parse::<TConst>()?;

                    let name = input.parse::<Ident>()?.name;
                    let def = if let Ok(_) = input.parse::<TEquals>() {
                        Some(input.parse()?)
                    } else {
                        None
                    };

                    generics.insert(name, GenParam::Const(def));
                }

                if !input.peek::<TRight>() {
                    input.parse::<TComma>()?;
                }
            }

            input.parse::<TRight>()?;
        }

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
            id: Default::default(),
            attributes,
            export,
            name,
            conv,
            generics,
            locals,
            blocks,
        })
    }
}

impl<D> Parse<D> for Attributes {
    fn parse(input: ParseStream<D>) -> Result<Attributes> {
        let mut attributes = Attributes::default();

        while !input.is_empty() && input.peek::<TAt>() {
            input.parse::<TAt>()?;

            let text = input.parse::<Ident>()?;

            match text.name.as_str() {
                "lang" => attributes.lang = true,
                "inline" => attributes.inline = true,
                "noinline" => attributes.no_inline = true,
                _ => return Err(
                    Diagnostic::new(Severity::Error, None, format!("Unknown attribute: {}", text))
                        .label(Severity::Error, text.span, None::<String>)
                ),
            }
        }

        Ok(attributes)
    }
}

impl<'t> Block<'t> {
    fn parse(input: ParseStream<TI<'t>>, id: BlockId) -> Result<Block<'t>> {
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

impl<'t> Parse<TI<'t>> for Stmt<'t> {
    fn parse(input: ParseStream<TI<'t>>) -> Result<Stmt<'t>> {
        let place = input.parse()?;

        input.parse::<TEquals>()?;

        let value = input.parse()?;

        Ok(Stmt::Assign(place, value))
    }
}

impl<'t> Parse<TI<'t>> for Terminator<'t> {
    fn parse(input: ParseStream<TI<'t>>) -> Result<Terminator<'t>> {
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

impl<D: Copy> Parse<D> for Place {
    fn parse(input: ParseStream<D>) -> Result<Place> {
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

impl<D: Copy> Parse<D> for PlaceBase {
    fn parse(input: ParseStream<D>) -> Result<PlaceBase> {
        if input.peek::<LocalId>() {
            Ok(PlaceBase::Local(input.parse()?))
        } else if input.peek::<Ident>() && !input.peek2::<TLeft>() {
            Ok(PlaceBase::Global(Addr::Name(input.parse::<Ident>()?.name)))
        } else {
            input.error("expected a local id or a global")
        }
    }
}

impl<'t> Parse<TI<'t>> for Operand<'t> {
    fn parse(input: ParseStream<TI<'t>>) -> Result<Operand<'t>> {
        if input.fork().parse::<Place>().is_ok() {
            Ok(Operand::Place(input.parse()?))
        } else {
            Ok(Operand::Constant(input.parse()?))
        }
    }
}

impl<'t> Parse<TI<'t>> for Const<'t> {
    fn parse(input: ParseStream<TI<'t>>) -> Result<Const<'t>> {
        if let Ok(_) = input.parse::<TUnit>() {
            Ok(Const::Unit)
        } else if let Ok(lit) = input.parse::<IntLiteral>() {
            Ok(Const::Scalar(lit.int, input.parse()?))
        } else if let Ok(lit) = input.parse::<StringLiteral>() {
            Ok(Const::Bytes(lit.text.into_bytes().into_boxed_slice()))
        } else if let Ok(_) = input.parse::<TDollar>() {
            Ok(Const::Param(input.parse::<Ident>()?.name))
        } else {
            let id = Addr::Name(input.parse::<Ident>()?.name);
            let mut generics = BTreeMap::new();

            if let Ok(_) = input.parse::<TLeft>() {
                while !input.is_empty() && !input.peek::<TRight>() {
                    let name = input.parse::<Ident>()?.name;

                    input.parse::<TEquals>()?;

                    if let Ok(ty) = input.parse() {
                        generics.insert(name, GenArg::Type(ty));
                    } else {
                        generics.insert(name, GenArg::Const(input.parse()?));
                    }

                    if !input.peek::<TRight>() {
                        input.parse::<TComma>()?;
                    }
                }

                input.parse::<TRight>()?;
            }

            Ok(Const::FuncAddr(id, generics))
        }
    }
}

impl<'t> Parse<TI<'t>> for Value<'t> {
    fn parse(input: ParseStream<TI<'t>>) -> Result<Value<'t>> {
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
        } else if { let fork = input.fork(); fork.parse::<Ty>().is_ok() && fork.peek::<TLBrace>() } {
            let ty = input.parse()?;
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

impl<'t> Parse<TI<'t>> for Ty<'t> {
    fn parse(input: ParseStream<TI<'t>>) -> Result<Ty<'t>> {
        use intern::Intern;
        let mut types = vec![input.call(Ty::parse_base)?];
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

            types.push(input.call(Ty::parse_base)?);
        }

        if types.len() == 1 {
            Ok(types.into_iter().next().unwrap())
        } else {
            Ok(Type::Union(tagged, types).intern(input.data))
        }
    }
}

impl<'t> Ty<'t> {
    fn parse_base(input: ParseStream<TI<'t>>) -> Result<Ty<'t>> {
        use intern::Intern;

        if let Ok(_) = input.parse::<TUnit>() {
            Ok(input.data.defaults.unit)
        } else if let Ok(_) = input.parse::<TBool>() {
            Ok(input.data.defaults.bool)
        } else if let Ok(_) = input.parse::<TChar>() {
            Ok(input.data.defaults.char)
        } else if let Ok(_) = input.parse::<TStr>() {
            Ok(input.data.defaults.str)
        } else if let Ok(_) = input.parse::<TRatio>() {
            Ok(input.data.defaults.ratio)
        } else if let Ok(_) = input.parse::<TU8>() {
            Ok(input.data.defaults.u8)
        } else if let Ok(_) = input.parse::<TU16>() {
            Ok(input.data.defaults.u16)
        } else if let Ok(_) = input.parse::<TU32>() {
            Ok(input.data.defaults.u32)
        } else if let Ok(_) = input.parse::<TU64>() {
            Ok(input.data.defaults.u64)
        } else if let Ok(_) = input.parse::<TU128>() {
            Ok(input.data.defaults.u128)
        } else if let Ok(_) = input.parse::<TUsize>() {
            Ok(input.data.defaults.usize)
        } else if let Ok(_) = input.parse::<TI8>() {
            Ok(input.data.defaults.i8)
        } else if let Ok(_) = input.parse::<TI16>() {
            Ok(input.data.defaults.i16)
        } else if let Ok(_) = input.parse::<TI32>() {
            Ok(input.data.defaults.i32)
        } else if let Ok(_) = input.parse::<TI64>() {
            Ok(input.data.defaults.i64)
        } else if let Ok(_) = input.parse::<TI128>() {
            Ok(input.data.defaults.i128)
        } else if let Ok(_) = input.parse::<TIsize>() {
            Ok(input.data.defaults.isize)
        } else if let Ok(_) = input.parse::<TF32>() {
            Ok(input.data.defaults.f32)
        } else if let Ok(_) = input.parse::<TF64>() {
            Ok(input.data.defaults.f64)
        } else if let Ok(_) = input.parse::<TFsize>() {
            Ok(input.data.defaults.fsize)
        } else if let Ok(_) = input.parse::<TAnd>() {
            Ok(Type::Ref(input.parse()?).intern(input.data))
        } else if let Ok(_) = input.parse::<TSlash>() {
            let tag = input.parse::<IntLiteral>()?.int as usize;
            let ty = input.parse()?;

            Ok(Type::Tagged(tag, ty).intern(input.data))
        } else if let Ok(_) = input.parse::<TLBracket>() {
            let of = input.parse()?;
            let kind = if let Ok(_) = input.parse::<TSemi>() {
                let len = input.parse::<IntLiteral>()?.int as usize;

                Type::Array(of, len)
            } else {
                Type::Slice(of)
            };

            input.parse::<TRBracket>()?;

            Ok(kind.intern(input.data))
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
                Ok(Type::Tuple(false, types).intern(input.data))
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
                Ok(Type::Tuple(true, types).intern(input.data))
            }
        } else if let Ok(_) = input.parse::<TFn>() {
            Ok(Type::Proc(input.parse()?).intern(input.data))
        } else if !input.peek::<LocalId>() {
            Ok(Type::Param(input.parse::<Ident>()?.name).intern(input.data))
        } else {
            input.error("Expected a type")
        }
    }
}

impl<D> Parse<D> for CallConv {
    fn parse(input: ParseStream<D>) -> Result<CallConv> {
        let s = input.parse::<StringLiteral>()?.text;

        match s.as_str() {
            "c" => Ok(CallConv::C),
            "ll" => Ok(CallConv::Lowlang),
            _ => input.error("invalid call convention"),
        }
    }
}

impl<D> Parse<D> for ItemId {
    fn parse(input: ParseStream<D>) -> Result<ItemId> {
        input.parse::<THash>()?;
        input.parse::<IntLiteral>()
            .map(|l| ItemId(l.int as usize))
    }
}

impl<D> Parse<D> for LocalId {
    fn parse(input: ParseStream<D>) -> Result<LocalId> {
        let name = input.parse::<Ident>()?.name;
        let num = name.chars().skip(1).collect::<String>();
        let id = num.parse::<usize>().map_err(|_| input.error::<usize, _>("invalid local id").unwrap_err())?;

        Ok(LocalId(id))
    }
}

impl<D> Parse<D> for BlockId {
    fn parse(input: ParseStream<D>) -> Result<BlockId> {
        input.parse::<TPct>()?;
        input.parse::<IntLiteral>()
            .map(|l| BlockId(l.int as usize))
    }
}

impl lowlang_parser::token::Token for LocalId {
    fn peek(cursor: lowlang_parser::buffer::Cursor) -> bool {
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
