use crate::ctx::{Ctx, TypeCtx, Var};
use syntax::*;
use diagnostics::{Diagnostic, Severity};

pub fn typecheck(ctx: &mut TypeCtx, program: &Program) {
    for f in &program.fns {
        let params = f.params.iter().map(|p| p.1.clone()).collect();
        
        ctx.add_fn(f.name.text.clone(), (params, f.ret.clone()));
    }
    
    for f in &program.fns {
        typecheck_fn(ctx, f);
    }
}

fn typecheck_fn(ctx: &mut TypeCtx, f: &Function) {
    for (id, ty) in &f.params {
        ctx.add_var(*id, ty.clone());
        ctx.var_alive(*id);
    }
    
    for b in &f.bindings {
        ctx.add_var(b.0, b.1.clone());
    }
    
    match ctx.get_var(&LocalId(0)) {
        None => ctx.reporter.add(Diagnostic::new(Severity::Error, Default::default(), format!("function '{}' has no return address", f.name))),
        Some(Var::Dead(ty)) | Some(Var::Alive(ty)) => {
            if ty != &f.ret {
                ctx.reporter.add(Diagnostic::new(Severity::Error, Default::default(), format!("type mismatch: `{}` != `{}`", ty, f.ret)));
            }
        },
    }
    
    ctx.var_alive(LocalId(0));
    
    for block in &f.blocks {
        for stmt in &block.statements {
            typecheck_stmt(ctx, stmt);
        }
        
        typecheck_term(ctx, &block.terminator);
    }
}

fn typecheck_stmt(ctx: &mut TypeCtx, stmt: &Statement) {
    match stmt {
        Statement::StorageLive(id) => ctx.var_alive(*id),
        Statement::StorageDead(id) => ctx.var_dead(*id),
        Statement::Free(p) => {
            let ty = typecheck_place(ctx, p);
            
            match ty {
                Type::Ptr(_) => (),
                _ => ctx.reporter.add(Diagnostic::new(Severity::Error, Default::default(), "expected a pointer"))
            }
        },
        Statement::Assign(p, v) => {
            let p_ty = typecheck_place(ctx, p);
            let v_ty = typecheck_rvalue(ctx, v);
            
            if p_ty != v_ty {
                ctx.reporter.add(
                    Diagnostic::new(Severity::Error, v.span(), format!("type mismatch: `{}` != `{}`", v_ty, p_ty))
                        .width_label(Severity::Info, p.span, Some(format!("'{}' has type `{}`", p, p_ty)))
                );
            }
        }
    }
}

fn typecheck_term(ctx: &mut TypeCtx, term: &Terminator) {
    
}

fn typecheck_rvalue(ctx: &mut TypeCtx, rv: &RValue) -> Type {
    match rv {
        RValue::Use(op) => typecheck_operand(ctx, op),
        RValue::Ref(p, _) => Type::Ptr(Box::new(typecheck_place(ctx, p))),
        RValue::Alloc(op, ty, _) => {
            match typecheck_operand(ctx, op) {
                Type::UInt(_) => (),
                _ => ctx.reporter.add(Diagnostic::new(Severity::Error, Default::default(), "alloc expects an unsigned integer for its size"))
            }
            
            Type::Ptr(Box::new(ty.clone()))
        },
        RValue::Binary(op, lhs, rhs, _) => {
            let lhs_ty = typecheck_operand(ctx, lhs);
            let rhs_ty = typecheck_operand(ctx, rhs);
            
            match op {
                BinOp::Add |
                BinOp::Sub |
                BinOp::Mul |
                BinOp::Div |
                BinOp::Mod => {
                    if lhs_ty != rhs_ty {
                        ctx.reporter.add(Diagnostic::new(Severity::Error, Default::default(), format!("type mismatch: `{}` != `{}`", lhs_ty, rhs_ty)));
                    }
                    
                    match &lhs_ty {
                        Type::Int(_) | Type::UInt(_) | Type::Float(_) => (),
                        _ => ctx.reporter.add(Diagnostic::new(Severity::Error, Default::default(), format!("the {:?} operator only accepts int, uint and float types", op)))
                    }
                    
                    lhs_ty
                },
                BinOp::BitAnd |
                BinOp::BitOr |
                BinOp::BitXor |
                BinOp::Shl |
                BinOp::Shr => {
                    if lhs_ty != rhs_ty {
                        ctx.reporter.add(Diagnostic::new(Severity::Error, Default::default(), format!("type mismatch: `{}` != `{}`", lhs_ty, rhs_ty)));
                    }
                    
                    match &lhs_ty {
                        Type::Int(_) | Type::UInt(_) => (),
                        _ => ctx.reporter.add(Diagnostic::new(Severity::Error, Default::default(), format!("the {:?} operator only accepts int and uint types", op)))
                    }
                    
                    lhs_ty
                },
                BinOp::Lt |
                BinOp::Le |
                BinOp::Gt |
                BinOp::Ge => {
                    if lhs_ty != rhs_ty {
                        ctx.reporter.add(Diagnostic::new(Severity::Error, Default::default(), format!("type mismatch: `{}` != `{}`", lhs_ty, rhs_ty)));
                    }
                    
                    match &lhs_ty {
                        Type::Int(_) | Type::UInt(_) | Type::Float(_) => (),
                        _ => ctx.reporter.add(Diagnostic::new(Severity::Error, Default::default(), format!("the {:?} operator only accepts int, uint and float types", op)))
                    }
                    
                    Type::Bool
                },
                BinOp::Eq |
                BinOp::Ne => {
                    if lhs_ty != rhs_ty {
                        ctx.reporter.add(Diagnostic::new(Severity::Error, Default::default(), format!("type mismatch: `{}` != `{}`", lhs_ty, rhs_ty)));
                    }
                    
                    Type::Bool
                }
            }
        },
        _ => unimplemented!()
    }
}

fn typecheck_operand(ctx: &mut TypeCtx, op: &Operand) -> Type {
    match op {
        Operand::Copy(p, _) => typecheck_place(ctx, p),
        Operand::Move(p, _) => typecheck_place(ctx, p),
        Operand::Constant(c, _) => typecheck_constant(ctx, c),
    }
}

fn typecheck_place(ctx: &mut TypeCtx, place: &Place) -> Type {
    let (mut ty, start) = match &place.base {
        PlaceBase::Local(id, span) => match ctx.get_var(id) {
            Some(Var::Alive(ty)) => (ty.clone(), *span),
            Some(Var::Dead(ty)) => {
                ctx.reporter.add(Diagnostic::new(Severity::Error, *span, format!("use of dead variable '{}'", id)));
                
                (ty.clone(), *span)
            },
            None => {
                ctx.reporter.add(Diagnostic::new(Severity::Error, *span, format!("undeclared variable '{}'", id)));
                
                (Type::Unit, *span)
            }
        },
    };
    
    for proj in place.projection.iter().rev() {
        match proj {
            PlaceElem::Deref(span) => match &ty {
                Type::Ptr(t) => ty = *t.clone(),
                _ => ctx.reporter.add(Diagnostic::new(Severity::Error, span.to(start), format!("type `{}` cannot be dereferenced", ty)))
            },
            PlaceElem::Field(i, span) => match &ty {
                Type::Tuple(tys) => if *i < tys.len() {
                    ty = tys.into_iter().nth(*i).unwrap().clone()
                } else {
                    ctx.reporter.add(Diagnostic::new(Severity::Error, start.to(*span), format!("no field '{}' on type `{}`", i, ty)));
                },
                _ => ctx.reporter.add(Diagnostic::new(Severity::Error, start, format!("type `{}` is not a tuple", ty)))
            }
        }
    }
    
    ty
}

fn typecheck_constant(ctx: &mut TypeCtx, c: &Constant) -> Type {
    match c {
        Constant::Bool(_) => Type::Bool,
        Constant::Tuple(tys) if tys.len() == 0 => Type::Unit,
        Constant::Tuple(tys) => {
            let tys = tys.iter().map(|t| typecheck_constant(ctx, t)).collect();
            
            Type::Tuple(tys)
        },
        Constant::UInt(_, ty) => Type::UInt(ty.clone()),
        Constant::Int(_, ty) => Type::Int(ty.clone()),
        Constant::Float(_, ty) => Type::Float(ty.clone()),
        Constant::Item(id) => match ctx.get_fn(&id.text) {
            Some(ty) => Type::Fn(ty.0.clone(), Box::new(ty.1.clone())),
            None => {
                ctx.reporter.add(Diagnostic::new(Severity::Error, Default::default(), format!("undefined function '{}'", id)));
                
                Type::Unit
            },
        },
        Constant::Bytes(..) => unimplemented!()
    }
}