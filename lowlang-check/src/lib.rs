use lowlang_syntax::*;
use intern::Intern;

pub struct Diagnostic {
    pub message: String,
    pub severity: diagnostics::Severity,
    pub location: Location,
}

pub fn verify<'t>(package: &Package<'t>, tcx: &TyCtx<'t>) {
    typecheck(package, tcx);
}

pub fn typecheck<'t>(package: &Package<'t>, tcx: &TyCtx<'t>) {
    let checker = TypeChecker { tcx, package };

    match checker.check() {
        Ok(_) => {},
        Err(errors) => {
            for e in errors {
                e.display(package);
            }

            std::process::exit(1);
        },
    }
}

struct TypeChecker<'a, 't> {
    tcx: &'a TyCtx<'t>,
    package: &'a Package<'t>,
}

impl<'a, 't> TypeChecker<'a, 't> {
    fn check(&self) -> Result<(), Vec<Diagnostic>> {
        let mut errors = Vec::new();

        for (_, body) in &self.package.bodies {
            match self.check_body(body) {
                Ok(_) => {},
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn check_body(&self, body: &Body<'t>) -> Result<(), Diagnostic> {
        for (_, block) in &body.blocks {
            self.check_block(body, block)?;
        }

        Ok(())
    }

    fn type_of_proc(&self, id: ItemId, loc: Location) -> Result<Ty<'t>, Diagnostic> {
        if let Some(ext) = self.package.externs.get(&id) {
            match ext {
                Extern::Proc(_, sig) => Ok(Type::Proc(sig.clone()).intern(self.tcx)),
                Extern::Global(_, _) => Err(Diagnostic::error("Expected a procedure, found a global", loc)),
            }
        } else {
            let body = &self.package.bodies[&id];
            let params = body.args().iter().map(|p| p.ty).collect();
            let rets = body.rets().iter().map(|r| r.ty).collect();
            let sig = Signature(body.conv, params, rets);

            Ok(Type::Proc(sig).intern(self.tcx))
        }
    }

    fn check_block(&self, body: &Body<'t>, block: &Block<'t>) -> Result<(), Diagnostic> {
        let Block { id, stmts, term } = block;
        let mut loc = Location { item: body.id, block: *id, stmt: 0 };

        for stmt in stmts {
            self.check_stmt(body, stmt, loc)?;
            loc.stmt += 1;
        }

        self.check_term(body, term, loc)?;

        Ok(())
    }

    fn check_stmt(&self, body: &Body<'t>, stmt: &Stmt<'t>, loc: Location) -> Result<(), Diagnostic> {
        match stmt {
            Stmt::Assign(place, value) => {
                let place_ty = self.type_of_place(body, place, loc)?;
                let value_ty = self.type_of_value(body, value, loc)?;

                self.unify(place_ty, value_ty, loc)?;
            },
        }

        Ok(())
    }

    fn check_term(&self, body: &Body<'t>, term: &Terminator<'t>, loc: Location) -> Result<(), Diagnostic> {
        match term {
            Terminator::Unset => return Err(Diagnostic::error("Unset terminator", loc)),
            Terminator::Abort => {},
            Terminator::Return => {},
            Terminator::Jump(_) => {},
            Terminator::Call(places, proc, args, _) => {
                let places = places.iter()
                    .map(|place| self.type_of_place(body, place, loc))
                    .collect::<Result<Vec<_>, _>>()?;
                let proc = self.type_of_op(body, proc, loc)?;
                let args = args.iter()
                    .map(|arg| self.type_of_op(body, arg, loc))
                    .collect::<Result<Vec<_>, _>>()?;

                match &*proc {
                    Type::Proc(sig) => {
                        if places.len() != sig.2.len() {
                            return Err(Diagnostic::error("Wrong number of places", loc));
                        }

                        for (place, ret) in places.iter().zip(sig.2.iter()) {
                            self.unify(*place, *ret, loc)?;
                        }

                        if args.len() != sig.1.len() {
                            return Err(Diagnostic::error("Wrong number of arguments given", loc));
                        }

                        for (arg, param) in args.iter().zip(sig.1.iter()) {
                            self.unify(*arg, *param, loc)?;
                        }
                    },
                    _ => return Err(Diagnostic::error(format!("Cannot call type `{}`", proc), loc)),
                }
            },
            Terminator::Switch(pred, _, _) => {
                let pred_ty = self.type_of_op(body, pred, loc)?;

                self.unify(pred_ty, self.tcx.defaults.bool, loc)?;
            },
        }

        Ok(())
    }

    fn type_of_place(&self, body: &Body<'t>, place: &Place, loc: Location) -> Result<Ty<'t>, Diagnostic> {
        let mut ty = match &place.base {
            PlaceBase::Local(id) => body.locals[id].ty,
            PlaceBase::Global(addr) => self.package.globals[&addr.id()].ty,
        };

        for elem in &place.elems {
            match elem {
                PlaceElem::Deref => match &*ty {
                    Type::Ref(to) => ty = *to,
                    _ => return Err(Diagnostic::error(format!("Type `{}` cannot be dereferenced", ty), loc)),
                },
                PlaceElem::Field(n) => match &*ty {
                    Type::Str => match n {
                        0 => ty = self.tcx.defaults.ptr_u8,
                        1 => ty = self.tcx.defaults.usize,
                        _ => return Err(Diagnostic::error(format!("Type `{}` only has 2 fields", ty), loc)),
                    },
                    Type::Slice(of) => match n {
                        0 => ty = Type::Ref(*of).intern(self.tcx),
                        1 => ty = self.tcx.defaults.usize,
                        _ => return Err(Diagnostic::error(format!("Type `{}` only has 2 fields", ty), loc)),
                    },
                    Type::Ratio => match n {
                        0 | 1=> ty = self.tcx.defaults.isize,
                        _ => return Err(Diagnostic::error(format!("Type `{}` only has 2 fields", ty), loc)),
                    },
                    Type::Tuple(_, fields) => {
                        if let Some(field) = fields.get(*n) {
                            ty = *field;
                        } else {
                            return Err(Diagnostic::error(format!("Type `{}` only has {} fields", ty, fields.len()), loc));
                        }
                    },
                    Type::Union(true, fields) => match n {
                        0 => ty = self.tcx.defaults.usize,
                        1 => ty = Type::Union(false, fields.clone()).intern(self.tcx),
                        _ => return Err(Diagnostic::error(format!("Type `{}` only has 2 fields", ty), loc)),
                    },
                    _ => return Err(Diagnostic::error(format!("Type `{}` has no fields", ty), loc)),
                },
                PlaceElem::Index(idx) => {
                    let idx_ty = self.type_of_place(body, idx, loc)?;

                    self.unify(idx_ty, self.tcx.defaults.usize, loc)?;

                    match &*ty {
                        Type::Str => ty = self.tcx.defaults.u8,
                        Type::Slice(of) => ty = *of,
                        Type::Array(of, _) => ty = *of,
                        Type::Vector(of, _) => ty = *of,
                        _ => return Err(Diagnostic::error(format!("Cannot index into type `{}`", ty), loc)),
                    }
                },
                PlaceElem::ConstIndex(idx) => match &*ty {
                    Type::Str => ty = self.tcx.defaults.u8,
                    Type::Slice(of) => ty = *of,
                    &Type::Array(of, len) |
                    &Type::Vector(of, len) => {
                        ty = of;

                        if *idx >= len {
                            return Err(Diagnostic::error("Constant index out of bounds", loc));
                        }
                    },
                    _ => return Err(Diagnostic::error(format!("Cannot index into type `{}`", ty), loc)),
                },
            }
        }

        Ok(ty)
    }

    fn type_of_op(&self, body: &Body<'t>, op: &Operand<'t>, loc: Location) -> Result<Ty<'t>, Diagnostic> {
        match op {
            Operand::Place(place) => self.type_of_place(body, place, loc),
            Operand::Constant(c) => self.type_of_const(c, loc),
        }
    }

    fn type_of_const(&self, const_: &Const<'t>, loc: Location) -> Result<Ty<'t>, Diagnostic> {
        match const_ {
            Const::Unit => Ok(self.tcx.defaults.unit),
            Const::Scalar(_, ty) => Ok(*ty),
            Const::Bytes(_) => Ok(self.tcx.defaults.str),
            Const::Param(name) => Ok(Type::Param(name.clone()).intern(self.tcx)),
            Const::FuncAddr(addr, subst) => {
                let ty = self.type_of_proc(addr.id(), loc)?;

                Ok(lowlang_syntax::mono::subst_ty(&ty, subst, self.tcx))
            },
        }
    }

    fn type_of_value(&self, body: &Body<'t>, value: &Value<'t>, loc: Location) -> Result<Ty<'t>, Diagnostic> {
        match value {
            Value::Use(op) => self.type_of_op(body, op, loc),
            Value::Ref(place) => Ok(Type::Ref(self.type_of_place(body, place, loc)?).intern(self.tcx)),
            Value::Cast(ty, op) => {
                self.type_of_op(body, op, loc)?;

                Ok(*ty)
            },
            Value::Slice(place, lo, hi) => {
                let place_ty = self.type_of_place(body, place, loc)?;
                let lo_ty = self.type_of_op(body, lo, loc)?;
                let hi_ty = self.type_of_op(body, hi, loc)?;

                self.unify(lo_ty, self.tcx.defaults.usize, loc)?;
                self.unify(hi_ty, self.tcx.defaults.usize, loc)?;

                match &*place_ty {
                    Type::Str => Ok(place_ty),
                    Type::Slice(_) => Ok(place_ty),
                    Type::Array(of, _) => Ok(Type::Slice(*of).intern(self.tcx)),
                    _ => return Err(Diagnostic::error(format!("Cannot slice type `{}`", place_ty), loc)),
                }
            },
            Value::BinOp(op, lhs, rhs) => {
                let lhs_ty = self.type_of_op(body, lhs, loc)?;
                let rhs_ty = self.type_of_op(body, rhs, loc)?;

                self.unify(lhs_ty, rhs_ty, loc)?;

                match op {
                    BinOp::Add | BinOp::Sub |
                    BinOp::Mul | BinOp::Div |
                    BinOp::Rem |
                    BinOp::Shl | BinOp::Shr |
                    BinOp::BitAnd | BinOp::BitOr |
                    BinOp::BitXOr => Ok(lhs_ty),
                    BinOp::Eq | BinOp::Ne |
                    BinOp::Lt | BinOp::Le |
                    BinOp::Gt | BinOp::Ge => Ok(self.tcx.defaults.bool),
                }
            },
            Value::UnOp(op, lhs) => {
                let lhs_ty = self.type_of_op(body, lhs, loc)?;

                match op {
                    UnOp::Neg => Ok(lhs_ty),
                    UnOp::Not => Ok(lhs_ty),
                }
            },
            Value::NullOp(op, _) => {
                match op {
                    NullOp::SizeOf => Ok(self.tcx.defaults.usize),
                    NullOp::AlignOf => Ok(self.tcx.defaults.usize),
                }
            },
            Value::Init(ty, ops) => {
                Ok(*ty)
            },
        }
    }

    fn unify(&self, a: Ty<'t>, b: Ty<'t>, loc: Location) -> Result<(), Diagnostic> {
        match (&*a, &*b) {
            (Type::Unit, Type::Unit) |
            (Type::Bool, Type::Bool) |
            (Type::Char, Type::Char) |
            (Type::Str, Type::Str) |
            (Type::Ratio, Type::Ratio) => Ok(()),
            (Type::Param(a), Type::Param(b)) if a == b => Ok(()),
            (Type::Int(a), Type::Int(b)) if a == b => Ok(()),
            (Type::UInt(a), Type::UInt(b)) if a == b => Ok(()),
            (Type::Float(a), Type::Float(b)) if a == b => Ok(()),
            (Type::Ref(a), Type::Ref(b)) => self.unify(*a, *b, loc),
            (Type::Array(a, la), Type::Array(b, lb)) if la == lb => self.unify(*a, *b, loc),
            (Type::Slice(a), Type::Slice(b)) => self.unify(*a, *b, loc),
            (Type::Vector(a, la), Type::Vector(b, lb)) if la == lb => self.unify(*a, *b, loc),
            (Type::Proc(a), Type::Proc(b)) => {
                if a.0 != b.0 {
                    return Err(Diagnostic::error("Inequal calling conventions", loc));
                }

                if a.1.len() != b.1.len() {
                    return Err(Diagnostic::error("Inequal number of parameters", loc));
                }

                for (a, b) in a.1.iter().zip(b.1.iter()) {
                    self.unify(*a, *b, loc)?;
                }

                if a.2.len() != b.2.len() {
                    return Err(Diagnostic::error("Inequal number of returns", loc));
                }

                for (a, b) in a.2.iter().zip(b.2.iter()) {
                    self.unify(*a, *b, loc)?;
                }
                
                Ok(())
            },
            (Type::Tuple(pa, a), Type::Tuple(pb, b)) if pa == pb && a.len() == b.len() => {
                for (a, b) in a.iter().zip(b.iter()) {
                    self.unify(*a, *b, loc)?;
                }

                Ok(())
            },
            (Type::Union(ta, a), Type::Union(tb, b)) if ta == tb && a.len() == b.len() => {
                if *ta {
                    for (a, b) in a.iter().zip(b.iter()) {
                        self.unify(*a, *b, loc)?;
                    }
                } else {
                    let mut found = 0;

                    for a in a.iter() {
                        if b.iter().any(|b| self.unify(*a, *b, loc).is_ok()) {
                            found += 1;
                        }
                    }

                    if found != a.len() {
                        return Err(Diagnostic::error("These unions don't share the same types", loc));
                    }
                }

                Ok(())
            },
            _ => Err(Diagnostic::error(format!("Mismatched types: `{}` != `{}`", a, b), loc))
        }
    }
}

impl Diagnostic {
    pub fn error(msg: impl Into<String>, loc: Location) -> Diagnostic {
        Diagnostic {
            message: msg.into(),
            severity: diagnostics::Severity::Error,
            location: loc,
        }
    }

    pub fn display(&self, package: &Package) {
        let body = &package.bodies[&self.location.item];
        let block = &body.blocks[&self.location.block];

        println!("[{}] {}", self.severity.to_string(), self.message);

        if self.location.stmt == block.stmts.len() {
            println!("{}", block.term);
        } else {
            println!("{}", block.stmts[self.location.stmt]);
        }
    }
}
