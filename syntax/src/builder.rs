use crate::*;
use std::collections::BTreeMap;

impl Package {
    pub fn new(name: String) -> Package {
        Package {
            name,
            externs: BTreeMap::new(),
            globals: BTreeMap::new(),
            bodies: BTreeMap::new(),
        }
    }

    fn next_id(&self) -> ItemId {
        ItemId(self.externs.len() + self.globals.len() + self.bodies.len())
    }

    pub fn declare_extern_proc(&mut self, name: String, sig: Signature) -> ItemId {
        let id = self.next_id();

        self.externs.insert(id, Extern::Proc(name, sig));

        id
    }

    pub fn declare_extern_global(&mut self, name: String, ty: Type) -> ItemId {
        let id = self.next_id();

        self.externs.insert(id, Extern::Global(name, ty));

        id
    }

    pub fn declare_global(&mut self, attributes: Attributes, export: bool, name: String, ty: Type) -> ItemId {
        let id = self.next_id();

        self.globals.insert(id, Global {
            attributes,
            export,
            name,
            ty,
            init: None,
        });

        id
    }

    pub fn define_global(&mut self, id: ItemId, value: Box<[u8]>) {
        self.globals.get_mut(&id).unwrap().init = Some(value);
    }

    pub fn declare_body(&mut self, attributes: Attributes, export: bool, name: String, sig: Signature) -> ItemId {
        let id = self.next_id();
        let mut locals = BTreeMap::new();

        for param in sig.1 {
            let id = LocalId(locals.len());

            locals.insert(id, Local {
                id,
                kind: LocalKind::Arg,
                ty: param,
            });
        }

        for ret in sig.2 {
            let id = LocalId(locals.len());

            locals.insert(id, Local {
                id,
                kind: LocalKind::Ret,
                ty: ret,
            });
        }

        self.bodies.insert(id, Body {
            attributes,
            name,
            export,
            conv: sig.0,
            locals,
            blocks: BTreeMap::new(),
        });

        id
    }

    pub fn define_body(&mut self, id: ItemId) -> BodyBuilder {
        let body = self.bodies.get_mut(&id).unwrap();

        BodyBuilder {
            body,
            current_block: None,
        }
    }
}

impl Signature {
    pub fn new() -> Signature {
        Signature(CallConv::Fluix, Vec::new(), Vec::new())
    }

    pub fn call_conv(mut self, conv: CallConv) -> Signature {
        self.0 = conv;
        self
    }

    pub fn arg(mut self, ty: Type) -> Signature {
        self.1.push(ty);
        self
    }

    pub fn ret(mut self, ty: Type) -> Signature {
        self.2.push(ty);
        self
    }
}

impl Place {
    pub fn local(id: LocalId) -> Place {
        Place {
            base: PlaceBase::Local(id),
            elems: Vec::new(),
        }
    }

    pub fn global(id: ItemId) -> Place {
        Place {
            base: PlaceBase::Global(id),
            elems: Vec::new(),
        }
    }

    pub fn deref(mut self) -> Place {
        self.elems.push(PlaceElem::Deref);
        self
    }

    pub fn field(mut self, idx: usize) -> Place {
        self.elems.push(PlaceElem::Field(idx));
        self
    }

    pub fn index(mut self, idx: Place) -> Place {
        self.elems.push(PlaceElem::Index(idx));
        self
    }

    pub fn const_index(mut self, idx: usize) -> Place {
        self.elems.push(PlaceElem::ConstIndex(idx));
        self
    }
}

pub struct BodyBuilder<'a> {
    body: &'a mut Body,
    current_block: Option<BlockId>,
}

impl<'a> BodyBuilder<'a> {
    fn block(&mut self) -> &mut Block {
        self.body.blocks.get_mut(self.current_block.as_ref().unwrap()).unwrap()
    }

    pub fn create_var(&mut self, ty: Type) -> LocalId {
        let id = LocalId(self.body.locals.len());

        self.body.locals.insert(id, Local {
            id,
            kind: LocalKind::Var,
            ty,
        });
        
        id
    }

    pub fn create_tmp(&mut self, ty: Type) -> LocalId {
        let id = LocalId(self.body.locals.len());

        self.body.locals.insert(id, Local {
            id,
            kind: LocalKind::Tmp,
            ty,
        });

        id
    }

    pub fn create_block(&mut self) -> BlockId {
        let id = BlockId(self.body.blocks.len());

        self.body.blocks.insert(id, Block {
            id,
            stmts: Vec::new(),
            term: Terminator::Unset,
        });

        id
    }

    pub fn use_(&mut self, place: Place, op: Operand) {
        self.block().stmts.push(Stmt::Assign(place, Value::Use(op)));
    }

    pub fn ref_(&mut self, place: Place, to: Place) {
        self.block().stmts.push(Stmt::Assign(place, Value::Ref(to)));
    }

    pub fn slice(&mut self, place: Place, arr: Place, lo: Operand, hi: Operand) {
        self.block().stmts.push(Stmt::Assign(place, Value::Slice(arr, lo, hi)));
    }

    pub fn binary(&mut self, place: Place, op: BinOp, lhs: Operand, rhs: Operand) {
        self.block().stmts.push(Stmt::Assign(place, Value::BinOp(op, lhs, rhs)));
    }

    pub fn unary(&mut self, place: Place, op: UnOp, val: Operand) {
        self.block().stmts.push(Stmt::Assign(place, Value::UnOp(op, val)));
    }

    pub fn nullary(&mut self, place: Place, op: NullOp, ty: Type) {
        self.block().stmts.push(Stmt::Assign(place, Value::NullOp(op, ty)));
    }

    pub fn init(&mut self, place: Place, ty: Type, ops: Vec<Operand>) {
        self.block().stmts.push(Stmt::Assign(place, Value::Init(ty, ops)));
    }

    pub fn return_(&mut self) {
        if let Terminator::Unset = self.block().term {
            self.block().term = Terminator::Return;
        }
    }

    pub fn jump(&mut self, target: BlockId) {
        if let Terminator::Unset = self.block().term {
            self.block().term = Terminator::Jump(target);
        }
    }

    pub fn call(&mut self, places: Vec<Place>, proc: Operand, args: Vec<Operand>, target: BlockId) {
        if let Terminator::Unset = self.block().term {
            self.block().term = Terminator::Call(places, proc, args, target);
        }
    }

    pub fn switch(&mut self, op: Operand, vals: Vec<u128>, targets: Vec<BlockId>) {
        if let Terminator::Unset = self.block().term {
            self.block().term = Terminator::Switch(op, vals, targets);
        }
    }
}
