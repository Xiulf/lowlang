use crate::db::IrDatabase;
use crate::*;
use logos::{Lexer, Logos};
use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Logos)]
pub enum Token<'a> {
    #[error]
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    #[regex(r";[^\n]*\n", logos::skip)]
    Error,

    #[regex(r"-?[0-9]+", |lex| lex.slice().parse())]
    Int(i128),

    #[regex(r#""(\\"|[^"])+""#, lex_string)]
    String(Cow<'a, str>),

    #[regex(r"[\pL_][\pL\pN_]*'*")]
    Ident(&'a str),

    #[regex(r"[\pL_][\pL\pN_]*'*(/[\pL_][\pL\pN_]*'*)+")]
    Path(&'a str),

    #[regex(r"\[\w+\]", |lex| &lex.slice()[1..lex.slice().len() - 1])]
    Flag(&'a str),

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token("=")]
    Equals,

    #[token("*")]
    Star,

    #[token("$")]
    Dollar,

    #[token("/")]
    Slash,

    #[token("->")]
    Arrow,

    #[token("<")]
    LAngle,

    #[token(">")]
    RAngle,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,
}

fn lex_string<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<Cow<'a, str>> {
    let text = &lex.slice()[1..lex.slice().len() - 1];

    if text.contains('\\') {
        let mut new = std::string::String::with_capacity(text.len());
        let mut chars = text.chars();

        while let Some(ch) = chars.next() {
            if ch == '\\' {
                match chars.next()? {
                    | '"' => new.push('"'),
                    | '\'' => new.push('\''),
                    | '\\' => new.push('\\'),
                    | 't' => new.push('\t'),
                    | 'r' => new.push('\r'),
                    | 'n' => new.push('\n'),
                    | '0' => new.push('\0'),
                    | _ => return None,
                }
            } else {
                new.push(ch);
            }
        }

        Some(Cow::Owned(new))
    } else {
        Some(Cow::Borrowed(text))
    }
}

use std::iter::Peekable;
use Token::*;

pub struct Parser<'a> {
    db: &'a dyn IrDatabase,
    lexer: Peekable<Lexer<'a, Token<'a>>>,
    module: Module,
    types: HashMap<&'a str, TypeDefId>,
    funcs: HashMap<&'a str, FuncId>,
    generics: Vec<HashMap<&'a str, GenericVar>>,
}

struct BodyParser<'a, 'b> {
    parser: &'b mut Parser<'a>,
    builder: builder::Builder<'b>,
    blocks: HashMap<&'a str, Block>,
    vars: HashMap<&'a str, Var>,
    block_refs: Vec<(&'a str, BlockRef)>,
}

struct BlockRef {
    block: Block,
    case: usize,
}

impl<'a> Parser<'a> {
    pub fn new(db: &'a dyn IrDatabase, source: &'a str) -> Self {
        Parser {
            db,
            lexer: Token::lexer(source).peekable(),
            module: Module::new(""),
            types: HashMap::new(),
            funcs: HashMap::new(),
            generics: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Option<Module> {
        let _ = self.expect(Ident("module"))?;
        let name = self.def_name()?;

        self.module.name = name.to_string();

        while !self.lexer.peek().is_none() {
            if let None = self.parse_item() {
                break;
            }
        }

        Some(self.module)
    }

    fn parse_item(&mut self) -> Option<()> {
        match self.lexer.next()? {
            | Ident("import") => self.parse_linkage(Linkage::Import),
            | Ident("export") => self.parse_linkage(Linkage::Export),
            | Ident("local") => self.parse_linkage(Linkage::Local),
            | Ident("struct") => self.parse_struct(false),
            | Ident("union") => self.parse_struct(true),
            | Ident("enum") => self.parse_enum(),
            | Ident("body") => self.parse_body(),
            | _ => None,
        }
    }

    fn parse_linkage(&mut self, linkage: Linkage) -> Option<()> {
        match self.lexer.peek()? {
            | Ident("type") => {
                self.lexer.next()?;
                self.parse_type_decl(linkage)
            },
            | _ => self.parse_func(linkage),
        }
    }

    fn parse_type_decl(&mut self, linkage: Linkage) -> Option<()> {
        let name = self.def_name()?;
        let id = TypeDef::declare(self.db, name);

        self.types.insert(name, id);
        self.module.add_type(linkage, id);
        Some(())
    }

    fn parse_func(&mut self, linkage: Linkage) -> Option<()> {
        let name = self.def_name()?;
        let _ = self.expect(Token::Colon)?;
        let _ = self.expect(Token::Dollar)?;
        let ty = self.parse_type()?;
        let id = self.module.declare_func(name, linkage, ty);

        self.funcs.insert(name, id);
        Some(())
    }

    fn parse_struct(&mut self, is_union: bool) -> Option<()> {
        let name = self.def_name()?;
        let id = self.types[name];

        if self.eat(LAngle) {
            self.generics.push(HashMap::new());

            while self.lexer.peek() != Some(&RAngle) {
                let kind = match self.lexer.next()? {
                    | Ident("type") => GenericParam::Type,
                    | Ident("figure") => GenericParam::Figure,
                    | Ident("symbol") => GenericParam::Symbol,
                    | _ => return None,
                };

                let name = self.ident()?;
                let id = id.lookup(self.db).add_generic_param(kind);

                self.generics.last_mut().unwrap().insert(name, id);
            }

            self.expect(RAngle)?;
        }

        let _ = self.expect(LBrace)?;
        let mut fields = Vec::new();

        while self.lexer.peek() != Some(&RBrace) {
            let name = self.ident()?;
            let _ = self.expect(Colon)?;
            let _ = self.expect(Dollar)?;
            let ty = self.parse_type()?;

            fields.push(TypeDefField { name: name.to_string(), ty });

            if self.lexer.peek() != Some(&RBrace) {
                self.expect(Comma)?;
            }
        }

        self.expect(RBrace)?;

        if is_union {
            id.lookup(self.db).define_union(fields);
        } else {
            id.lookup(self.db).define_struct(fields);
        }

        Some(())
    }

    fn parse_enum(&mut self) -> Option<()> {
        let name = self.def_name()?;
        let id = self.types[name];

        if self.eat(LAngle) {
            self.generics.push(HashMap::new());

            while self.lexer.peek() != Some(&RAngle) {
                let kind = match self.lexer.next()? {
                    | Ident("type") => GenericParam::Type,
                    | Ident("figure") => GenericParam::Figure,
                    | Ident("symbol") => GenericParam::Symbol,
                    | _ => return None,
                };

                let name = self.ident()?;
                let id = id.lookup(self.db).add_generic_param(kind);

                self.generics.last_mut().unwrap().insert(name, id);
            }

            self.expect(RAngle)?;
        }

        let _ = self.expect(LBrace)?;
        let mut variants = Vec::new();

        while self.lexer.peek() != Some(&RBrace) {
            let name = self.ident()?;
            let payload = if self.eat(Colon) {
                self.expect(Dollar)?;
                Some(self.parse_type()?)
            } else {
                None
            };

            variants.push(TypeDefVariant {
                name: name.to_string(),
                payload,
            });

            if self.lexer.peek() != Some(&RBrace) {
                self.expect(Comma)?;
            }
        }

        self.expect(RBrace)?;
        id.lookup(self.db).define_enum(variants);

        Some(())
    }

    fn parse_body(&mut self) -> Option<()> {
        let parser = unsafe { &mut *(self as *mut _) };
        let body = self.module.declare_body();
        let builder = self.module.define_body(self.db, body);
        let parser = BodyParser {
            parser,
            builder,
            blocks: HashMap::new(),
            vars: HashMap::new(),
            block_refs: Vec::new(),
        };

        parser.parse(body)
    }

    fn parse_type(&mut self) -> Option<Ty> {
        let owned = self.eat(Flag("owned"));
        let non_null = self.eat(Flag("non_null"));
        let mut ty = match self.lexer.next()? {
            | Star => self.parse_type().map(|t| t.ptr(self.db)),
            | Ident("box") => self.parse_type().map(|t| t.gen_box(self.db)),
            | Ident("rc") => self.parse_type().map(|t| t.rc_box(self.db)),
            | Ident(other) => match other {
                | "u8" => Some(Ty::int(self.db, Integer::I8, false)),
                | "u16" => Some(Ty::int(self.db, Integer::I16, false)),
                | "u32" => Some(Ty::int(self.db, Integer::I32, false)),
                | "u64" => Some(Ty::int(self.db, Integer::I64, false)),
                | "u128" => Some(Ty::int(self.db, Integer::I128, false)),
                | "usize" => Some(Ty::int(self.db, Integer::ISize, false)),
                | "i8" => Some(Ty::int(self.db, Integer::I8, true)),
                | "i16" => Some(Ty::int(self.db, Integer::I16, true)),
                | "i32" => Some(Ty::int(self.db, Integer::I32, true)),
                | "i64" => Some(Ty::int(self.db, Integer::I64, true)),
                | "i128" => Some(Ty::int(self.db, Integer::I128, true)),
                | "isize" => Some(Ty::int(self.db, Integer::ISize, true)),
                | _ => {
                    let found = self
                        .generics
                        .iter()
                        .rev()
                        .enumerate()
                        .find_map(|(i, map)| map.get(other).map(|r| r.at(i as u8)));

                    if let Some(found) = found {
                        Some(Ty::new(self.db, typ::Var(found), Flags::EMPTY))
                    } else {
                        let id = *self.types.get(other)?;
                        let subst = self.parse_substs()?;
                        let subst = if subst.is_empty() { None } else { Some(subst) };

                        Some(Ty::new(self.db, typ::Def(id, subst), Flags::EMPTY))
                    }
                },
            },
            | LAngle => {
                let mut generic = Ty::generic();
                self.generics.push(HashMap::new());

                while self.lexer.peek() != Some(&RAngle) {
                    let kind = match self.lexer.next()? {
                        | Ident("type") => GenericParam::Type,
                        | Ident("figure") => GenericParam::Figure,
                        | Ident("symbol") => GenericParam::Symbol,
                        | _ => return None,
                    };

                    let name = self.ident()?;
                    let id = generic.add_param(kind);

                    self.generics.last_mut().unwrap().insert(name, id);
                }

                self.expect(RAngle)?;

                let inner = self.parse_type()?;

                Some(generic.finish(self.db, inner))
            },
            | LParen => {
                let mut ty_flags = vec![];
                let mut tys = vec![];

                while self.lexer.peek() != Some(&RParen) {
                    ty_flags.push(if self.eat(Flag("in")) { Flags::IN } else { Flags::EMPTY });
                    tys.push(self.parse_type()?);

                    if self.lexer.peek() != Some(&RParen) {
                        self.expect(Comma)?;
                    }
                }

                self.expect(RParen)?;

                if self.eat(Arrow) {
                    let mut rets = Vec::new();

                    if self.eat(LParen) {
                        while self.lexer.peek() != Some(&RParen) {
                            let flags = if self.eat(Flag("out")) { Flags::OUT } else { Flags::EMPTY };

                            rets.push(SigParam { flags, ty: self.parse_type()? });

                            if self.lexer.peek() != Some(&RParen) {
                                self.expect(Comma)?;
                            }
                        }

                        self.expect(RParen)?;
                    } else {
                        let flags = if self.eat(Flag("out")) { Flags::OUT } else { Flags::EMPTY };

                        rets.push(SigParam { flags, ty: self.parse_type()? });
                    }

                    let sig = Signature {
                        params: tys.into_iter().zip(ty_flags).map(|(ty, flags)| SigParam { flags, ty }).collect(),
                        rets,
                    };

                    Some(Ty::new(self.db, typ::Func(sig), Flags::TRIVIAL))
                } else {
                    Some(Ty::tuple(self.db, tys))
                }
            },
            | _ => None,
        };

        if owned {
            ty = ty.map(|t| t.owned(self.db));
        }

        if non_null {
            ty = ty.map(|t| t.flag(self.db, Flags::NON_NULL));
        }

        ty
    }

    fn parse_substs(&mut self) -> Option<Vec<Subst>> {
        let mut subst = Vec::new();

        if self.eat(LAngle) {
            subst.push(self.parse_subst()?);

            while self.eat(Comma) {
                subst.push(self.parse_subst()?);
            }

            self.expect(RAngle)?;
        }

        Some(subst)
    }

    fn parse_subst(&mut self) -> Option<Subst> {
        match self.lexer.next()? {
            | Dollar => Some(Subst::Type(self.parse_type()?)),
            | Int(i) => Some(Subst::Figure(i as u128)),
            | String(s) => Some(Subst::Symbol(s.to_string())),
            | _ => None,
        }
    }

    fn string(&mut self) -> Option<Cow<'a, str>> {
        match self.lexer.next()? {
            | String(s) => Some(s),
            | _ => None,
        }
    }

    fn ident(&mut self) -> Option<&'a str> {
        match self.lexer.next()? {
            | Ident(s) => Some(s),
            | _ => None,
        }
    }

    fn def_name(&mut self) -> Option<&'a str> {
        match self.lexer.next()? {
            | Ident(s) => Some(s),
            | Path(p) => Some(p),
            | _ => None,
        }
    }

    fn int(&mut self) -> Option<i128> {
        match self.lexer.next()? {
            | Int(i) => Some(i),
            | _ => None,
        }
    }

    fn eat(&mut self, tok: Token) -> bool {
        if let Some(cur) = self.lexer.peek() {
            if *cur == tok {
                self.lexer.next().unwrap();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn expect(&mut self, tok: Token) -> Option<Token> {
        let cur = self.lexer.next()?;

        if cur == tok {
            Some(cur)
        } else {
            None
        }
    }
}

impl<'a, 'b> BodyParser<'a, 'b> {
    fn parse(mut self, id: BodyId) -> Option<()> {
        if let Some(&Ident(name) | &Path(name)) = self.lexer.peek() {
            let func = *self.funcs.get(name)?;
            let _ = self.lexer.next()?;

            self.parser.module.define_func(func, id);
        }

        if self.eat(LAngle) {
            self.generics.push(HashMap::new());

            while self.lexer.peek() != Some(&RAngle) {
                let kind = match self.lexer.next()? {
                    | Ident("type") => GenericParam::Type,
                    | Ident("figure") => GenericParam::Figure,
                    | Ident("symbol") => GenericParam::Symbol,
                    | _ => return None,
                };

                let name = self.ident()?;
                let id = self.builder.add_generic_param(kind);

                self.generics.last_mut().unwrap().insert(name, id);
            }

            self.expect(RAngle)?;
        }

        self.expect(LBrace)?;

        while self.lexer.peek() != Some(&RBrace) {
            if let None = self.parse_block() {
                break;
            }
        }

        let _ = self.expect(RBrace)?;

        for (name, BlockRef { block, case }) in self.block_refs {
            let block_data = &mut self.builder.body_mut()[block];

            match &mut block_data.term {
                | Some(Term::Br { to }) => {
                    to.block = self.blocks[name];
                },
                | Some(Term::Switch { cases, default, .. }) => {
                    if case == cases.len() {
                        default.block = self.blocks[name];
                    } else {
                        cases[case].to.block = self.blocks[name];
                    }
                },
                | _ => {},
            }
        }

        Some(())
    }

    fn parse_block(&mut self) -> Option<()> {
        let name = self.ident()?;
        let id = self.builder.create_block();

        if self.eat(LParen) {
            while self.lexer.peek() != Some(&RParen) {
                let var = self.ident()?;
                let _ = self.expect(Colon)?;
                let _ = self.expect(Dollar)?;
                let ty = self.parse_type()?;
                let param = self.builder.add_param(id, ty);

                self.vars.insert(var, param);

                if self.lexer.peek() != Some(&RParen) {
                    self.expect(Comma)?;
                }
            }

            self.expect(RParen)?;
        }

        self.expect(Colon)?;
        self.blocks.insert(name, id);
        self.builder.set_block(id);

        while self.lexer.peek() != Some(&RBrace) {
            match self.parse_term() {
                | Some(true) | None => break,
                | Some(false) => {},
            }
        }

        Some(())
    }

    fn parse_term(&mut self) -> Option<bool> {
        match self.lexer.peek()? {
            | Ident("unreachable") => {
                self.lexer.next()?;
                self.builder.unreachable();
                Some(true)
            },
            | Ident("return") => {
                let _ = self.lexer.next()?;
                let mut vals = Vec::new();

                if let Some(&Ident(var)) = self.lexer.peek() {
                    self.lexer.next()?;
                    vals.push(self.vars[var]);

                    while self.eat(Comma) {
                        let var = self.ident()?;
                        vals.push(self.vars[var]);
                    }
                }

                self.builder.return_(vals);

                Some(true)
            },
            | Ident("br") => {
                let _ = self.lexer.next()?;
                let to = self.parse_target(0)?;

                self.builder.br(to.block, to.args);

                Some(true)
            },
            | Ident("switch") => {
                let _ = self.lexer.next()?;
                let pred = self.ident()?;
                let pred = self.vars[pred];
                let _ = self.expect(Comma)?;
                let mut switch = self.builder.switch(pred);
                let mut cases = 0;

                while let Some(&Token::Int(val)) = self.lexer.peek() {
                    let _ = self.lexer.next()?;
                    let _ = self.expect(Colon)?;
                    let to = self.parse_target(cases)?;
                    let _ = self.expect(Comma)?;

                    switch.case(val as u128, to.block, to.args);
                    cases += 1;
                }

                let _ = self.expect(Ident("default"))?;
                let def = self.parse_target(cases)?;

                switch.build(&mut self.builder, def.block, def.args);

                Some(true)
            },
            | _ => self.parse_instr().map(|_| false),
        }
    }

    fn parse_target(&mut self, case: usize) -> Option<BrTarget> {
        let block = self.ident()?;
        let mut args = Vec::new();

        if self.eat(LParen) {
            let var = self.ident()?;
            args.push(self.vars[var]);

            while self.eat(Comma) {
                let var = self.ident()?;
                args.push(self.vars[var]);
            }

            self.expect(RParen)?;
        }

        let cur_block = self.builder.current_block();

        self.block_refs.push((block, BlockRef { block: cur_block, case }));

        Some(BrTarget { block: Block::ENTRY, args })
    }

    fn peek_var(&mut self) -> Option<&'a str> {
        const NO_RET_INSTRS: &[&'static str] = &[
            "stack_free",
            "box_free",
            "store",
            "copy_addr",
            "drop_addr",
            "drop_value",
            "tuple_insert",
            "struct_insert",
            "apply",
            "intrinsic",
        ];

        if let Some(&Ident(var)) = self.lexer.peek() {
            if !NO_RET_INSTRS.contains(&var) {
                Some(var)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn parse_instr(&mut self) -> Option<()> {
        let mut vars = Vec::new();

        if let Some(var) = self.peek_var() {
            self.lexer.next()?;
            vars.push(var);

            while self.eat(Comma) {
                let var = self.ident()?;
                vars.push(var);
            }

            self.expect(Equals)?;
        }

        let rets = match self.ident()? {
            | "stack_alloc" => {
                let _ = self.expect(Dollar)?;
                let ty = self.parse_type()?;
                let ret = self.builder.stack_alloc(ty);

                vec![ret]
            },
            | "stack_free" => {
                let var = self.ident()?;
                let var = self.vars[var];

                self.builder.stack_free(var);
                Vec::new()
            },
            | "box_alloc" => {
                let kind = match self.ident()? {
                    | "gen" => BoxKind::Gen,
                    | "rc" => BoxKind::Rc,
                    | _ => return None,
                };

                let _ = self.expect(Dollar)?;
                let ty = self.parse_type()?;
                let ret = self.builder.box_alloc(kind, ty);

                vec![ret]
            },
            | "box_free" => {
                let var = self.ident()?;
                let var = self.vars[var];

                self.builder.box_free(var);
                Vec::new()
            },
            | "box_addr" => {
                let var = self.ident()?;
                let var = self.vars[var];
                let ret = self.builder.box_addr(var);

                vec![ret]
            },
            | "load" => {
                let var = self.ident()?;
                let var = self.vars[var];
                let ret = self.builder.load(var);

                vec![ret]
            },
            | "store" => {
                let val = self.ident()?;
                let val = self.vars[val];
                let _ = self.expect(Comma)?;
                let ptr = self.ident()?;
                let ptr = self.vars[ptr];

                self.builder.store(val, ptr);
                Vec::new()
            },
            | "copy_addr" => {
                let mut flags = Flags::EMPTY;
                let old = self.ident()?;
                let old = self.vars[old];

                if self.eat(Flag("take")) {
                    flags = flags.set(Flags::TAKE);
                }

                let _ = self.expect(Comma)?;
                let new = self.ident()?;
                let new = self.vars[new];

                if self.eat(Flag("init")) {
                    flags = flags.set(Flags::INIT);
                }

                self.builder.copy_addr(old, new, flags);
                Vec::new()
            },
            | "copy_value" => {
                let val = self.ident()?;
                let val = self.vars[val];
                let ret = self.builder.copy_value(val);

                vec![ret]
            },
            | "drop_addr" => {
                let addr = self.ident()?;
                let addr = self.vars[addr];

                self.builder.drop_addr(addr);
                Vec::new()
            },
            | "drop_value" => {
                let val = self.ident()?;
                let val = self.vars[val];

                self.builder.drop_value(val);
                Vec::new()
            },
            | "const_int" => {
                let val = self.int()? as u128;
                let _ = self.expect(Comma)?;
                let _ = self.expect(Dollar)?;
                let ty = self.parse_type()?;
                let ret = self.builder.const_int(val, ty);

                vec![ret]
            },
            | "const_str" => {
                let val = self.string()?;
                let ret = self.builder.const_str(val);

                vec![ret]
            },
            | "func_ref" => {
                let name = self.def_name()?;
                let func = self.funcs[name];
                let ret = self.builder.func_ref(func);

                vec![ret]
            },
            | "tuple" => {
                let mut vals = Vec::new();
                let _ = self.expect(LParen)?;

                if self.lexer.peek() != Some(&RParen) {
                    let var = self.ident()?;

                    vals.push(self.vars[var]);

                    if self.lexer.peek() != Some(&RParen) {
                        self.expect(Comma)?;
                    }
                }

                self.expect(RParen)?;

                let ret = self.builder.tuple(vals);

                vec![ret]
            },
            | "tuple_extract" => {
                let tuple = self.ident()?;
                let tuple = self.vars[tuple];
                let _ = self.expect(Comma)?;
                let field = self.int()? as usize;
                let ret = self.builder.tuple_extract(tuple, field);

                vec![ret]
            },
            | "tuple_insert" => {
                let tuple = self.ident()?;
                let tuple = self.vars[tuple];
                let _ = self.expect(Comma)?;
                let field = self.int()? as usize;
                let _ = self.expect(Comma)?;
                let val = self.ident()?;
                let val = self.vars[val];

                self.builder.tuple_insert(tuple, field, val);
                Vec::new()
            },
            | "tuple_addr" => {
                let tuple = self.ident()?;
                let tuple = self.vars[tuple];
                let _ = self.expect(Comma)?;
                let field = self.int()? as usize;
                let ret = self.builder.tuple_addr(tuple, field);

                vec![ret]
            },
            | "struct" => {
                let _ = self.expect(Dollar)?;
                let ty = self.parse_type()?;
                let mut vals = Vec::new();
                let _ = self.expect(LParen)?;

                while self.lexer.peek() != Some(&RParen) {
                    let lbl = self.ident()?;
                    let _ = self.expect(Colon)?;
                    let var = self.ident()?;

                    vals.push((lbl.to_string(), self.vars[var]));

                    if self.lexer.peek() != Some(&RParen) {
                        self.expect(Comma)?;
                    }
                }

                self.expect(RParen)?;

                let ret = self.builder.struct_(ty, vals);

                vec![ret]
            },
            | "struct_extract" => {
                let struc = self.ident()?;
                let _ = self.expect(Comma)?;
                let field = self.ident()?;
                let ret = self.builder.struct_extract(self.vars[struc], field);

                vec![ret]
            },
            | "struct_insert" => {
                let struc = self.ident()?;
                let _ = self.expect(Comma)?;
                let field = self.ident()?;
                let _ = self.expect(Comma)?;
                let val = self.ident()?;

                self.builder.struct_insert(self.vars[struc], field, self.vars[val]);

                Vec::new()
            },
            | "struct_addr" => {
                let struc = self.ident()?;
                let _ = self.expect(Comma)?;
                let field = self.ident()?;
                let ret = self.builder.struct_addr(self.vars[struc], field);

                vec![ret]
            },
            | "apply" => {
                let func = self.ident()?;
                let func = self.vars[func];
                let subst = self.parse_substs()?;
                let _ = self.expect(LParen)?;
                let mut args = Vec::new();

                while self.lexer.peek() != Some(&RParen) {
                    let arg = self.ident()?;
                    let arg = self.vars[arg];

                    args.push(arg);

                    if self.lexer.peek() != Some(&RParen) {
                        self.expect(Comma)?;
                    }
                }

                self.expect(RParen)?;
                self.builder.apply(func, subst, args)
            },
            | "intrinsic" => {
                let name = self.string()?;
                let subst = self.parse_substs()?;
                let _ = self.expect(LParen)?;
                let mut args = Vec::new();

                while self.lexer.peek() != Some(&RParen) {
                    let arg = self.ident()?;
                    let arg = self.vars[arg];

                    args.push(arg);

                    if self.lexer.peek() != Some(&RParen) {
                        self.expect(Comma)?;
                    }
                }

                self.expect(RParen)?;
                self.builder.intrinsic(name, subst, args)
            },
            | _ => return None,
        };

        for (var, ret) in vars.into_iter().zip(rets) {
            self.vars.insert(var, ret);
        }

        Some(())
    }
}

impl<'a, 'b> std::ops::Deref for BodyParser<'a, 'b> {
    type Target = Parser<'a>;

    fn deref(&self) -> &Self::Target {
        self.parser
    }
}

impl<'a, 'b> std::ops::DerefMut for BodyParser<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.parser
    }
}
