use crate::lexer::{lex, Token};
use crate::*;
use std::collections::HashMap;

pub fn parse(src: &str) -> Result<Module, String> {
    let tokens = lex(src);
    let mut decls = IndexVec::new();
    let mut impls = IndexVec::new();
    let mut bodies = IndexVec::new();
    let mut i = 0;

    while i < tokens.len() {
        if peek_decl(&tokens, i) {
            i = parse_decl(&tokens, i, &mut decls)?;
        } else if peek_impl(&tokens, i) {
            i = parse_impl(&tokens, i, &decls, &mut impls)?;
        } else if peek_body(&tokens, i) {
            i = parse_body(&tokens, i, &decls, &mut bodies)?;
        } else {
            return Err(format!(
                "Expected end of input, found {:?} at {}",
                tokens[i], i
            ));
        }
    }

    Ok(Module {
        decls,
        impls,
        bodies,
    })
}

macro_rules! expect {
    ($tokens:ident, $i:expr, $p:pat $(if $guard:expr)?) => {
        match &$tokens[$i] {
            $p $(if $guard)? => $i + 1,
            tok => return Err(format!("Unexpected token {:?} at {}", tok, $i)),
        }
    }
}

macro_rules! peek {
    ($tokens:ident, $i:expr, $p:pat $(if $guard:expr)?) => {
        match &$tokens[$i] {
            $p $(if $guard)? => true,
            _ => false,
        }
    }
}

fn peek_decl(tokens: &[Token], i: usize) -> bool {
    match &tokens[i] {
        Token::Identifier(id) => match id.as_str() {
            "export" | "import" | "local" | "hidden" => true,
            _ => false,
        },
        _ => false,
    }
}

fn parse_decl(
    tokens: &[Token],
    i: usize,
    decls: &mut IndexVec<DeclId, Decl>,
) -> Result<usize, String> {
    let (linkage, i) = parse_linkage(tokens, i)?;
    let (name, i) = parse_declid(tokens, i)?;
    let i = expect!(tokens, i, Token::DblColon);
    let (ty, i) = parse_type(tokens, i)?;
    let id = decls.next_idx();

    decls.insert(
        id,
        Decl {
            id,
            linkage,
            name,
            ty,
        },
    );

    Ok(i)
}

fn parse_linkage(tokens: &[Token], i: usize) -> Result<(Linkage, usize), String> {
    match &tokens[i] {
        Token::Identifier(id) => match id.as_str() {
            "export" => Ok((Linkage::Export, i + 1)),
            "import" => Ok((Linkage::Import, i + 1)),
            "hidden" => Ok((Linkage::Hidden, i + 1)),
            "local" => Ok((Linkage::Local, i + 1)),
            _ => Err(format!("Invalid linkage {} at {}", id, i)),
        },
        _ => Err(format!("Expected a valid linkage at {}", i)),
    }
}

fn peek_impl(tokens: &[Token], i: usize) -> bool {
    match &tokens[i] {
        Token::Identifier(id) => match id.as_str() {
            "impl" => true,
            _ => false,
        },
        _ => false,
    }
}

fn parse_impl(
    tokens: &[Token],
    i: usize,
    decls: &IndexVec<DeclId, Decl>,
    impls: &mut IndexVec<ImplId, Impl>,
) -> Result<usize, String> {
    let (name, i) = parse_ident(tokens, i + 1)?;
    let mut i = expect!(tokens, i, Token::LBrace);
    let mut entries = Vec::new();

    while i < tokens.len() && !matches!(tokens[i], Token::RBrace) {
        let (entry, next_i) = parse_impl_entry(tokens, i, decls, impls)?;

        entries.push(entry);
        i = next_i;
    }

    let i = expect!(tokens, i, Token::RBrace);
    let id = impls.next_idx();

    impls.insert(id, Impl { id, name, entries });

    Ok(i)
}

fn parse_impl_entry(
    tokens: &[Token],
    i: usize,
    decls: &IndexVec<DeclId, Decl>,
    impls: &IndexVec<ImplId, Impl>,
) -> Result<(ImplEntry, usize), String> {
    match &tokens[i] {
        Token::Identifier(id) => match id.as_str() {
            "base" => {
                let (name, i) = parse_ident(tokens, i + 1)?;
                let id = if let Some(impl_) = impls.iter().find(|i| i.name == name) {
                    impl_.id
                } else {
                    return Err(format!("Unknown impl block {} at {}", name, i));
                };

                Ok((ImplEntry::Base(id), i))
            }
            "fn" => {
                let (name, i) = parse_ident(tokens, i + 1)?;
                let i = expect!(tokens, i, Token::DblColon);
                let (decl, i) = parse_declid(tokens, i)?;
                let decl = if let Some(decl) = decls.iter().find(|d| d.name == decl) {
                    decl.id
                } else {
                    return Err(format!("Unknown decl '{}' at {}", name, i - 1));
                };

                Ok((ImplEntry::Func(name, decl), i))
            }
            _ => Err(format!("Expected a valid impl entry at {}", i)),
        },
        _ => Err(format!("Expected a valid impl entry at {}", i)),
    }
}

fn peek_body(tokens: &[Token], i: usize) -> bool {
    match &tokens[i] {
        Token::Identifier(id) => match id.as_str() {
            "fn" => true,
            _ => false,
        },
        _ => false,
    }
}

fn parse_body(
    tokens: &[Token],
    i: usize,
    decls: &IndexVec<DeclId, Decl>,
    bodies: &mut IndexVec<BodyId, Body>,
) -> Result<usize, String> {
    let (name, i) = parse_declid(tokens, i + 1)?;
    let decl = if let Some(decl) = decls.iter().find(|d| d.name == name) {
        decl.id
    } else {
        return Err(format!("Unknown decl '{}' at {}", name, i - 1));
    };

    let mut i = expect!(tokens, i, Token::LBrace);
    let mut locals = IndexVec::new();
    let mut local_ids = HashMap::new();

    while i < tokens.len() && peek_local(tokens, i) {
        i = parse_local(tokens, i, &mut locals, &mut local_ids)?;
    }

    let mut blocks = IndexVec::new();

    while i < tokens.len() && peek_block(tokens, i) {
        i = parse_block(tokens, i, decls, &local_ids, &mut blocks)?;
    }

    let i = expect!(tokens, i, Token::RBrace);
    let id = bodies.next_idx();

    bodies.insert(
        id,
        Body {
            id,
            decl,
            locals,
            blocks,
        },
    );

    Ok(i)
}

fn peek_local(tokens: &[Token], i: usize) -> bool {
    match &tokens[i] {
        Token::Identifier(id) => match id.as_str() {
            "ret" | "arg" | "var" | "tmp" => true,
            _ => false,
        },
        _ => false,
    }
}

fn parse_local(
    tokens: &[Token],
    i: usize,
    locals: &mut IndexVec<Local, LocalData>,
    local_ids: &mut HashMap<String, Local>,
) -> Result<usize, String> {
    let (kind, i) = parse_local_kind(tokens, i)?;
    let (name, i) = parse_local_name(tokens, i)?;
    let i = expect!(tokens, i, Token::DblColon);
    let (ty, i) = parse_type(tokens, i)?;
    let id = locals.next_idx();

    local_ids.insert(name, id);
    locals.insert(id, LocalData { id, kind, ty });

    Ok(i)
}

fn parse_local_kind(tokens: &[Token], i: usize) -> Result<(LocalKind, usize), String> {
    match &tokens[i] {
        Token::Identifier(id) => match id.as_str() {
            "ret" => Ok((LocalKind::Ret, i + 1)),
            "arg" => Ok((LocalKind::Arg, i + 1)),
            "var" => Ok((LocalKind::Var, i + 1)),
            "tmp" => Ok((LocalKind::Tmp, i + 1)),
            _ => Err(format!("Invalid local kind {} at {}", id, i)),
        },
        _ => Err(format!("Expected a valid local kind at {}", i)),
    }
}

fn parse_local_name(tokens: &[Token], i: usize) -> Result<(String, usize), String> {
    match &tokens[i] {
        Token::Local(id) => Ok((id.clone(), i + 1)),
        _ => Err(format!("Expected a local id at {}", i)),
    }
}

fn peek_block(tokens: &[Token], i: usize) -> bool {
    matches!(tokens[i], Token::Block(_))
}

fn parse_block(
    tokens: &[Token],
    i: usize,
    decls: &IndexVec<DeclId, Decl>,
    locals: &HashMap<String, Local>,
    blocks: &mut IndexVec<Block, BlockData>,
) -> Result<usize, String> {
    let (id, i) = parse_block_id(tokens, i)?;
    let mut i = expect!(tokens, i, Token::Colon);
    let mut stmts = Vec::new();
    let mut term = Term::Abort;

    while i < tokens.len() && !peek_block(tokens, i) && !matches!(tokens[i], Token::RBrace) {
        if peek_term(tokens, i) {
            let (t, next_i) = parse_term(tokens, i, decls, locals)?;

            i = next_i;
            term = t;
            break;
        } else {
            let (stmt, next_i) = parse_stmt(tokens, i, decls, locals)?;

            i = next_i;
            stmts.push(stmt);
        }
    }

    blocks.insert(id, BlockData { id, stmts, term });

    Ok(i)
}

fn parse_block_id(tokens: &[Token], i: usize) -> Result<(Block, usize), String> {
    match tokens[i] {
        Token::Block(b) => Ok((Block::new(b), i + 1)),
        _ => Err(format!("Expected a block id at {}", i)),
    }
}

fn peek_term(tokens: &[Token], i: usize) -> bool {
    match &tokens[i] {
        Token::Identifier(id) => match id.as_str() {
            "abort" | "return" | "jump" | "switch" => true,
            _ => false,
        },
        _ => false,
    }
}

fn parse_stmt(
    tokens: &[Token],
    i: usize,
    decls: &IndexVec<DeclId, Decl>,
    locals: &HashMap<String, Local>,
) -> Result<(Stmt, usize), String> {
    if peek!(tokens, i, Token::Identifier(id) if id == "call") {
        let (func, i) = parse_operand(tokens, i + 1, decls, locals)?;
        let mut i = expect!(tokens, i, Token::LParen);
        let mut args = Vec::new();

        while i < tokens.len() && !matches!(tokens[i], Token::RParen) {
            let (op, next_i) = parse_operand(tokens, i, decls, locals)?;

            i = next_i;
            args.push(op);

            if !matches!(tokens[i], Token::RParen) {
                i = expect!(tokens, i, Token::Comma);
            }
        }

        let mut i = expect!(tokens, i, Token::RParen);
        let mut rets = Vec::new();

        if let Token::Arrow = tokens[i] {
            let (p, next_i) = parse_place(tokens, i + 1, decls, locals)?;

            rets.push(p);
            i = next_i;

            while i < tokens.len() && matches!(tokens[i], Token::Comma) {
                let (p, next_i) = parse_place(tokens, i + 1, decls, locals)?;

                rets.push(p);
                i = next_i;
            }
        }

        Ok((Stmt::Call(rets, func, args), i))
    } else {
        let (place, i) = parse_place(tokens, i, decls, locals)?;
        let i = expect!(tokens, i, Token::Equals);
        let (rvalue, i) = parse_rvalue(tokens, i, decls, locals)?;

        Ok((Stmt::Assign(place, rvalue), i))
    }
}

fn parse_term(
    tokens: &[Token],
    i: usize,
    decls: &IndexVec<DeclId, Decl>,
    locals: &HashMap<String, Local>,
) -> Result<(Term, usize), String> {
    match &tokens[i] {
        Token::Identifier(id) => match id.as_str() {
            "abort" => Ok((Term::Abort, i + 1)),
            "return" => Ok((Term::Return, i + 1)),
            "jump" => {
                let (block, i) = parse_block_id(tokens, i + 1)?;

                Ok((Term::Jump(block), i))
            }
            "switch" => {
                let (op, i) = parse_operand(tokens, i + 1, decls, locals)?;
                let mut i = expect!(tokens, i, Token::LBracket);
                let mut vals = Vec::new();
                let mut blocks = Vec::new();

                while i < tokens.len()
                    && !peek!(tokens, i, Token::Identifier(id) if id == "otherwise")
                {
                    let (val, next_i) = parse_scalar(tokens, i)?;
                    let next_i = expect!(tokens, next_i, Token::Comma);
                    let (block, next_i) = parse_block_id(tokens, next_i)?;

                    i = next_i;
                    vals.push(val);
                    blocks.push(block);
                }

                let i = expect!(tokens, i, Token::Identifier(id) if id == "otherwise");
                let (block, i) = parse_block_id(tokens, i)?;

                blocks.push(block);

                Ok((Term::Switch(op, vals, blocks), i))
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
}

fn parse_rvalue(
    tokens: &[Token],
    i: usize,
    decls: &IndexVec<DeclId, Decl>,
    locals: &HashMap<String, Local>,
) -> Result<(RValue, usize), String> {
    if peek!(tokens, i, Token::Identifier(id) if id == "addrof") {
        let (place, i) = parse_place(tokens, i + 1, decls, locals)?;

        Ok((RValue::AddrOf(place), i))
    } else if let Token::Intrinsic(name) = &tokens[i] {
        let mut i = expect!(tokens, i + 1, Token::LParen);
        let mut args = Vec::new();

        while i < tokens.len() && !matches!(tokens[i], Token::RParen) {
            let (arg, next_i) = parse_operand(tokens, i, decls, locals)?;

            args.push(arg);
            i = next_i;

            if !matches!(tokens[i], Token::RParen) {
                i = expect!(tokens, i, Token::Comma);
            }
        }

        let i = expect!(tokens, i, Token::RParen);

        Ok((RValue::Intrinsic(name.clone(), args), i))
    } else {
        let (op, i) = parse_operand(tokens, i, decls, locals)?;

        Ok((RValue::Use(op), i))
    }
}

fn parse_operand(
    tokens: &[Token],
    i: usize,
    decls: &IndexVec<DeclId, Decl>,
    locals: &HashMap<String, Local>,
) -> Result<(Operand, usize), String> {
    if peek_place(tokens, i) {
        let (p, i) = parse_place(tokens, i, decls, locals)?;

        Ok((Operand::Place(p), i))
    } else {
        let (c, i) = parse_const(tokens, i, decls)?;

        Ok((Operand::Const(c), i))
    }
}

fn peek_place(tokens: &[Token], i: usize) -> bool {
    match tokens[i] {
        Token::LParen if i + 1 < tokens.len() => peek_place(tokens, i + 1),
        Token::Star => true,
        Token::Local(_) => true,
        _ => false,
    }
}

fn parse_place(
    tokens: &[Token],
    i: usize,
    decls: &IndexVec<DeclId, Decl>,
    locals: &HashMap<String, Local>,
) -> Result<(Place, usize), String> {
    fn rec(
        tokens: &[Token],
        i: usize,
        elems: &mut Vec<PlaceElem>,
        decls: &IndexVec<DeclId, Decl>,
        locals: &HashMap<String, Local>,
    ) -> Result<(Local, usize), String> {
        let (local, mut i) = if let Token::LParen = tokens[i] {
            let (local, i) = rec(tokens, i + 1, elems, decls, locals)?;
            let i = expect!(tokens, i, Token::RParen);

            (local, i)
        } else if let Token::Star = tokens[i] {
            elems.push(PlaceElem::Deref);
            rec(tokens, i + 1, elems, decls, locals)?
        } else {
            let (name, i) = parse_local_name(tokens, i)?;
            let local = locals[&name];

            (local, i)
        };

        while i < tokens.len() && matches!(tokens[i], Token::LBracket | Token::Dot) {
            if let Token::LBracket = tokens[i] {
                let (op, next_i) = parse_operand(tokens, i + 1, decls, locals)?;

                elems.push(PlaceElem::Index(op));
                i = expect!(tokens, next_i, Token::RBracket);
            } else if let Token::Dot = tokens[i] {
                if let Token::Scalar(s) = tokens[i + 1] {
                    elems.push(PlaceElem::Field(s as usize));
                    i += 2;
                } else {
                    return Err(format!("Expected a number at {}", i));
                }
            }
        }

        Ok((local, i))
    }

    let mut elems = Vec::new();
    let (local, i) = rec(tokens, i, &mut elems, decls, locals)?;

    Ok((Place { local, elems }, i))
}

fn parse_const(
    tokens: &[Token],
    mut i: usize,
    decls: &IndexVec<DeclId, Decl>,
) -> Result<(Const, usize), String> {
    if let Token::LParen = tokens[i] {
        let mut cs = Vec::new();

        i += 1;

        while i < tokens.len() && !matches!(tokens[i], Token::RParen) {
            let (c, next_i) = parse_const(tokens, i, decls)?;

            cs.push(c);
            i = next_i;

            if !matches!(tokens[i], Token::RParen) {
                i = expect!(tokens, i, Token::Comma);
            }
        }

        let i = expect!(tokens, i, Token::RParen);

        Ok((Const::Tuple(cs), i))
    } else if let Token::Scalar(s) = tokens[i] {
        let i = expect!(tokens, i + 1, Token::Colon);
        let (ty, i) = parse_type(tokens, i)?;

        Ok((Const::Scalar(s, ty), i))
    } else if let Token::Decl(name) = &tokens[i] {
        let decl = if let Some(decl) = decls.iter().find(|d| &d.name == name) {
            decl.id
        } else {
            return Err(format!("Unknown decl '{}' at {}", name, i));
        };

        Ok((Const::Addr(decl), i + 1))
    } else {
        Err(format!("Expected a valid constant at {}", i))
    }
}

fn parse_type(tokens: &[Token], i: usize) -> Result<(Type, usize), String> {
    let (ty, mut i) = parse_type_func(tokens, i)?;
    let mut tys = vec![ty];

    while let Token::Pipe = tokens[i] {
        let (next, next_i) = parse_type_func(tokens, i + 1)?;

        tys.push(next);
        i = next_i;
    }

    if tys.len() == 1 {
        Ok((tys.pop().unwrap(), i))
    } else {
        Ok((Type::Union(tys), i))
    }
}

fn parse_type_func(tokens: &[Token], i: usize) -> Result<(Type, usize), String> {
    let (left, i) = parse_type_atom(tokens, i)?;

    if let Token::Arrow = tokens[i] {
        if let Type::Tuple(params) = left {
            let (right, i) = parse_type_atom(tokens, i + 1)?;

            if let Type::Tuple(rets) = right {
                Ok((Type::Func(Signature { params, rets }), i))
            } else {
                Err(format!(
                    "Right hand side of a function arrow must be a list of types at {}",
                    i
                ))
            }
        } else {
            Err(format!(
                "Left hand side of a function arrow must be a list of types at {}",
                i
            ))
        }
    } else {
        Ok((left, i))
    }
}

fn parse_type_atom(tokens: &[Token], i: usize) -> Result<(Type, usize), String> {
    if let Token::Star = tokens[i] {
        let (to, i) = parse_type_atom(tokens, i + 1)?;

        Ok((Type::Ptr(Box::new(to)), i))
    } else if let Token::LParen = tokens[i] {
        let mut tys = Vec::new();
        let mut i = i + 1;

        while !matches!(tokens[i], Token::RParen) {
            let (ty, next_i) = parse_type(tokens, i)?;

            tys.push(ty);
            i = next_i;

            if !matches!(tokens[i], Token::RParen) {
                i = expect!(tokens, i, Token::Comma);
            }
        }

        let i = expect!(tokens, i, Token::RParen);

        Ok((Type::Tuple(tys), i))
    } else if let Token::Identifier(ref id) = tokens[i] {
        match id.as_str() {
            "i8" => Ok((Type::I8, i + 1)),
            "i16" => Ok((Type::I16, i + 1)),
            "i32" => Ok((Type::I32, i + 1)),
            "i64" => Ok((Type::I64, i + 1)),
            "i128" => Ok((Type::I128, i + 1)),
            "f32" => Ok((Type::F32, i + 1)),
            "f64" => Ok((Type::F64, i + 1)),
            "type" => {
                let (name, i) = parse_ident(tokens, i + 1)?;

                Ok((Type::Type(name), i))
            }
            "vwt" => {
                let (name, i) = parse_ident(tokens, i + 1)?;

                Ok((Type::Vwt(name), i))
            }
            _ => Ok((Type::Opaque(id.clone()), i + 1)),
        }
    } else {
        Err(format!("Expected a valid type at {}", i))
    }
}

fn parse_declid(tokens: &[Token], i: usize) -> Result<(String, usize), String> {
    match &tokens[i] {
        Token::Decl(name) => Ok((name.clone(), i + 1)),
        _ => Err(format!("Expected a decl name at {}", i)),
    }
}

fn parse_ident(tokens: &[Token], i: usize) -> Result<(String, usize), String> {
    match &tokens[i] {
        Token::Identifier(name) => Ok((name.clone(), i + 1)),
        _ => Err(format!("Expected a name at {}", i)),
    }
}

fn parse_scalar(tokens: &[Token], i: usize) -> Result<(u128, usize), String> {
    match tokens[i] {
        Token::Scalar(s) => Ok((s, i + 1)),
        _ => Err(format!("Expected a scalar at {}", i)),
    }
}
