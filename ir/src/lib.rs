use arena::{Arena, Idx};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body {
    pub vars: Arena<VarInfo>,
    pub blocks: Arena<Block>,
}

pub type Var = Idx<VarInfo>;
pub type BlockId = Idx<Block>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarInfo {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub instrs: Vec<Instr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    BoxAlloc {},
    BoxFree {},
}
