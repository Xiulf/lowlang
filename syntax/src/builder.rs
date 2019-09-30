use crate::*;

pub struct Builder {
    
}

pub struct FunctionBuilder<'a> {
    function: &'a mut Function,
    block: BlockId,
}

impl<'a> FunctionBuilder<'a> {
    pub fn create_bb(&mut self) -> BlockId {
        let id = BlockId(self.function.blocks.len());
        
        self.function.blocks.push(BasicBlock {
            id,
            statements: Vec::new(),
            terminator: Terminator::Unreachable,
        });
        
        id
    }
    
    pub fn use_bb(&mut self, id: BlockId) {
        self.block = id;
    }
}