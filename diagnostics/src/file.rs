use path_absolutize::Absolutize;
use fluix_encode::{Encodable, Decodable};
use std::collections::BTreeMap;
use std::rc::Rc;
use std::cell::{Ref, RefCell};

#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceFile {
    pub name: String,
    pub dir: String,
    pub text: String,
    pub id: FileId,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Encodable, Decodable)]
pub struct FileId(usize);

pub struct Filemap {
    files: Rc<RefCell<BTreeMap<FileId, SourceFile>>>,
}

impl SourceFile {
    pub fn load(name: impl Into<String>) -> SourceFile {
        use std::fs::File;
        use std::io::Read;
        
        let name = name.into();
        let mut file = File::open(&name).expect(&format!("failed to open file {}", name));
        let mut text = String::new();
        
        file.read_to_string(&mut text).expect(&format!("failed to read file {}", name));
        
        SourceFile {
            text,
            name: name.clone(),
            id: FileId(0),
            dir: std::path::PathBuf::from(name)
                .absolutize().expect("failed to absolutize file path")
                .parent().expect("file path has no parent directory")
                .to_str().unwrap().to_string()
        }
    }
}

impl Filemap {
    pub fn new() -> Filemap {
        Filemap {
            files: Default::default()
        }
    }
    
    pub fn add(&self, file: &mut SourceFile) -> FileId {
        let id = FileId(self.files.borrow().len());
        
        file.id = id;
        self.files.borrow_mut().insert(id, file.clone());
        
        id
    }
    
    pub fn get(&self, i: FileId) -> Ref<SourceFile> {
        Ref::map(self.files.borrow(), |files| &files[&i])
    }
}