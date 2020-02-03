use std::path::PathBuf;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(usize);

pub struct FileInfo {
    pub name: PathBuf,
    pub source: String,
}

impl intern::InternKey for FileId {
    fn from_usize(src: usize) -> FileId {
        FileId(src)
    }

    fn into_usize(self) -> usize {
        self.0
    }
}

intern::interner!(FileInterner, FILE_INTERNER, FileInfo, FileId);
