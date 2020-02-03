#![feature(decl_macro)]
#![feature(associated_type_defaults)]

pub use lazy_static::lazy_static;
pub use parking_lot;

pub trait Intern {
    type Key: InternKey;

    fn intern(self) -> Self::Key;

    fn untern<'a>(key: Self::Key) -> Interned<'a, Self> where Self: Sized;

    fn untern_mut<'a>(key: Self::Key) -> InternedMut<'a, Self> where Self: Sized;
}

pub trait InternKey {
    fn from_usize(src: usize) -> Self;
    fn into_usize(self) -> usize;
}

pub trait Interner {
    type Value: Intern;
    type Key = <Self::Value as Intern>::Key;

    fn intern(&mut self, value: Self::Value) -> Self::Key;

    fn untern<'a>(&'a self, key: Self::Key) -> &'a Self::Value;

    fn untern_mut<'a>(&'a mut self, key: Self::Key) -> &'a mut Self::Value;
}

pub trait InternerExt: Interner {
    fn set(key: Self::Key, value: Self::Value);

    fn has(key: Self::Key) -> bool;

    fn iter<'a>() -> InternIter<'a, Self> where Self: Sized;

    fn iter_mut<'a>() -> InternIterMut<'a, Self> where Self: Sized;
}

pub type Interned<'a, I> = parking_lot::MappedRwLockReadGuard<'a, I>;
pub type InternedMut<'a, I> = parking_lot::MappedRwLockWriteGuard<'a, I>;

pub struct InternIter<'a, I: Interner>(parking_lot::RwLockReadGuard<'a, I>);
pub struct InternIterMut<'a, I: Interner>(parking_lot::RwLockWriteGuard<'a, I>);

impl<'a, I: Interner> InternIter<'a, I> {
    pub fn new(guard: parking_lot::RwLockReadGuard<'a, I>) -> InternIter<'a, I> {
        InternIter(guard)
    }
}

impl<'a, I: Interner> InternIterMut<'a, I> {
    pub fn new(guard: parking_lot::RwLockWriteGuard<'a, I>) -> InternIterMut<'a, I> {
        InternIterMut(guard)
    }
}

impl<'a, 'b, I: Interner> IntoIterator for &'b InternIter<'a, I>
where
    &'b I: IntoIterator
{
    type Item = <&'b I as IntoIterator>::Item;
    type IntoIter = <&'b I as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<'a, 'b, I: Interner> IntoIterator for &'b InternIterMut<'a, I>
where
    &'b I: IntoIterator
{
    type Item = <&'b I as IntoIterator>::Item;
    type IntoIter = <&'b I as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<'a, 'b, I: Interner> IntoIterator for &'b mut InternIterMut<'a, I>
where
    &'b mut I: IntoIterator
{
    type Item = <&'b mut I as IntoIterator>::Item;
    type IntoIter = <&'b mut I as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&mut self.0).into_iter()
    }
}

pub macro interner($name:ident, $static:ident, $type:ty, $key:ty) {
    pub struct $name {
        storage: std::collections::HashMap<usize, $type>,
    }

    $crate::lazy_static!{
        static ref $static
            : $crate::parking_lot::RwLock<$name>
            = $crate::parking_lot::RwLock::new($name { storage: Default::default() });
    }

    impl<'a> std::iter::IntoIterator for &'a $name {
        type Item = ($key, &'a $type);
        type IntoIter = std::iter::Map<
            std::collections::hash_map::Iter<'a, usize, $type>,
            fn((&'a usize, &'a $type)) -> ($key, &'a $type)
        >;

        fn into_iter(self) -> Self::IntoIter {
            self.storage.iter().map(|(k, v)| (<$key as $crate::InternKey>::from_usize(*k), v))
        }
    }

    impl<'a> std::iter::IntoIterator for &'a mut $name {
        type Item = ($key, &'a mut $type);
        type IntoIter = std::iter::Map<
            std::collections::hash_map::IterMut<'a, usize, $type>,
            fn((&'a usize, &'a mut $type)) -> ($key, &'a mut $type)
        >;

        fn into_iter(self) -> Self::IntoIter {
            self.storage.iter_mut().map(|(k, v)| (<$key as $crate::InternKey>::from_usize(*k), v))
        }
    }

    impl $crate::Intern for $type {
        type Key = $key;

        fn intern(self) -> $key {
            $static.write().intern(self)
        }

        fn untern<'a>(key: $key) -> $crate::Interned<'a, $type> {
            $crate::parking_lot::RwLockReadGuard::map($static.read(), |int| int.untern(key))
        }

        fn untern_mut<'a>(key: $key) -> $crate::InternedMut<'a, $type> {
            $crate::parking_lot::RwLockWriteGuard::map($static.write(), |int| int.untern_mut(key))
        }
    }

    impl $crate::Interner for $name {
        type Value = $type;
        type Key = $key;

        fn intern(&mut self, value: $type) -> $key {
            let key = self.storage.len();

            self.storage.insert(key, value);

            <$key as $crate::InternKey>::from_usize(key)
        }

        fn untern<'a>(&'a self, key: $key) -> &'a $type {
            let key = <$key as $crate::InternKey>::into_usize(key);

            self.storage.get(&key).unwrap()
        }

        fn untern_mut<'a>(&'a mut self, key: $key) -> &'a mut $type {
            let key = <$key as $crate::InternKey>::into_usize(key);

            self.storage.get_mut(&key).unwrap()
        }
    }

    impl $crate::InternerExt for $name {
        fn set(key: $key, value: $type) {
            let key = <$key as $crate::InternKey>::into_usize(key);

            $static.write().storage.insert(key, value);
        }

        fn has(key: $key) -> bool {
            let key = <$key as $crate::InternKey>::into_usize(key);

            $static.read().storage.contains_key(&key)
        }

        fn iter<'a>() -> $crate::InternIter<'a, $name> {
            $crate::InternIter::new($static.read())
        }

        fn iter_mut<'a>() -> $crate::InternIterMut<'a, $name> {
            $crate::InternIterMut::new($static.write())
        }
    }
}
