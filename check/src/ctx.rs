use syntax::{LocalId, Type};
use diagnostics::{Diagnostic, Severity};
use std::collections::BTreeMap;

pub trait Ctx<F, T> {
    fn add_fn(&mut self, name: String, f: F);
    
    fn add_var(&mut self, id: LocalId, v: T);
    
    fn var_alive(&mut self, id: LocalId);
    
    fn var_dead(&mut self, id: LocalId);
    
    fn get_var(&self, id: &LocalId) -> Option<&Var<T>>;
    
    fn get_fn(&self, name: &str) -> Option<&F>;
    
    fn clear_vars(&mut self);
}

pub enum Var<T> {
    Alive(T),
    Dead(T),
}

pub struct TypeCtx<'a> {
    pub reporter: &'a diagnostics::Reporter,
    fns: BTreeMap<String, FnType>,
    vars: BTreeMap<LocalId, Var<Type>>,
}

pub type FnType = (Vec<Type>, Type);

impl<'a> TypeCtx<'a> {
    pub fn new(reporter: &'a diagnostics::Reporter) -> TypeCtx<'a> {
        TypeCtx {
            reporter,
            fns: BTreeMap::new(),
            vars: BTreeMap::new(),
        }
    }
}

impl<'a> Ctx<FnType, Type> for TypeCtx<'a> {
    fn add_fn(&mut self, name: String, f: FnType) {
        self.fns.insert(name, f);
    }
    
    fn add_var(&mut self, id: LocalId, v: Type) {
        self.vars.insert(id, Var::Dead(v));
    }
    
    fn var_alive(&mut self, id: LocalId) {
        if let Some(var) = self.vars.get_mut(&id) {
            match var {
                Var::Dead(v) => *var = Var::Alive(v.clone()),
                Var::Alive(_) => ()
            }
        } else {
            self.reporter.add(Diagnostic::new(Severity::Bug, Default::default(), format!("Undefined variable '{}'", id)));
        }
    }
    
    fn var_dead(&mut self, id: LocalId) {
        if let Some(var) = self.vars.get_mut(&id) {
            match var {
                Var::Alive(v) => *var = Var::Dead(v.clone()),
                Var::Dead(_) => ()
            }
        } else {
            self.reporter.add(Diagnostic::new(Severity::Bug, Default::default(), format!("Undefined variable '{}'", id)));
        }
    }
    
    fn get_fn(&self, name: &str) -> Option<&FnType> {
        self.fns.get(name)
    }
    
    fn get_var(&self, id: &LocalId) -> Option<&Var<Type>> {
        self.vars.get(id)
    }
    
    fn clear_vars(&mut self) {
        self.vars.clear();
    }
}