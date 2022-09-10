use std::collections::HashMap;
use std::fmt::{Debug, Formatter, Write};
use crate::analyzer::{Sem};
use crate::external_resources::IOResource;
use crate::function_resolver::FunctionResolver;
use crate::functions::SpecialFunctions;
use crate::types::{Function, Type};
use crate::value_store::ValueStore;
use crate::values::{Value};

pub struct Environment {
    value_store : Box<dyn ValueStore>,
    functions : HashMap<String, Vec<Function>>,
}

impl Debug for Environment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Environment")
    }
}

impl Environment {
    pub fn new(mut value_store : Box<dyn ValueStore>) -> Environment {
        Environment {
            value_store,
            functions : HashMap::new(),
        }
    }

    pub fn get_funcs(&self, name : &str) -> Option<&Vec<Function>> {
        self.functions.get(name)
    }

    pub fn resolve_value(&self, name: &str) -> Option<Sem> {
        match self.value_store.resolve(name) {
            None => { None }
            Some((t, v)) => {
                match Value::from_string(t, v) {
                    Ok(v) => {
                        Some(Sem::from_value(v))
                    }
                    Err(_) => {
                        None
                    }
                }
            }
        }
    }

    pub fn store_value(&mut self, name: &str, val : Value) {
        self.value_store.store(name, val.get_type().to_string(), val.to_string());
    }

    pub fn find_function(&self, name : &String, args : &Vec<Sem>) -> Option<Sem> {
        FunctionResolver::resolve(self, name, args)
    }

    pub fn find_function_with_resource(&self, name : &String, args : &Vec<Sem>, res : IOResource) -> Option<Sem> {
        FunctionResolver::resolve_with_resource(self, name, args, res)
    }

    pub fn put_function(&mut self, f : Function) {
        let func_name = &f.name().clone();
        if !self.functions.contains_key(func_name) {
            self.functions.insert(func_name.clone(), vec!());
        }
        let mut func_vec = self.functions.get_mut(func_name).unwrap();
        func_vec.push(f);
    }

    pub fn put_functions(&mut self, fs : Vec<Function>) {
        for f in fs {
            self.put_function(f)
        }
    }
}
