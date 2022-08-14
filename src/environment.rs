use std::collections::HashMap;
use crate::analyzer::{Sem};
use crate::types::{Function, Type};
use crate::value_store::ValueStore;
use crate::values::{Value};

pub struct Environment {
    value_store : Box<dyn ValueStore>,
    functions : HashMap<String, Vec<Function>>,
}

impl Environment {
    pub fn new(mut value_store : Box<dyn ValueStore>) -> Environment {
        Environment {
            value_store,
            functions : HashMap::new(),
        }
    }

    pub fn resolve_value(&self, name: &str) -> Option<Sem> {
        match self.value_store.resolve(name) {
            None => { None }
            Some((t, v)) => {
                match Value::from_string(t, v) {
                    Ok(v) => {
                        match v.get_type() {
                            Type::String => {
                                Some(Sem::ValueString(v))
                            }
                            Type::Integral => {
                                Some(Sem::ValueIntegral(v))
                            }
                            Type::Fractional => {
                                Some(Sem::ValueFractional(v))
                            }
                            Type::Boolean => {
                                Some(Sem::ValueBoolean(v))
                            }
                            Type::Time => {
                                Some(Sem::ValueTime(v))
                            }
                            Type::URL => {
                                Some(Sem::ValueUrl(v))
                            }
                            _ => {
                                None
                            }
                        }
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

    pub fn find_function(&self, name : &String, arg_types : Vec<Type>) -> Option<Function> {
        if self.functions.contains_key(name) {
            let func_vec = self.functions.get(name).unwrap();
            find_matching(func_vec, &arg_types)
        } else {
            None
        }
    }

    pub fn put_function(&mut self, f : Function) {
        let func_name = &f.name().clone();
        if !self.functions.contains_key(func_name) {
            self.functions.insert(func_name.clone(), vec!());
        }
        let mut func_vec = self.functions.get_mut(func_name).unwrap();
        func_vec.push(f);
    }

}

fn find_matching(func_vec : &Vec<Function>, arg_types : &Vec<Type>) -> Option<Function> {
    for func in func_vec {
        if func_matches(func, arg_types) {
            return Some(func.clone());
        }
    }
    None
}

fn func_matches(func: &Function, arg_types: &Vec<Type>) -> bool {
    let func_args = func.sig().clone().arguments;

    if func_args.len() != arg_types.len() {
        return false;
    }
    let mut idx = 0;
    for fn_arg in func_args {
        if !fn_arg.eq(arg_types.get(idx).unwrap()) {
            return false;
        }
        idx = idx + 1;
    }

    true
}
