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
                        value_to_sem(v)
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
    let number_of_args_provided = arg_types.len();
    let mut provided_types_idx : usize = 0;

    for func_arg_type in func_args {
        if number_of_args_provided > provided_types_idx {
            match func_arg_type {
                Type::VarArgs(vararg_type) => {
                    // consume args that match vararg type
                    while number_of_args_provided > provided_types_idx &&
                        Box::new(arg_types.get::<usize>(provided_types_idx).unwrap().clone()) == vararg_type {
                        provided_types_idx = provided_types_idx + 1;
                    }
                }
                _ => {
                    if arg_types.get::<usize>(provided_types_idx).unwrap().clone() == func_arg_type {
                        provided_types_idx = provided_types_idx + 1;
                    } else {
                        break;
                    }
                }
            }
        }
    }

    // make sure all provided args have been accounted for in signature
    number_of_args_provided == provided_types_idx
}


fn value_to_sem(val : Value) -> Option<Sem> {
    match val {
        Value::ValueString { .. } => {
            Some(Sem::ValueString(val))
        }
        Value::ValueIntegral { .. } => {
            Some(Sem::ValueIntegral(val))
        }
        Value::ValueFractional { .. } => {
            Some(Sem::ValueFractional(val))
        }
        Value::ValueBoolean { .. } => {
            Some(Sem::ValueBoolean(val))
        }
        Value::ValueTime { .. } => {
            Some(Sem::ValueTime(val))
        }
        Value::ValueUrl { .. } => {
            Some(Sem::ValueUrl(val))
        }
        Value::ValueList { item_type, vals } => {
            let mut sem_vec = vec!();
            for v in vals {
                sem_vec.push(value_to_sem(v)?);
            }
            Some(Sem::ValueList(sem_vec))
        }
        Value::ValueProperty { name, val } => {
            let sem = value_to_sem(*val.clone())?;
            Some(Sem::ValueProperty(name, Box::new(sem)))
        }
        Value::ValuePropertySet { vals } => {
            let mut sems = vec![];
            for val in vals {
                sems.push(value_to_sem(val)?);
            }

            Some(Sem::ValuePropertySet(sems))
        }
    }
}

#[cfg(test)]
mod test {
    use crate::environment::func_matches;
    use crate::types::{Function, Signature, Type};
    use crate::Value;

    #[test]
    fn test_vararg_match() {
        let my_fn = Function::create(
            "vararg_func",
            Signature {
                value: Type::String,
                arguments: vec![Type::String, Type::VarArgs(Box::new(Type::String))],
                resource_type: None
            },
            |args| {
                Box::new(Value::ValueString { val : String::from("")})
            }
        );
        assert!(func_matches(&my_fn, &vec!(Type::String, Type::String, Type::String, Type::String)));
        assert!(func_matches(&my_fn, &vec!(Type::String, Type::String, Type::String)));
        assert!(func_matches(&my_fn, &vec!(Type::String, Type::String)));
        assert!(func_matches(&my_fn, &vec!(Type::String)));
        assert!(!func_matches(&my_fn, &vec!(Type::String, Type::Integral, Type::String)));
        assert!(!func_matches(&my_fn, &vec!(Type::Integral, Type::String, Type::String)));
        assert!(!func_matches(&my_fn, &vec!(Type::String, Type::String, Type::Integral)));


        let my_fn2 = Function::create(
            "vararg_func",
            Signature {
                value: Type::String,
                arguments: vec![Type::String, Type::VarArgs(Box::new(Type::String)), Type::VarArgs(Box::new(Type::Integral))],
                resource_type: None
            },
            |args| {
                Box::new(Value::ValueString { val : String::from("")})
            }
        );
        assert!(func_matches(&my_fn2, &vec!(Type::String, Type::String, Type::String, Type::String)));
        assert!(func_matches(&my_fn2, &vec!(Type::String, Type::String, Type::String, Type::String, Type::Integral)));
        assert!(func_matches(&my_fn2, &vec!(Type::String, Type::Integral, Type::Integral)));
    }
}