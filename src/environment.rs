use std::collections::HashMap;
use crate::analyzer::{Sem};
use crate::functions::SpecialFunctions;
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

    pub fn find_function(&self, name : &String, arg_types : Vec<Type>) -> Option<(Function, Vec<Function>)> {
        if self.functions.contains_key(name) {
            let func_vec = self.functions.get(name).unwrap();
            self.find_matching(func_vec, &arg_types)
        } else {
            None
        }
    }

    pub fn find_function_with_target(&self, name : &String, arg_types : &Vec<Type>, fn_value_type : &Type) -> Option<Function> {
        if self.functions.contains_key(name) {
            let func_vec = self.functions.get(name).unwrap();
            for f in func_vec {
                if Environment::args_match_exact(arg_types, &f.sig().arguments) &&
                    fn_value_type.eq(&f.sig().value){
                    return Some(f.clone());
                }
            }
        }
        None
    }

    fn args_match_exact(type_list1 : &Vec<Type>, type_list2 : &Vec<Type>) -> bool {
        if type_list1.len() != type_list2.len() {
            return false;
        }
        for idx in 0..type_list1.len() {
            let t1 = type_list1.get(idx).unwrap();
            let t2 = type_list2.get(idx).unwrap();
            if t1.ne(t2) {
                return false;
            }
        }
        true
    }

    pub fn put_function(&mut self, f : Function) {
        let func_name = &f.name().clone();
        if !self.functions.contains_key(func_name) {
            self.functions.insert(func_name.clone(), vec!());
        }
        let mut func_vec = self.functions.get_mut(func_name).unwrap();
        func_vec.push(f);
    }

    fn find_matching(&self, func_vec : &Vec<Function>, arg_types : &Vec<Type>) -> Option<(Function, Vec<Function>)> {
        for func in func_vec {
            if let Some(coercions) = self.func_matches(func, arg_types, false) {
                return Some((func.clone(), coercions));
            }
        }
        for func in func_vec {
            if let Some(coercions) = self.func_matches(func, arg_types, true) {
                return Some((func.clone(), coercions));
            }
        }
        None
    }

    fn func_matches(&self, func: &Function, arg_types: &Vec<Type>, allow_coercions : bool) -> Option<Vec<Function>> {
        let func_args = func.sig().clone().arguments;
        let number_of_args_provided = arg_types.len();
        let mut provided_types_idx : usize = 0;
        let mut coercion_fns = vec![];

        for func_arg_type in func_args {
            if number_of_args_provided > provided_types_idx {
                match func_arg_type {
                    Type::VarArgs(vararg_type) => {
                        // consume args that match vararg type
                        loop {
                            let provided_arg_type = arg_types.get::<usize>(provided_types_idx).unwrap().clone();
                            if number_of_args_provided > provided_types_idx {
                                if Box::new(provided_arg_type.clone()) == vararg_type {
                                    provided_types_idx = provided_types_idx + 1;
                                    coercion_fns.push(SpecialFunctions::id());
                                } else if let Some(coercion_fn) = self.find_coercion_fn(&vararg_type, &provided_arg_type) {
                                    provided_types_idx = provided_types_idx + 1;
                                    coercion_fns.push(coercion_fn);
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                    }
                    _ => {
                        let provided_arg_type = arg_types.get::<usize>(provided_types_idx).unwrap().clone();
                        if provided_arg_type == func_arg_type {
                            provided_types_idx = provided_types_idx + 1;
                            coercion_fns.push(SpecialFunctions::id());
                        } else if allow_coercions {
                            if let Some(coercion_fn) = self.find_coercion_fn(&func_arg_type, &provided_arg_type) {
                                provided_types_idx = provided_types_idx + 1;
                                coercion_fns.push(coercion_fn);
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        // make sure all provided args have been accounted for in signature
        if number_of_args_provided == provided_types_idx {
            Some(coercion_fns)
        } else {
            None
        }
    }

    fn find_coercion_fn(&self, target_type: &Type, source_type: &Type) -> Option<Function> {
        self.find_function_with_target(
            &String::from("coercion"),
            &vec![source_type.clone()],
            target_type)
    }
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
    use crate::environment::{func_matches, func_matches_no_coercions};
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
        assert!(func_matches(&my_fn, &vec!(Type::String, Type::String, Type::String, Type::String), false));
        assert!(func_matches(&my_fn, &vec!(Type::String, Type::String, Type::String), false));
        assert!(func_matches(&my_fn, &vec!(Type::String, Type::String), false));
        assert!(func_matches(&my_fn, &vec!(Type::String), false));
        assert!(!func_matches(&my_fn, &vec!(Type::String, Type::Integral, Type::String), false));
        assert!(!func_matches(&my_fn, &vec!(Type::Integral, Type::String, Type::String), false));
        assert!(!func_matches(&my_fn, &vec!(Type::String, Type::String, Type::Integral), false));


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
        assert!(func_matches(&my_fn2, &vec!(Type::String, Type::String, Type::String, Type::String), false));
        assert!(func_matches(&my_fn2, &vec!(Type::String, Type::String, Type::String, Type::String, Type::Integral), false));
        assert!(func_matches(&my_fn2, &vec!(Type::String, Type::Integral, Type::Integral), false));
    }
}