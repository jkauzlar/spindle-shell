use crate::analyzer::{Sem, Typed};
use crate::Environment;
use crate::functions::SpecialFunctions;
use crate::types::{Function, Type};

pub struct FunctionResolver<'a> {
    env : &'a Environment,
    name : String,
    arg_sems : &'a Vec<Sem>,
}

impl FunctionResolver<'_> {
    pub fn resolve(env : &Environment, name : &String, arg_sems : &Vec<Sem>) -> Option<Sem> {
        FunctionResolver {
            env,
            name : name.clone(),
            arg_sems,
        }.find_function()
    }

    fn find_function(&mut self) -> Option<Sem> {
        let funcs = self.env.get_funcs(self.name.as_str())?;

        // try without coercions
        for f in funcs {
            if let Some(sem) = FuncMatcher::try_match(
                self, f, &self.arg_sems, false) {
                return Some(sem);
            }
        }

        // try with coercions
        for f in funcs {
            if let Some(sem) = FuncMatcher::try_match(
                self, f, &self.arg_sems, true) {
                return Some(sem);
            }
        }
        None
    }

    fn find_function_with_value_type(
        &self, name : &String,
        arg_types: &Vec<Type>,
        fn_value_type : &Type) -> Option<Function> {
        if let Some(funcs_vec) = self.env.get_funcs(name) {
            for f in funcs_vec {
                if FunctionResolver::args_match_exact(arg_types, &f.sig().arguments) {
                    if fn_value_type.eq(&f.sig().value) {
                        return Some(f.clone());
                    }
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
}

struct FuncMatcher<'a> {
    func_resolver : &'a FunctionResolver<'a>,
    func : &'a Function,
    func_arg_idx : usize,
    provided_arg_sems: &'a Vec<Sem>,
    provided_arg_types: Vec<Type>,
    provided_arg_idx: usize,
    allow_coercions : bool,
    coercion_fns : Vec<Function>,
}

impl FuncMatcher<'_> {

    fn try_match(func_resolver : &'_ FunctionResolver<'_>, func: &Function, arg_sems: &Vec<Sem>, allow_coercions : bool) -> Option<Sem> {
        let mut arg_types = vec![];
        for s in arg_sems {
            arg_types.push(s.get_type().clone());
        }
        FuncMatcher {
            func_resolver,
            func,
            func_arg_idx : 0,
            provided_arg_sems: arg_sems,
            provided_arg_types: arg_types,
            provided_arg_idx: 0,
            allow_coercions,
            coercion_fns: vec![],
        }.run()
    }

    fn run(&mut self) -> Option<Sem> {
        while self.func_arg_idx < self.func.sig().arguments.len() {
            match self.func.sig().arguments.get(self.func_arg_idx) {
                None => {
                    panic!("just checked that the index is valid. Shouldn't get here!")
                }
                Some(func_arg_type) => {
                    if self.match_func_arg(func_arg_type) {
                        self.consume_func_type();
                    } else {
                        return None;
                    }
                }
            }
        }

        if self.provided_arg_idx < self.provided_arg_types.len() {
            None
        } else {
            Some(Sem::FnCall(
                self.func.clone(),
                self.apply_coercions(),
            ))
        }
    }

    fn apply_coercions(&self) -> Vec<Sem> {
        let mut result_sems = vec![];
        let mut idx = 0;
        for arg_sem in self.provided_arg_sems {
            match self.coercion_fns.get(idx) {
                None => { panic!("number of coercion functions should equal sem_args at this point!")}
                Some(cf) => {
                    if cf.name().eq(SpecialFunctions::id().name()) {
                        result_sems.push(arg_sem.clone())
                    } else {
                        result_sems.push(self.apply_coercion(cf, arg_sem))
                    }
                }
            }

            idx = idx + 1;
        }

        result_sems
    }

    fn apply_coercion(&self, coercion_fn: &Function, sem: &Sem) -> Sem {
        Sem::FnCall(
            coercion_fn.clone(),
            vec![sem.clone()],
        )
    }

    fn match_func_arg(&mut self, func_arg_type: &Type) -> bool {
        match func_arg_type {
            Type::VarArgs(vararg_type) => {
                while self.match_func_arg(vararg_type) { }
                return true;
            }
            _ => {
                if let Some(cfn) = self.check_provided_arg_matches(func_arg_type) {
                    self.coercion_fns.push(cfn);
                    self.consume_provided_arg_type();
                    return true;
                }
            }
        }
        false
    }

    fn consume_func_type(&mut self) {
        self.func_arg_idx = self.func_arg_idx + 1
    }

    fn consume_provided_arg_type(&mut self) {
        self.provided_arg_idx = self.provided_arg_idx + 1
    }

    fn current_provided_arg_type(&self) -> Option<&Type> {
        self.provided_arg_types.get(self.provided_arg_idx)
    }

    /// returns a coercion function if arg_type matches the current provided type.
    /// if !allow_coercions, then return identity function
    fn check_provided_arg_matches(&self, t: &Type) -> Option<Function> {
        let provided_arg = self.current_provided_arg_type()?;
        if provided_arg.eq(t) {
            Some(SpecialFunctions::id())
        } else if self.allow_coercions {
            let cf = self.func_resolver.find_function_with_value_type(
                &SpecialFunctions::coercion_fn_name(),
                &vec![provided_arg.clone()],
                &t.clone())?;
            Some(cf)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use bigdecimal::BigDecimal;
    use num_bigint::BigInt;
    use crate::types::{Function, Signature, Type};
    use crate::{Environment, InMemoryValueStore, Value};
    use crate::analyzer::Sem;
    use crate::function_resolver::{FuncMatcher, FunctionResolver};
    use crate::functions::SpecialFunctions;
    use crate::functions::get_coercions;
    use crate::Value::ValueIntegral;

    #[test]
    fn test_find_funcs_with_value_type() {
        let mut env = Environment::new(Box::new(InMemoryValueStore::create()));
        for f in get_coercions() {
            env.put_function(f)
        }

        let resolver = FunctionResolver {
            env: &env,
            name: "".to_string(),
            arg_sems: &vec![]
        };

        match resolver.find_function_with_value_type(
            &SpecialFunctions::coercion_fn_name(),
            &vec![Type::Integral],
            &Type::Fractional,
        ) {
            None => { assert_eq!(1, 2, "Should find match for coercion function Integral -> Fractional") }
            Some(_) => {
                // success
            }
        }

        match resolver.find_function_with_value_type(
            &SpecialFunctions::coercion_fn_name(),
            &vec![Type::String],
            &Type::URL,
        ) {
            None => {
                // success
            }
            Some(_) => {
                assert_eq!(1, 2, "Should not find a match for coercion function String -> URL")
            }
        }


    }

    #[test]
    fn test_vararg_fns() {
        let mut env = Environment::new(Box::new(InMemoryValueStore::create()));
        for f in get_coercions() {
            env.put_function(f)
        }

        let my_fn = Function::create(
            "my_func",
            Signature {
                value: Type::String,
                arguments: vec![Type::Fractional, Type::VarArgs(Box::new(Type::String))],
                resource_type: None
            },
            |args| {
                Box::new(Value::ValueString { val : String::from("")})
            }
        );
        env.put_function(my_fn.clone());

        let mut func_resolver = FunctionResolver {
            env: &env,
            name: "".to_string(),
            arg_sems: &vec![]
        };


        // should match
        assert!(run_match(&my_fn, &mut func_resolver, &vec![
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1) }),
            Sem::ValueString(Value::ValueString { val: String::from("") }),
        ], false), "Should match without coercion");

        // shouldn't match without arguments
        assert!(!run_match(&my_fn, &mut func_resolver, &vec![
        ], false), "shouldn't match without arguments");

        // should match
        assert!(run_match(&my_fn, &mut func_resolver, &vec![
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1) }),
            Sem::ValueString(Value::ValueString { val: String::from("") }),
            Sem::ValueString(Value::ValueString { val: String::from("") }),
            Sem::ValueString(Value::ValueString { val: String::from("") }),
            Sem::ValueString(Value::ValueString { val: String::from("") }),
        ], false), "Should match without coercion");

        // should match
        assert!(run_match(&my_fn, &mut func_resolver, &vec![
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1) }),
        ], false), "Should match without any vararg matches");

        // shouldn't match without coercion
        assert!(!run_match(&my_fn, &mut func_resolver, &vec![
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1) }),
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1) }),
        ], false), "Shouldn't match without coercion");

        // should match with coercion
        assert!(run_match(&my_fn, &mut func_resolver, &vec![
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1) }),
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1) }),
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1) }),
        ], true), "Should match with coercion");

    }

    #[test]
    fn test_simple_fns() {
        let mut env = Environment::new(Box::new(InMemoryValueStore::create()));
        for f in get_coercions() {
            env.put_function(f)
        }

        let my_fn = Function::create(
            "my_func",
            Signature {
                value: Type::String,
                arguments: vec![Type::Fractional, Type::Fractional],
                resource_type: None
            },
            |args| {
                Box::new(Value::ValueString { val : String::from("")})
            }
        );
        env.put_function(my_fn.clone());

        let mut func_resolver = FunctionResolver {
            env: &env,
            name: "".to_string(),
            arg_sems: &vec![]
        };


        // should match
        assert!(run_match(&my_fn, &mut func_resolver, &vec![
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1) }),
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1) }),
        ], false), "Should match without coercion");

        // should not match
        assert!(!run_match(&my_fn, &mut func_resolver, &vec![
            Sem::ValueIntegral(Value::ValueIntegral { val: BigInt::from(1) }),
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1) }),
        ], false), "Should NOT match without coercion");

        // should not match
        assert!(!run_match(&my_fn, &mut func_resolver, &vec![], false),
                "Should NOT match without arguments");

        // should match with coercion
        assert!(run_match(&my_fn, &mut func_resolver, &vec![
            Sem::ValueIntegral(Value::ValueIntegral { val: BigInt::from(1) }),
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1) }),
        ], true), "Test match with simple coercion");

    }

    fn run_match(my_fn: &Function, func_resolver: &mut FunctionResolver, sems: &Vec<Sem>, allow_coercions: bool) -> bool {
        let result = FuncMatcher::try_match(
            &func_resolver,
            &my_fn,
            sems,
            allow_coercions);

        match result {
            None => { false }
            Some(s) => {
                match s {
                    Sem::FnCall(f, arg_sems) => {
                        true
                    }
                    _ => {
                        false
                    }
                }
            }
        }
    }

}