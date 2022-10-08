use std::collections::HashMap;
use crate::analyzer::{Sem, Typed};
use crate::environment::Environment;
use crate::external_resources::IOResource;
use crate::functions::SpecialFunctions;
use crate::types::{Function, Type};

#[derive(Debug)]
pub struct FunctionResolver<'a> {
    env : &'a Environment,
    name : String,
    arg_sems : &'a Vec<Sem>,
    res : Option<IOResource>,
}

impl FunctionResolver<'_> {
    pub fn resolve(env : &Environment, name : &String, arg_sems : &Vec<Sem>) -> Option<Sem> {

        FunctionResolver {
            env,
            name : name.clone(),
            arg_sems,
            res : None,
        }.find_function()
    }

    pub fn resolve_with_resource(env : &Environment, name : &String, arg_sems : &Vec<Sem>, res : IOResource) -> Option<Sem> {

        FunctionResolver {
            env,
            name : name.clone(),
            arg_sems,
            res : Some(res),
        }.find_function()
    }


    fn find_function(&mut self) -> Option<Sem> {
        let funcs = self.env.get_funcs(self.name.as_str())?;

        // try without coercions
        for f in funcs {
            if let Some(sem) = FuncMatcher::try_match(self, f, false) {
                return Some(sem);
            }
        }

        // try with coercions
        for f in funcs {
            if let Some(sem) = FuncMatcher::try_match(self, f, true) {
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

#[derive(Debug)]
struct FuncMatcher<'a> {
    func_resolver : &'a FunctionResolver<'a>,
    func : &'a Function,
    func_arg_idx : usize,
    provided_arg_sems: &'a Vec<Sem>,
    provided_arg_types: Vec<Type>,
    provided_arg_idx: usize,
    allow_coercions : bool,
    coercion_fns : Vec<Function>,
    concrete_type_map : HashMap<String, Type>,
}

impl FuncMatcher<'_> {

    fn try_match(func_resolver : &'_ FunctionResolver<'_>, func: &Function, allow_coercions : bool) -> Option<Sem> {
        let mut arg_types = vec![];
        for s in func_resolver.arg_sems {
            arg_types.push(s.get_type().clone());
        }
        FuncMatcher {
            func_resolver,
            func,
            func_arg_idx : 0,
            provided_arg_sems: func_resolver.arg_sems,
            provided_arg_types: arg_types,
            provided_arg_idx: 0,
            allow_coercions,
            coercion_fns: vec![],
            concrete_type_map: HashMap::new(),
        }.run()
    }

    fn run(&mut self) -> Option<Sem> {
        // only match the function when the function requires a resource if the provided resource's type matches
        if let Some(func_res) = &self.func.sig().resource_type {
            if let Some(provided_res) = &self.func_resolver.res {
                if func_res.ne(&provided_res.resource_type) {
                    return None;
                }
            } else {
                return None;
            }
        }
        // if a resource was provided, then it didn't match at this point, so do not match
        else if let Some(_) = &self.func_resolver.res {
            return None;
        }

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
                self.func.clone().reify_types(&self.concrete_type_map),
                self.func_resolver.res.clone(),
                self.apply_coercions(),
            ))
        }
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

    /// returns a coercion function if arg_type matches the current provided type.
    /// if !allow_coercions, then return identity function
    fn check_provided_arg_matches(&mut self, t: &Type) -> Option<Function> {
        let provided_arg = self.current_provided_arg_type()?;
        if t.is_generic() {
            let concrete_types = t.match_to_concrete(provided_arg)?;
            for (type_lbl, concrete_type) in concrete_types.iter() {
                self.concrete_type_map.insert(String::from(type_lbl), concrete_type.clone());
            }
            // do we need to handle coercions with generics?
            Some(SpecialFunctions::id())
        } else if provided_arg.eq(t) {
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
            self.func_resolver.res.clone(),
            vec![sem.clone()],
        )
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

}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use bigdecimal::BigDecimal;
    use num_bigint::BigInt;
    use crate::analyzer::{Sem, Typed};
    use crate::environment::Environment;
    use crate::external_resources::{BuiltInResources, IOResource};
    use crate::function_resolver::{FuncMatcher, FunctionResolver};
    use crate::functions::{get_coercions, SpecialFunctions};
    use crate::types::{Function, Signature, Type};
    use crate::value_store::InMemoryValueStore;
    use crate::values::Value;

    #[test]
    fn test_find_funcs_with_value_type() {
        let mut env = Environment::new(Box::new(InMemoryValueStore::create()));
        for f in get_coercions() {
            env.put_function(f)
        }

        let resolver = FunctionResolver {
            env: &env,
            name: "".to_string(),
            arg_sems: &vec![],
            res: None
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
            &Type::Resource("file".to_string()),
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
    fn test_resource_matches() {
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
                Ok(Value::str_val(""))
            }
        );

        let my_fn2 = Function::create(
            "my_func",
            Signature {
                value: Type::String,
                arguments: vec![Type::Fractional, Type::Fractional],
                resource_type: Some(BuiltInResources::file_resource_type())
            },
            |args| {
                Ok(Value::str_val(""))
            }
        );

        let my_fn3 = Function::create(
            "my_func",
            Signature {
                value: Type::String,
                arguments: vec![Type::Fractional, Type::Fractional],
                resource_type: Some(BuiltInResources::http_resource_type())
            },
            |args| {
                Ok(Value::str_val(""))
            }
        );


        env.put_function(my_fn.clone());
        env.put_function(my_fn2.clone());
        env.put_function(my_fn3.clone());

        let sems = &vec![
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1u8) }),
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1u8) }),
        ];
        let resource_opt = Some(IOResource {
            id: String::from("file-resource"),
            properties: Default::default(),
            resource_type: BuiltInResources::file_resource_type(),
        });

        // should not match function with no resource
        match run_matcher(&my_fn, &mut env, sems, resource_opt.clone(), false) {
            None => {
                // expected
            }
            Some(_) => { assert_eq!(my_fn, my_fn2, "my_fn<>:(Frac,Frac)->String !~ my_fn<file>:(Frac,Frac)->String") }
        }
        // should match
        match run_matcher(&my_fn2, &mut env, sems, resource_opt.clone(), false) {
            None => { assert_eq!(1, 2, "my_fn<file>:(Frac,Frac)->String =~ my_fn<file>:(Frac,Frac)->String") }
            Some(Sem::FnCall(f, res, args)) => {
                assert_eq!(f.sig().resource_type, Some(BuiltInResources::file_resource_type()));
                assert!(res.is_some());
                assert_eq!(res.unwrap().id, String::from("file-resource"));
            }
            Some(unexpected) => {
                assert_eq!(1, 2, "Unexpected match: {}", unexpected)
            }
        }
        // should not match function that requires different resource type
        match run_matcher(&my_fn3, &mut env, sems, resource_opt.clone(), false) {
            None => {
                // expected
            }
            Some(_) => { assert_eq!(my_fn, my_fn2, "my_fn<http>:(Frac,Frac)->String !~ my_fn<file>:(Frac,Frac)->String") }
        }

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
                Ok(Value::ValueString { val : String::from("")})
            }
        );
        env.put_function(my_fn.clone());


        // should match
        assert!(run_matches(&my_fn, &mut env, &vec![
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1u8) }),
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1u8) }),
        ], false), "Should match without coercion");

        // should not match
        assert!(!run_matches(&my_fn, &mut env, &vec![
            Sem::ValueIntegral(Value::ValueIntegral { val: BigInt::from(1u8) }),
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1u8) }),
        ], false), "Should NOT match without coercion");

        // should not match
        assert!(!run_matches(&my_fn, &mut env, &vec![], false),
                "Should NOT match without arguments");

        // should match with coercion
        assert!(run_matches(&my_fn, &mut env, &vec![
            Sem::ValueIntegral(Value::ValueIntegral { val: BigInt::from(1u8) }),
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1u8) }),
        ], true), "Test match with simple coercion");

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
                Ok(Value::ValueString { val : String::from("")})
            }
        );
        env.put_function(my_fn.clone());


        // should match
        assert!(run_matches(&my_fn, &mut env, &vec![
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1) }),
            Sem::ValueString(Value::ValueString { val: String::from("") }),
        ], false), "Should match without coercion");

        // shouldn't match without arguments
        assert!(!run_matches(&my_fn, &mut env, &vec![
        ], false), "shouldn't match without arguments");

        // should match
        assert!(run_matches(&my_fn, &mut env, &vec![
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1u8) }),
            Sem::ValueString(Value::ValueString { val: String::from("") }),
            Sem::ValueString(Value::ValueString { val: String::from("") }),
            Sem::ValueString(Value::ValueString { val: String::from("") }),
            Sem::ValueString(Value::ValueString { val: String::from("") }),
        ], false), "Should match without coercion");

        // should match
        assert!(run_matches(&my_fn, &mut env, &vec![
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1u8) }),
        ], false), "Should match without any vararg matches");

        // shouldn't match without coercion
        assert!(!run_matches(&my_fn, &mut env, &vec![
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1u8) }),
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1u8) }),
        ], false), "Shouldn't match without coercion");

        // should match with coercion
        assert!(run_matches(&my_fn, &mut env, &vec![
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1u8) }),
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1u8) }),
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1u8) }),
        ], true), "Should match with coercion");

    }

    #[test]
    fn test_generics() {
        let mut env = Environment::new(Box::new(InMemoryValueStore::create()));
        for f in get_coercions() {
            env.put_function(f)
        }

        let my_fn = Function::create(
            "+",
            Signature {
                value: Type::list_of(Type::generic("A")),
                arguments: vec![Type::list_of(Type::generic("A")), Type::generic("A")],
                resource_type: None
            },
            |args| {
                Ok(Value::ValueString { val : String::from("")})
            }
        );
        let my_fn2 = Function::create(
            "+",
            Signature {
                value: Type::list_of(Type::generic("A")),
                arguments: vec![Type::list_of(Type::generic("A")), Type::list_of(Type::generic("A"))],
                resource_type: None
            },
            |args| {
                Ok(Value::ValueString { val : String::from("")})
            }
        );
        env.put_function(my_fn.clone());
        env.put_function(my_fn2.clone());

        let arg_sems = &vec![
            Sem::ValueList(
                vec![
                    Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(1u8) }),
                    Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(2u8) }),
                ]
            ),
            Sem::ValueFractional(Value::ValueFractional { val: BigDecimal::from(3u8) }),
        ];

        let result = run_matcher(&my_fn, &mut env, arg_sems, None, false);

        match result {
            None => {
                assert_eq!(1, 2, "Expected (List<A>,A) =~ (List<Frac>,Frac)")
            }
            Some(s) => {
                match s {
                    Sem::FnCall(f, res, arg_sems) => {
                        assert_eq!(2, arg_sems.len());
                        assert_eq!(2, f.sig().arguments.len());
                        assert_eq!(&Type::list_of(Type::Fractional),
                                   f.sig().arguments.get(0).unwrap());
                        assert_eq!(&Type::Fractional,
                                   f.sig().arguments.get(1).unwrap());
                        assert_eq!(Type::list_of(Type::Fractional), f.sig().value);
                        assert!(res.is_none());
                    }
                    _ => {
                        assert_eq!(1, 2, "Expected (List<A>,A) =~ (List<Frac>,Frac)")
                    }
                }
            }
        }

        let arg_sems2 = &vec![
            Sem::ValueList(
                vec![
                    Sem::ValueFractional(Value::frac_val(1.0)),
                    Sem::ValueFractional(Value::frac_val(2.0)),
                ]
            ),
            Sem::ValueList(
                vec![
                    Sem::ValueFractional(Value::frac_val(3.0)),
                    Sem::ValueFractional(Value::frac_val(4.0)),
                ]
            ),
        ];

        let result2 = run_matcher(&my_fn, &mut env, arg_sems, None, false);


        match result2 {
            None => {
                assert_eq!(1, 2, "Expected (List<A>,A) =~ (List<Frac>,Frac)")
            }
            Some(s) => {
                match s {
                    Sem::FnCall(f, res, arg_sems) => {
                        assert_eq!(2, arg_sems.len());
                        assert_eq!(2, f.sig().arguments.len());
                        assert_eq!(&Type::list_of(Type::Fractional),
                                   f.sig().arguments.get(0).unwrap());
                        assert_eq!(&Type::list_of(Type::Fractional),
                                   f.sig().arguments.get(1).unwrap());
                        assert_eq!(Type::list_of(Type::Fractional), f.sig().value);
                        assert!(res.is_none());
                    }
                    _ => {
                        assert_eq!(1, 2, "Expected (List<A>,A) =~ (List<Frac>,Frac)")
                    }
                }
            }
        }

    }
    fn run_matches(func : &Function, env: &mut Environment, arg_sems: &Vec<Sem>, allow_coercions : bool) -> bool {
        match run_matcher(func, env, arg_sems, None, allow_coercions) {
            None => {
                false
            }
            Some(_) => {
                true
            }
        }
    }

    fn run_matcher(func : &Function, env: &mut Environment, arg_sems: &Vec<Sem>, res : Option<IOResource>, allow_coercions : bool) -> Option<Sem> {
        let mut func_resolver = FunctionResolver {
            env: &env,
            name : func.name().clone(),
            arg_sems,
            res
        };

        let mut arg_types = vec![];
        for s in func_resolver.arg_sems {
            arg_types.push(s.get_type().clone());
        }
        FuncMatcher {
            func_resolver: &func_resolver,
            func,
            func_arg_idx: 0,
            provided_arg_sems: &func_resolver.arg_sems,
            provided_arg_types: arg_types,
            provided_arg_idx: 0,
            allow_coercions,
            coercion_fns: vec![],
            concrete_type_map: HashMap::new(),
        }.run()
    }


}
