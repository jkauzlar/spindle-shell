use std::ops::{Add, Div, Mul, Neg, Sub};
use std::str::FromStr;
use bigdecimal::{BigDecimal, ToPrimitive};
use num_bigint::BigInt;
use crate::types::{Function, Signature, Type};
use crate::types::Type::Generic;
use crate::values::{Value};


macro_rules! create_cmp_fn {
   ($typ:tt, $val_name:tt, $name:expr, $cmp_fn:tt) => {
        Function::create(
            $name,
            Signature {
                value : Type::Boolean,
                arguments : vec![Type::$typ, Type::$typ],
                resource_type: None,
            },
            |args| {
                if let Value::$val_name { val : arg0 } = args.get(0).unwrap() {
                    if let Value::$val_name { val : arg1 } = args.get(1).unwrap() {
                        return Box::new(Value::ValueBoolean { val : arg0.cmp(arg1).$cmp_fn() });
                    }
                }
                panic!("");
            }
        )
    };
}

macro_rules! create_binary_endo_fn {
    ($name:expr, $typ:tt, $val_name:tt, $($stuff:tt)+) => {
        Function::create(
            $name,
            Signature {
                value: Type::$typ,
                arguments: vec![Type::$typ, Type::$typ],
                resource_type: None,
            },
            |args| {
                if let Value::$val_name { val : arg0 } = args.get(0).unwrap() {
                    if let Value::$val_name { val : arg1 } = args.get(1).unwrap() {
                        return Box::new(Value::$val_name { val : $($stuff)+(arg0, arg1)})
                    }
                }
                panic!("")
            }
        )
    }
}

macro_rules! create_unary_endo_fn {
    ($name:expr, $typ:tt, $val_name:tt, $($stuff:tt)+) => {
        Function::create(
            $name,
            Signature {
                value: Type::$typ,
                arguments: vec![Type::$typ],
                resource_type: None,
            },
            |args| {
                if let Value::$val_name { val : arg0 } = args.get(0).unwrap() {
                    return Box::new(Value::$val_name { val : $($stuff)+(arg0)})
                }
                panic!("")
            }
        )
    }
}

pub fn get_coercions() -> Vec<Function> {
    let mut funcs = vec![];

    funcs.push(Function::create(
       "coercion",
        Signature {
            value: Type::Fractional,
            arguments : vec![Type::Integral],
            resource_type: None,
        },
       |args : Vec<Value> | {
           if let Value::ValueIntegral { val : arg0 } = args.get(0).unwrap() {
               let result = BigDecimal::from_str(arg0.to_string().as_str()).unwrap();
               return Box::new(Value::ValueFractional { val : result });
           }
           panic!("");
       }
    ));

    funcs.push(Function::create(
       "coercion",
        Signature {
            value: Type::String,
            arguments: vec![Type::Integral],
            resource_type: None,
        },
        |args : Vec<Value> | {
            if let Value::ValueIntegral { val : arg0 } = args.get(0).unwrap() {
                return Box::new(Value::ValueString { val : arg0.to_string() });
            }
            panic!("");
        }
    ));

    funcs.push(Function::create(
        "coercion",
        Signature {
            value: Type::String,
            arguments: vec![Type::Fractional],
            resource_type: None,
        },
        |args : Vec<Value> | {
            if let Value::ValueFractional { val : arg0 } = args.get(0).unwrap() {
                return Box::new(Value::ValueString { val : arg0.to_string() });
            }
            panic!("");
        }
    ));

    funcs
}


pub fn get_builtins() -> Vec<Function> {
    let mut funcs = vec![];
    funcs.push(create_binary_endo_fn!("+", Integral, ValueIntegral, (|arg0 : &BigInt, arg1 : &BigInt| arg0.add(arg1))));
    funcs.push(create_binary_endo_fn!("-", Integral, ValueIntegral, (|arg0 : &BigInt, arg1 : &BigInt| arg0.sub(arg1))));
    funcs.push(create_binary_endo_fn!("*", Integral, ValueIntegral, (|arg0 : &BigInt, arg1 : &BigInt| arg0.mul(arg1))));
    funcs.push(create_binary_endo_fn!("/", Integral, ValueIntegral, (|arg0 : &BigInt, arg1 : &BigInt| arg0.div(arg1))));
    funcs.push(create_binary_endo_fn!("pow", Integral, ValueIntegral, (|arg0 : &BigInt, arg1 : &BigInt| arg0.pow(arg1.to_u32().unwrap()))));
    funcs.push(create_unary_endo_fn!("-", Integral, ValueIntegral, (|arg0 : &BigInt| arg0.neg())));
    funcs.push(create_unary_endo_fn!("inc", Integral, ValueIntegral, (|arg0 : &BigInt| arg0.add(BigInt::from(1)))));

    funcs.push(create_binary_endo_fn!("+", Fractional, ValueFractional, (|arg0 : &BigDecimal, arg1 : &BigDecimal| arg0.add(arg1))));
    funcs.push(create_binary_endo_fn!("-", Fractional, ValueFractional, (|arg0 : &BigDecimal, arg1 : &BigDecimal| arg0.sub(arg1))));
    funcs.push(create_binary_endo_fn!("*", Fractional, ValueFractional, (|arg0 : &BigDecimal, arg1 : &BigDecimal| arg0.mul(arg1))));
    funcs.push(create_binary_endo_fn!("/", Fractional, ValueFractional, (|arg0 : &BigDecimal, arg1 : &BigDecimal| arg0.div(arg1))));
    funcs.push(create_unary_endo_fn!("-", Fractional, ValueFractional, (|arg0 : &BigDecimal| arg0.neg())));

    funcs.push(create_binary_endo_fn!("+", String, ValueString, (|arg0 : &String, arg1 : &String| {
                    let mut result = String::from(arg0);
                    result.push_str(arg1);
                    result
    })));

    funcs.push(create_cmp_fn!(Integral, ValueIntegral, "==", is_eq));
    funcs.push(create_cmp_fn!(Integral, ValueIntegral, "!=", is_ne));
    funcs.push(create_cmp_fn!(Integral, ValueIntegral, ">", is_gt));
    funcs.push(create_cmp_fn!(Integral, ValueIntegral, "<", is_lt));
    funcs.push(create_cmp_fn!(Integral, ValueIntegral, ">=", is_ge));
    funcs.push(create_cmp_fn!(Integral, ValueIntegral, "<=", is_le));

    funcs.push(create_cmp_fn!(Fractional, ValueFractional, "==", is_eq));
    funcs.push(create_cmp_fn!(Fractional, ValueFractional, "!=", is_ne));
    funcs.push(create_cmp_fn!(Fractional, ValueFractional, ">", is_gt));
    funcs.push(create_cmp_fn!(Fractional, ValueFractional, "<", is_lt));
    funcs.push(create_cmp_fn!(Fractional, ValueFractional, ">=", is_ge));
    funcs.push(create_cmp_fn!(Fractional, ValueFractional, "<=", is_le));

    funcs.push(create_cmp_fn!(String, ValueString, "==", is_eq));
    funcs.push(create_cmp_fn!(String, ValueString, "!=", is_ne));
    funcs.push(create_cmp_fn!(String, ValueString, ">", is_gt));
    funcs.push(create_cmp_fn!(String, ValueString, "<", is_lt));
    funcs.push(create_cmp_fn!(String, ValueString, ">=", is_ge));
    funcs.push(create_cmp_fn!(String, ValueString, "<=", is_le));

    funcs.push(Function::create(
        "list",
        Signature {
            value: Type::List(Box::new(Type::Generic(String::from("A")))),
            arguments: vec![Type::Generic(String::from("A"))],
            resource_type: None,
        },
        |args : Vec<Value> | {
            if let v = args.get(0).unwrap() {
                return Box::new(Value::ValueList {
                    item_type : v.get_type(),
                    vals : vec![v.clone()] });
            }
            panic!("");
        }
    ));

    funcs
}

