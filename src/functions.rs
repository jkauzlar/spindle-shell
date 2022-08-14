use std::ops::{Add, Div, Mul, Sub};
use num_bigint::BigInt;
use crate::types::{Function, Signature, Type};
use crate::values::{Value};


pub fn get_builtins() -> Vec<Function> {
    let mut funcs = vec![];
    funcs.push(Function::create(
        "+",
        Signature {
            value: Type::Integral,
            arguments: vec![Type::Integral, Type::Integral]
        },
        |args| {

            if let Value::ValueIntegral { val : arg0 } = args.get(0).unwrap() {
                if let Value::ValueIntegral { val : arg1 } = args.get(1).unwrap() {
                    return Box::new(Value::ValueIntegral { val : arg0.add(arg1) });
                }
            }
            panic!("")
        }
    ));
    funcs.push(Function::create(
        "-",
        Signature {
            value: Type::Integral,
            arguments: vec![Type::Integral, Type::Integral]
        },
        |args| {
            if let Value::ValueIntegral { val : arg0 } = args.get(0).unwrap() {
                if let Value::ValueIntegral { val : arg1 } = args.get(1).unwrap() {
                    return Box::new(Value::ValueIntegral { val : arg0.sub(arg1) });
                }
            }
            panic!("")
        }
    ));

    funcs.push(Function::create(
        "*",
        Signature {
            value: Type::Integral,
            arguments: vec![Type::Integral, Type::Integral]
        },
        |args| {
            if let Value::ValueIntegral { val : arg0 } = args.get(0).unwrap() {
                if let Value::ValueIntegral { val : arg1 } = args.get(1).unwrap() {
                    return Box::new(Value::ValueIntegral { val : arg0.mul(arg1) });
                }
            }
            panic!("")
        }
    ));

    funcs.push(Function::create(
        "/",
        Signature {
            value: Type::Integral,
            arguments: vec![Type::Integral, Type::Integral]
        },
        |args| {

            if let Value::ValueIntegral { val : arg0 } = args.get(0).unwrap() {
                if let Value::ValueIntegral { val : arg1 } = args.get(1).unwrap() {
                    return Box::new(Value::ValueIntegral { val : arg0.div(arg1) });
                }
            }
            panic!("")
        }
    ));

    funcs.push(Function::create(
        "+",
        Signature {
            value: Type::Fractional,
            arguments: vec![Type::Fractional, Type::Fractional]
        },
        |args| {

            if let Value::ValueFractional { val : arg0 } = args.get(0).unwrap() {
                if let Value::ValueFractional { val : arg1 } = args.get(1).unwrap() {
                    return Box::new(Value::ValueFractional { val : arg0.add(arg1) });
                }
            }
            panic!("")
        }
    ));
    funcs.push(Function::create(
        "-",
        Signature {
            value: Type::Fractional,
            arguments: vec![Type::Fractional, Type::Fractional]
        },
        |args| {
            if let Value::ValueFractional { val : arg0 } = args.get(0).unwrap() {
                if let Value::ValueFractional { val : arg1 } = args.get(1).unwrap() {
                    return Box::new(Value::ValueFractional { val : arg0.sub(arg1) });
                }
            }
            panic!("")
        }
    ));

    funcs.push(Function::create(
        "*",
        Signature {
            value: Type::Fractional,
            arguments: vec![Type::Fractional, Type::Fractional]
        },
        |args| {
            if let Value::ValueFractional { val : arg0 } = args.get(0).unwrap() {
                if let Value::ValueFractional { val : arg1 } = args.get(1).unwrap() {
                    return Box::new(Value::ValueFractional { val : arg0.mul(arg1) });
                }
            }
            panic!("")
        }
    ));

    funcs.push(Function::create(
        "/",
        Signature {
            value: Type::Fractional,
            arguments: vec![Type::Fractional, Type::Fractional]
        },
        |args| {

            if let Value::ValueFractional { val : arg0 } = args.get(0).unwrap() {
                if let Value::ValueFractional { val : arg1 } = args.get(1).unwrap() {
                    return Box::new(Value::ValueFractional { val : arg0.div(arg1) });
                }
            }
            panic!("")
        }
    ));

    funcs.push(Function::create(
        "inc",
        Signature {
            value: Type::Integral,
            arguments: vec![Type::Integral]
        },
        |args| {

            if let Value::ValueIntegral { val : arg0 } = args.get(0).unwrap() {
                return Box::new(Value::ValueIntegral { val : arg0.add(BigInt::from(1)) });
            }
            panic!("")
        }
    ));

    funcs
}

