use std::ops::Add;
use num_bigint::BigInt;
use crate::types::{Function, Signature, Type};
use crate::values::{Value};

pub fn get_builtins() -> Vec<Function> {
    let mut funcs = vec![];
    funcs.push(Function::create(
        "add",
        Signature {
            value: Type::Integral,
            arguments: vec![Type::Integral, Type::Integral]
        },
        |args| {

            match args.get(0).unwrap() {
                Value::ValueIntegral { val : arg0 } => {
                    match args.get(1).unwrap() {
                        Value::ValueIntegral { val: arg1 } => {
                            Box::new(Value::ValueIntegral {
                                val: arg0.add(arg1)
                            })
                        }
                        _ => panic!("")
                    }
                }
                _ => panic!("")
            }
        }
    ));


    funcs
}