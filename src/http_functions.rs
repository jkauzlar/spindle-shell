use std::future::Future;
use reqwest::{Error, Response};
use crate::evaluator::EvaluationError;
use crate::external_resources::{BuiltInResources, IOResource};
use crate::types::{Function, FunctionArgs, Signature, Type};
use crate::Value;

struct HttpFunctions { }

impl HttpFunctions {

    pub fn fn_read() -> Function {
        Function::create("read", Signature {
            value: Type::String,
            arguments: vec![],
            resource_type: Some(BuiltInResources::http_resource_type()),
        }, Self::do_read)
    }

    pub fn fn_write() -> Function {
        Function::create("write", Signature {
            value: Type::Void,
            arguments: vec![Type::String],
            resource_type: Some(BuiltInResources::http_resource_type()),
        }, Self::do_write)
    }

    fn do_read(args : FunctionArgs) -> Result<Value, EvaluationError> {
        // if let Some(http_res) = args.res {
        //     return match reqwest::get(http_res.id).await {
        //         Ok(r) => {
        //             match r.text().await {
        //                 Ok(txt) => {
        //                     Ok(Value::ValueString { val: txt.clone() })
        //                 }
        //                 Err(e) => {
        //                     Err(EvaluationError::new(e.to_string().as_str()))
        //                 }
        //             }
        //         }
        //         Err(e) => {
        //             Err(EvaluationError::new(e.to_string().as_str()))
        //         }
        //     }
        // }
        panic!("")
    }

    fn do_write(args : FunctionArgs) -> Result<Value, EvaluationError> {
        // if let Some(http_res) = args.res {
        //     if let Value::ValueString { val : arg1 } = args.get_unchecked(0) {
        //         let client = reqwest::Client::new();
        //         return match client.post(http_res.id).body(arg1).send().await {
        //             Ok(response) => {
        //                 Ok(Value::ValueVoid)
        //             }
        //             Err(e) => {
        //                 Err(EvaluationError::new(e.to_string().as_str()))
        //             }
        //         }
        //     }
        // }
        panic!("")
    }
}