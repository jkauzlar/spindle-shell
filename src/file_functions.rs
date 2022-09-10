use std::fs;

use crate::evaluator::EvaluationError;
use crate::external_resources::{BuiltInResources, IOResource};
use crate::types::{Function, FunctionArgs, Signature, Type};
use crate::Value;
use crate::Value::{ValueString, ValueVoid};

pub struct FileFunctions {}

impl FileFunctions {

    pub fn get_file_functions() -> Vec<Function> {
        vec![
            Self::fn_read(),
            Self::fn_write(),
        ]
    }

    pub fn fn_read() -> Function {
        Function::create("read", Signature {
                value: Type::String,
                arguments: vec![],
                resource_type: Some(BuiltInResources::file_resource_type()),
            }, | args : FunctionArgs | {
                if let Some(IOResource { id : filename, .. }) = args.res {
                    match fs::read_to_string(filename) {
                        Ok(res) => {
                            return Ok(ValueString { val : res});
                        }
                        Err(e) => {
                            return Err(EvaluationError::new(e.to_string().as_str()));
                        }
                    }
                }
                panic!("")
            })
    }

    pub fn fn_write() -> Function {
        Function::create("write", Signature {
            value: Type::Void,
            arguments: vec![Type::String],
            resource_type: Some(BuiltInResources::file_resource_type()),
        },|args : FunctionArgs | {
            if let Some(IOResource { id : filename, .. }) = &args.res {
                if let Value::ValueString { val } = &args.get_unchecked(0) {
                    match fs::write(filename.clone(), val.clone()) {
                        Ok(_) => {
                            return Ok(ValueVoid);

                        }
                        Err(err) => {
                            return Err(EvaluationError::new(err.to_string().as_str()))
                        }
                    }
                }
            }
            panic!("")
        })
    }

}


#[cfg(test)]
mod tests {
    use std::fs;
    use reqwest::Url;
    use crate::external_resources::{BuiltInResources, IOResource};

    use crate::file_functions::FileFunctions;
    use crate::Value;

    #[test]
    fn test_file_write_read() {
        let path = "target/test-file-name";
        let file_contents = "test";
        let res = IOResource {
            id: String::from(path),
            properties: Default::default(),
            resource_type: BuiltInResources::file_resource_type(),
        };

        let call = FileFunctions::fn_write().create_call(vec![Value::str_val(file_contents)]).with_resource(res.clone());
        match call.run() {
            Ok(result) => {
                println!("{}", result.to_string());
                match fs::read_to_string(path) {
                    Ok(s) => {
                        assert_eq!(file_contents, s)
                    }
                    Err(err) => { assert_eq!(1,2, "{}", err)}
                }
            }
            Err(err) => {
                assert_eq!(1, 2, "{}", err)
            }
        }


        let call_read = FileFunctions::fn_read().create_call(vec![]).with_resource(res.clone());
        match call_read.run() {
            Ok(result) => {
                assert_eq!(Value::str_val(file_contents), result);
                fs::remove_file(path);
            }
            Err(err) => {
                assert_eq!(1, 2, "{}", err)
            }
        }

    }

}
