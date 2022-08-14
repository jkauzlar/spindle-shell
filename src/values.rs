use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::str::{FromStr};
use bigdecimal::{BigDecimal};
use num_bigint::{BigInt};
use reqwest::Url;
use crate::types::Type;

#[derive(Debug,Clone, Eq, PartialEq)]
pub enum Value {

    ValueString {
        val : String,
    },

    ValueIntegral {
        val : BigInt,
    },

    ValueFractional {
        val : BigDecimal,
    },

    ValueBoolean {
        val : bool,
    },

    ValueTime {
        val : u64,
    },

    ValueUrl {
        val : Url
    },
}


impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_string().as_str())
    }
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::ValueString { .. } => Type::String,
            Value::ValueIntegral { .. } => Type::Integral,
            Value::ValueFractional { .. } => Type::Fractional,
            Value::ValueBoolean { .. } => Type::Boolean,
            Value::ValueTime { .. } => Type::Time,
            Value::ValueUrl { .. } => Type::URL,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Value::ValueString { val } => format!("\"{}\"", val),
            Value::ValueIntegral { val } => format!("{}", val),
            Value::ValueFractional { val } => format!("{}", val),
            Value::ValueBoolean { val } => format!("{}", val),
            Value::ValueTime { val } => format!("{}", val),
            Value::ValueUrl { val } => format!("@{}", val),
        }
    }

    pub fn from_string(val_type : &str, val_str : &str) -> Result<Value, TypeSerializationError> {
        match Type::from_str(val_type) {
            Ok(t) => {
                match  t {
                    Type::String => {
                        Ok(Value::ValueString {
                            val: String::from(val_str)
                        })
                    }
                    Type::Integral => {
                        match BigInt::from_str(val_str) {
                            Ok(val) => {
                                Ok(Value::ValueIntegral { val })
                            }
                            Err(err) => {
                                Err(TypeSerializationError::new(err.to_string().as_str()))
                            }
                        }
                    }
                    Type::Fractional => {
                        match BigDecimal::from_str(val_str) {
                            Ok(val) => {
                                Ok(Value::ValueFractional { val })
                            }
                            Err(err) => {
                                Err(TypeSerializationError::new(err.to_string().as_str()))
                            }
                        }
                    }
                    Type::Boolean => {
                        let mut val = true;
                        if val_str.eq("true") {
                            val = true;
                        } else if val_str.eq("false") {
                            val = false;
                        } else {
                            return Err(TypeSerializationError::new(
                                format!("Invalid boolean string [{}]", val_str).as_str()))
                        }

                        Ok(Value::ValueBoolean { val })
                    }
                    Type::Time => {
                        Ok(Value::ValueTime {
                            val: 0
                        })
                    }
                    Type::URL => {
                        match Url::parse(val_str) {
                            Ok(val) => {
                                Ok(Value::ValueUrl { val })
                            }
                            Err(err) => {
                                Err(TypeSerializationError::new(err.to_string().as_str()))
                            }
                        }
                    }
                    _ => Err(TypeSerializationError::new(
                        format!("Invalid type string [{}] with value [{}]", val_type, val_str).as_str())),
                }
            }
            Err(err) => {
                Err(TypeSerializationError::new(err.to_string().as_str()))
            }
        }
    }
}

#[derive(Debug)]
pub struct TypeSerializationError {
    pub message : String
}

impl TypeSerializationError {
    pub fn new(message : &str) -> TypeSerializationError {
        TypeSerializationError {
            message : String::from(message)
        }
    }
}

impl Display for TypeSerializationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.message.as_str())
    }
}

impl Error for TypeSerializationError { }