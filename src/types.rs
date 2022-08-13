use std::fmt::{Display, Formatter};
use std::process::Output;
use std::str::FromStr;
use crate::analyzer::TypeError;

use crate::values::Value;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type {
    String,
    Integral,
    Fractional,
    Boolean,
    Time,
    URL,
    Generic(String)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Signature {
    pub value : Type,
    pub arguments : Vec<Type>,
}

impl Display for Signature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut args_strs = vec![];
        for arg in &self.arguments {
            args_strs.push(arg.to_string());
        }
        let args_str = args_strs.join(", ");
        f.write_str(format!("({}) -> {}", args_str, self.value).as_str())
    }
}

#[derive(Clone)]
pub struct Function {
    name : String,
    sig : Signature,
    f : fn(Vec<Value>) -> Box<Value>,
}

impl Function {

    pub fn create(name : &str, sig : Signature, f : fn(Vec<Value>) -> Box<Value>) -> Function {
        Function {
            name: String::from(name),
            sig,
            f
        }
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    pub fn sig(&self) -> &Signature {
        &self.sig
    }

    pub fn return_type(&self) -> &Type {
        &self.sig.value
    }

    pub fn call(&self, args : Vec<Value>) -> Box<Value> {
        (self.f)(args)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(format!("{}:{}", self.name, self.sig.to_string()).as_str())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::String => {
                f.write_str("String")
            }
            Type::Integral => {
                f.write_str("Int")
            }
            Type::Fractional => {
                f.write_str("Frac")
            }
            Type::Boolean => {
                f.write_str("Boolean")
            }
            Type::Time => {
                f.write_str("Time")
            }
            Type::URL => {
                f.write_str("URL")
            }
            Type::Generic(t) => {
                f.write_str(format!("[{}]", t).as_str())
            }
        }
    }
}

impl FromStr for Type {
    type Err = TypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq("String") {
            Ok(Type::String)
        } else if s.eq("Int") {
            Ok(Type::Integral)
        } else if s.eq("Frac") {
            Ok(Type::Fractional)
        } else if s.eq("Boolean") {
            Ok(Type::Boolean)
        } else if s.eq("Time") {
            Ok(Type::Time)
        } else if s.eq("URL") {
            Ok(Type::URL)
        } else {
            Err(TypeError::new(format!("Unknown type [{}]", s).as_str()))
        }
    }
}
