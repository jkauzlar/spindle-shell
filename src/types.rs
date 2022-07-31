use crate::values::{Value};

#[derive(Debug, Eq, PartialEq)]
pub enum Type {
    String,
    Integral,
    Fractional,
    Boolean,
    Time,
    URI,
    Generic(String)
}

#[derive(Debug, Eq, PartialEq)]
pub struct Signature {
    value : Type,
    arguments : Vec<Type>,
}

pub struct Function {
    name : String,
    sig : Signature,
    f : fn(Vec<&dyn Value>) -> Box<dyn Value>,
}

impl Function {

    pub fn create(name : &str, sig : Signature, f : fn(Vec<&dyn Value>) -> Box<dyn Value>) -> Function {
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

    pub fn call(&self, args : Vec<&dyn Value>) -> Box<dyn Value> {
        (self.f)(args)
    }
}