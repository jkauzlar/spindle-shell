use std::collections::HashMap;
use std::fmt::{Display, Formatter, Write};
use std::str::FromStr;
use crate::analyzer::TypeError;
use crate::external_resources::{ExternalResource, ResourceType};

use crate::values::Value;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type {
    String,
    Integral,
    Fractional,
    Boolean,
    Time,
    URL,
    Resource,
    List(Box<Type>),
    Generic(String),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Signature {
    pub value : Type,
    pub arguments : Vec<Type>,
    pub resource_type : Option<ResourceType>,
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
    marked_args : HashMap<String,Type>,
}

pub struct FunctionCall {
    func : Function,
    args : Vec<Value>,
    marked_args : HashMap<String, Value>,
    resource : Option<ExternalResource>
}

impl FunctionCall {
    pub fn set_marked_arg(&mut self, name : &str, v : Value) {
        self.marked_args.insert(String::from(name), v);
    }

    pub fn set_resource(&mut self, res : ExternalResource) {
        self.resource = Some(res);
    }

    pub fn run(&self) -> Box<Value> {
        self.func.call(self.args.clone())
    }
}


impl Function {

    pub fn create(name : &str, sig : Signature, f : fn(Vec<Value>) -> Box<Value>) -> Function {
        Function {
            name: String::from(name),
            sig,
            f,
            marked_args: HashMap::new(),
        }
    }

    pub fn create_call(&self, args : Vec<Value>) -> FunctionCall {
        FunctionCall {
            func: self.clone(),
            args,
            marked_args: Default::default(),
            resource: None
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

    pub fn set_marked_arg_type(&mut self, id : &str, t : Type) {
        self.marked_args.insert(String::from(id), t);
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
            Type::Resource => {
                f.write_str("Resource")
            }
            Type::Generic(t) => {
                f.write_str(format!("[{}]", t).as_str())
            }
            Type::List(t) => {
                f.write_str(format!("List<{}>", t.to_string()).as_str())
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
        } else if s.eq("Resource") {
            Ok(Type::Resource)
        } else {
            Err(TypeError::new(format!("Unknown type [{}]", s).as_str()))
        }
    }
}
