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
    Property(String,Box<Type>),
    PropertySet(Vec<Type>), // arguments must be properties
    /// Signature type only; not a Value type
    Generic(String),
    /// Signature type only; not a Value type
    VarArgs(Box<Type>),
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

#[derive(Clone)]
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
                f.write_str("Integral")
            }
            Type::Fractional => {
                f.write_str("Fractional")
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
            Type::List(t) => {
                f.write_str(format!("List({})", t.to_string()).as_str())
            }

            Type::Generic(t) => {
                f.write_str(format!("[{}]", t).as_str())
            }

            Type::VarArgs(t) => {
                f.write_str(format!("({})*", t.to_string()).as_str())
            }
            Type::Property(name, t) => {
                f.write_str(format!("Prop({}:{})", name, t.to_string()).as_str())
            }
            Type::PropertySet(props) => {
                let mut buf = String::new();
                buf.push_str("PropertySet(");
                let mut first = true;
                for prop in props {
                    if first {
                        first = false;
                    } else {
                        buf.push(',');
                    }
                    buf.push_str(prop.to_string().clone().as_str())
                }

                buf.push(')');
                f.write_str(buf.as_str())
            }
        }
    }
}

impl FromStr for Type {
    type Err = TypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        TypeReader::read(s)
    }
}

pub struct TypeReader {
    type_str : String,
    buf : Vec<char>,
    pos : usize,
}

impl TypeReader {
    pub fn read(type_str : &str) -> Result<Type, TypeError> {
        let mut tr = TypeReader {
            type_str : String::from(type_str),
            buf: type_str.chars().collect(),
            pos: 0,
        };
        let result = tr.read_type();

        if tr.end_of_input_reached() {
            result
        } else {
            Err(TypeError::new(format!(
                "No valid type found in type-string [{}] or extra-characters discovered at position [{}] with character [{}]",
                tr.type_str, tr.pos, tr.peek().unwrap()).as_str()))
        }
    }

    fn read_type(&mut self) -> Result<Type, TypeError> {
        let mut t : Option<Type> = None;
        let type_name = self.read_type_name();
        if type_name.eq(&String::from("String")) {
            t = Some(Type::String)
        } else if type_name.eq(&String::from("Integral")) {
            t = Some(Type::Integral)
        } else if type_name.eq(&String::from("Fractional")) {
            t = Some(Type::Fractional)
        } else if type_name.eq(&String::from("Boolean")) {
            t = Some(Type::Boolean)
        } else if type_name.eq(&String::from("Time")) {
            t = Some(Type::Time)
        } else if type_name.eq(&String::from("URL")) {
            t = Some(Type::URL)
        } else if type_name.eq(&String::from("Resource")) {
            t = Some(Type::Resource)
        } else if type_name.eq(&String::from("Prop")) {
            if self.read_char('(') {
                let prop_name = self.read_identifier();
                if prop_name.len() > 0 && self.read_char(':') {
                    let prop_type = self.read_type()?;
                    if self.read_char(')') {
                        t = Some(Type::Property(prop_name, Box::new(prop_type)));
                    }
                }
            }
        } else if type_name.eq(&String::from("PropertySet")) {
            let sts = self.read_sub_types()?;
            t = Some(Type::PropertySet(sts));
        } else if type_name.eq(&String::from("List")) {
            let sts = self.read_sub_types()?;
            if let Some(st) = sts.get(0) {
                t = Some(Type::List(Box::new(st.clone())));
            } else {
                t = None;
            }
        } else {
            t = None
        }

        match t {
            None => {
                Err(TypeError::new(format!(
                    "No valid type found in type-string [{}]", self.type_str).as_str()))
            }
            Some(t) => {
                Ok(t)
            }
        }
    }

    fn read_sub_types(&mut self) -> Result<Vec<Type>, TypeError> {
        if !self.read_char('(') {
            Err(TypeError::new(
                format!("Opening parenthesis expected after subtype in input [{}]",
                        self.type_str).as_str()))
        } else {
            let types = self.read_comma_separated_types()?;

            if !self.read_char(')') {
                Err(TypeError::new(
                    format!("Closing parenthesis expected after subtype in input [{}]",
                            self.type_str).as_str()))
            } else {
                Ok(types)
            }
        }
    }

    fn read_comma_separated_types(&mut self) -> Result<Vec<Type>, TypeError> {
        let mut types = vec![];

        loop {
            let t = self.read_type()?;
            types.push(t);

            if !self.read_char(',') {
                break;
            }
        }

        Ok(types)
    }

    fn read_type_name(&mut self) -> String {
        let mut type_buf = String::new();
        loop {
            match self.peek() {
                None => { break; }
                Some(&c) => {
                    if c.is_alphabetic() {
                        type_buf.push(c);
                        self.pos = self.pos + 1;
                    } else {
                        break;
                    }
                }
            }
        }

        type_buf
    }

    fn peek(&self) -> Option<&char> {
        self.buf.get(self.pos)
    }

    fn end_of_input_reached(&self) -> bool {
        self.pos >= self.buf.len()
    }
    fn read_char(&mut self, expected: char) -> bool {
        match self.peek() {
            None => {
                false
            }
            Some(&c) => {
                if expected == c {
                    self.pos = self.pos + 1;
                    true
                } else {
                    false
                }
            }
        }
    }
    fn read_identifier(&mut self) -> String {
        let mut type_buf = String::new();
        loop {
            match self.peek() {
                None => { break; }
                Some(&c) => {
                    if c.is_alphabetic() || c.is_digit(10) || c == '_' || c =='.' {
                        type_buf.push(c);
                        self.pos = self.pos + 1;
                    } else {
                        break;
                    }
                }
            }
        }

        type_buf
    }
}


#[cfg(test)]
mod tests {
    use crate::analyzer::TypeError;
    use crate::types::{Type, TypeReader};

    #[test]
    fn test_typereader() {
        assert_type("String", Type::String);
        assert_type("Integral", Type::Integral);
        assert_type("Fractional", Type::Fractional);
        assert_type("Boolean", Type::Boolean);
        assert_type("Time", Type::Time);
        assert_type("URL", Type::URL);
        assert_type("Resource", Type::Resource);
        assert_type("List(String)", Type::List(Box::new(Type::String)));
        assert_type("Prop(my_prop:String)", Type::Property(
            String::from("my_prop"),Box::new(Type::String)));
        assert_type("PropertySet(Prop(first_prop:String),Prop(second_prop:Integral),Prop(third_prop:Fractional))",
                    Type::PropertySet(
                        vec!(
                            Type::Property(String::from("first_prop"),Box::new(Type::String)),
                            Type::Property(String::from("second_prop"),Box::new(Type::Integral)),
                            Type::Property(String::from("third_prop"),Box::new(Type::Fractional)),
        )));
        assert_type("List(List(Time))", Type::List(Box::new(Type::List(Box::new(Type::Time)))));
    }

    fn assert_type(type_str : &str, expected : Type) {
        debug_assert_eq!(TypeReader::read(type_str), Ok(expected));
    }
}