use std::error::Error;
use std::fmt::{Debug, Display, format, Formatter};
use std::str::{FromStr};
use bigdecimal::{BigDecimal};
use num_bigint::{BigInt, ParseBigIntError};
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

    ValueList {
        item_type : Type,
        vals : Vec<Value>
    }
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
            Value::ValueList { item_type, vals: _vals } => Type::List(Box::new(item_type.clone())),
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
            Value::ValueList { item_type, vals } => {
                let mut buf = String::new();
                buf.push('[');
                let mut first = true;
                for val in vals {
                    if !first {
                        buf.push(',');
                    } else {
                        first = false;
                    }
                    buf.push_str(val.to_string().as_str());
                }
                buf.push(']');
                buf.to_string()
            }
        }
    }

    pub fn from_string(val_type : &str, val_str : &str) -> Result<Value, TypeSerializationError> {
        match Type::from_str(val_type) {
            Ok(t) => {
                ValueReader::read(val_str, t)
            }
            Err(err) => {
                Err(TypeSerializationError::new(err.to_string().as_str()))
            }
        }
    }
}

pub struct ValueReader {
    val_str : String,
    buf : Vec<char>,
    pos : usize,
}

impl ValueReader {
    pub fn read(val_str : &str, t : Type) -> Result<Value, TypeSerializationError> {
        let mut vp = ValueReader {
            val_str : String::from(val_str),
            buf : val_str.chars().collect(),
            pos : 0,
        };
        vp.read_value(t)
    }

    fn read_value(&mut self, t : Type) -> Result<Value, TypeSerializationError> {
        self.read_until(|c| !c.is_whitespace());
        match t {
            Type::String => {
                match self.peek() {
                    None => {
                        return Err(TypeSerializationError::new(
                            "Error parsing string: Expected opening double-quote, got end of input"));
                    }
                    Some(&c) => {
                        if c == '"' {
                            self.pos = self.pos + 1;
                        } else {
                            return Err(TypeSerializationError::new(
                                format!(
                                    "Error parsing string: Expected opening quote in input [{}]",
                                    self.val_str.as_str()).as_str()))
                        }
                    }
                }
                let s = self.read_until(|c| c == '"');
                match self.peek() {
                    None => {
                        return Err(TypeSerializationError::new(
                            "Error parsing string: Expected closing double-quote, got end of input"));
                    }
                    Some(&c) => {
                        if c == '"' {
                            self.pos = self.pos + 1;
                        } else {
                            return Err(TypeSerializationError::new(
                                format!(
                                    "Error parsing string: Expected closing double-quote in input [{}]",
                                    self.val_str.as_str()).as_str()))
                        }
                    }
                }
                Ok(Value::ValueString {
                    val : s.clone()
                })
            }
            Type::Integral => {
                let s = self.read_until(|c| c == ',' || c == ']' || c.is_whitespace() );
                match BigInt::from_str(s.as_str()) {
                    Ok(val) => {
                        Ok(Value::ValueIntegral { val })
                    }
                    Err(err) => {
                        Err(TypeSerializationError::new(format!("Error parsing integral: {}",
                                                                err.to_string().as_str()).as_str()))
                    }
                }
            }
            Type::Fractional => {
                let s = self.read_until(|c| c == ',' || c == ']' || c.is_whitespace() );
                match BigDecimal::from_str(s.as_str()) {
                    Ok(val) => {
                        Ok(Value::ValueFractional { val })
                    }
                    Err(err) => {
                        Err(TypeSerializationError::new(format!("Error parsing fractional: {}",
                                                                err.to_string().as_str()).as_str()))
                    }
                }
            }
            Type::Boolean => {
                let s = self.read_until(|c| c == ',' || c == ']' || c.is_whitespace() );
                if s.eq("true") {
                    Ok(Value::ValueBoolean { val : true })
                } else if s.eq("false") {
                    Ok(Value::ValueBoolean { val : false })
                } else {
                    Err(TypeSerializationError::new(format!(
                        "Error parsing boolean: Expected 'true' or 'false', got [{}]",
                        s.as_str()).as_str()))
                }
            }
            Type::URL => {
                match self.peek() {
                    None => {
                        return Err(TypeSerializationError::new(
                            "Error parsing List: unexpected end of input"));
                    }
                    Some(&c) => {
                        if c == '@' {
                            self.pos = self.pos + 1;
                        } else {
                            return Err(TypeSerializationError::new(format!(
                                "Error parsing URL: URL should start with '@', got [{}]",
                                c).as_str()));
                        }
                    }
                }
                let s = self.read_until(|c| c == ',' || c == ']' || c.is_whitespace() );
                match Url::parse(s.as_str()) {
                    Ok(val) => {
                        Ok(Value::ValueUrl { val })
                    }
                    Err(err) => {
                        Err(TypeSerializationError::new(format!(
                            "Error parsing URL: Expected 'true' or 'false', got [{}]",
                            s.as_str()).as_str()))
                    }
                }
            }
            Type::List(t) => {
                match self.peek() {
                    None => {
                        return Err(TypeSerializationError::new(
                            "Error parsing List: unexpected end of input"));
                    }
                    Some(&c) => {
                        if c == '[' {
                            self.pos = self.pos + 1;
                        } else {
                            return Err(TypeSerializationError::new(format!(
                                "Error parsing List: List should start with '[' in input [{}]",
                                self.val_str.as_str()).as_str()));
                        }
                    }
                }

                let mut vals = vec!();

                loop {
                    let v = self.read_value(*t.clone())?;
                    vals.push(v);

                    self.read_until(|c| !c.is_whitespace());

                    match self.peek() {
                        None => {
                            return Err(TypeSerializationError::new(
                                format!(
                                    "Unexpected end of input parsing list in input [{}]",
                                    self.val_str).as_str()));
                        }
                        Some(c) => {
                            match c {
                                ',' => {
                                    self.pos = self.pos + 1;
                                }
                                ']' => {
                                    self.pos = self.pos + 1;
                                    break;
                                }
                                _ => {
                                    return Err(TypeSerializationError::new(
                                        format!(
                                            "Error parsing list: Unexpected character [{}] in input [{}]", c, self.val_str).as_str()));
                                }
                            }
                        }
                    }
                }

                Ok(Value::ValueList {
                    item_type: (*t).clone(),
                    vals
                })
            }
            _ => Err(TypeSerializationError::new(
                format!("Invalid value string [{}] with type [{}]", self.val_str, t).as_str())),
        }

    }


    fn peek(&self) -> Option<&char> {
        self.buf.get(self.pos)
    }

    fn read_until(&mut self, condition:fn(char)-> bool) -> String {
        let mut buf = String::new();

        loop {
            match self.peek() {
                None => {
                    break;
                }
                Some(&c) => {
                    if ! condition(c) {
                        buf.push(c);
                        self.pos = self.pos + 1;
                    } else {
                        break;
                    }
                }
            }
        }

        buf
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


#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use bigdecimal::{BigDecimal, FromPrimitive};
    use num_bigint::BigInt;
    use reqwest::Url;
    use crate::types::Type;
    use crate::Value;
    use crate::values::ValueReader;

    #[test]
    fn test_value_reader() {
        assert_eq!(ValueReader::read("\"hello there\"", Type::String).unwrap(),
                   Value::ValueString { val : String::from("hello there")});
        assert_eq!(ValueReader::read("123456789", Type::Integral).unwrap(),
                   Value::ValueIntegral { val : BigInt::from(123456789 )});
        assert_eq!(ValueReader::read("1234.56789", Type::Fractional).unwrap(),
                   Value::ValueFractional { val : BigDecimal::from_str("1234.56789").unwrap()});
        assert_eq!(ValueReader::read("@https://www.google.com", Type::URL).unwrap(),
                   Value::ValueUrl { val : Url::parse("https://www.google.com").unwrap()});
        // add extra whitespace
        assert_eq!(ValueReader::read("  123456789  ", Type::Integral).unwrap(),
                   Value::ValueIntegral { val : BigInt::from(123456789 )});
        debug_assert_eq!(
            ValueReader::read("[1,2,3,4,5]", Type::List(Box::new(Type::Integral))).unwrap(),
            Value::ValueList { item_type: Type::Integral, vals : vec!(
                Value::ValueIntegral { val: BigInt::from(1) },
                Value::ValueIntegral { val: BigInt::from(2) },
                Value::ValueIntegral { val: BigInt::from(3) },
                Value::ValueIntegral { val: BigInt::from(4) },
                Value::ValueIntegral { val: BigInt::from(5) },
            ) }
        );

        // add extra whitespace
        debug_assert_eq!(
            ValueReader::read("  [ \"a\"  ,  \"b\",\"c\"  ,\"d\",  \"e\" ]  ", Type::List(Box::new(Type::String))).unwrap(),
            Value::ValueList { item_type: Type::String, vals : vec!(
                Value::ValueString { val: String::from("a") },
                Value::ValueString { val: String::from("b") },
                Value::ValueString { val: String::from("c") },
                Value::ValueString { val: String::from("d") },
                Value::ValueString { val: String::from("e") },
            ) }
        );

    }
}