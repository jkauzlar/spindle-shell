use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::str::{FromStr};
use bigdecimal::{BigDecimal, FromPrimitive};
use num_bigint::{BigInt};
use crate::types::{FunctionArgs, Type};

#[derive(Debug,Clone, Eq, PartialEq)]
pub enum Value {
    ValueVoid,
    ValueTypeLiteral(Type),

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

    ValueProperty {
        name : String,
        val : Box<Value>,
    },

    ValuePropertySet {
        vals : Vec<Value>,
    },

    ValueList {
        item_type : Type,
        vals : Vec<Value>,
    },

    ValueStream {
        item_type : Type,
        has_more : fn(FunctionArgs) -> bool,
        next: fn(FunctionArgs) -> Value,
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
            Value::ValueVoid => Type::Void,
            Value::ValueString { .. } => Type::String,
            Value::ValueIntegral { .. } => Type::Integral,
            Value::ValueFractional { .. } => Type::Fractional,
            Value::ValueBoolean { .. } => Type::Boolean,
            Value::ValueTime { .. } => Type::Time,
            Value::ValueList { item_type, vals: _vals } => Type::List(Box::new(item_type.clone())),
            Value::ValueProperty { name , val } =>
                Type::Property(name.clone(), Box::new(val.get_type())),
            Value::ValuePropertySet { vals } => {
                let mut val_types = vec!();
                for val in vals {
                    val_types.push(val.get_type());
                }
                Type::PropertySet(val_types)
            }
            Value::ValueTypeLiteral(_) => Type::TypeLiteral,
            Value::ValueStream { item_type, .. } => { Type::Stream(Box::new(item_type.clone()))}
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Value::ValueVoid => String::new(),
            Value::ValueString { val } => format!("\"{}\"", val),
            Value::ValueIntegral { val } => format!("{}", val),
            Value::ValueFractional { val } => format!("{}", val),
            Value::ValueBoolean { val } => format!("{}", val),
            Value::ValueTime { val } => format!("{}", val),
            // always end a URL with a space, because it's the easiest way to tell when to stop reading
            Value::ValueList { item_type: _, vals } => {
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
            Value::ValueProperty { name, val } => {
                format!("{}:{}", name, val)
            }
            Value::ValuePropertySet { vals } => {
                let mut buf = String::new();
                buf.push('{');
                let mut first = true;
                for val in vals {
                    if !first {
                        buf.push(',');
                    } else {
                        first = false;
                    }
                    buf.push_str(val.to_string().as_str());
                }
                buf.push('}');
                buf.to_string()
            }
            Value::ValueTypeLiteral(t) => {
                format!("'{}", t)
            }
            Value::ValueStream { item_type, .. } => {
                format!("({})|>", item_type)
            }
        }
    }

    pub fn from_string(val_type : &str, val_str : &str) -> Result<Value, TypeSerializationError> {
        match Type::from_str(val_type) {
            Ok(t) => {
                ValueReader::read(val_str, &t)
            }
            Err(err) => {
                Err(TypeSerializationError::new(err.to_string().as_str()))
            }
        }
    }

    /// if this is an integral, then return the value, otherwise zero
    pub fn as_bigint(&self) -> BigInt {
        if let Value::ValueIntegral { val } = self {
            val.clone()
        } else {
            BigInt::from(0)
        }
    }

    /// if this is a List, return vector of values, otherwise vector containing this item
    pub fn as_vec(&self) -> Vec<Value> {
        if let Value::ValueList { item_type, vals } = self {
            vals.clone()
        } else {
            vec![self.clone()]
        }
    }

    pub fn get_default(t : &Type) -> Self {
        match t {
            Type::String => { Value::str_val("")}
            Type::Integral => { Value::int_val(0) }
            Type::Fractional => { Value::frac_val(0.0)}
            Type::Boolean => { Value::false_val() }
            Type::List(t) => { Value::ValueList { item_type: *t.clone(), vals: vec![] }}
            Type::Property(n, t) => {
                Value::ValueProperty { name: n.clone(), val: Box::new(Self::get_default(t)) }
            }
            Type::PropertySet(props) => {
                let prop_defaults = props.iter().map(Self::get_default).collect();
                Value::ValuePropertySet { vals: prop_defaults }
            }
            Type::TypeLiteral => { Value::ValueTypeLiteral(Type::Void)}
            _ => { Value::ValueVoid }
        }
    }

    pub fn str_val(v : &str) -> Self {
        Value::ValueString { val : String::from(v) }
    }

    pub fn int_val(v : i128) -> Self {
        Value::ValueIntegral { val : BigInt::from(v) }
    }

    pub fn frac_val(v : f64) -> Self {
        Value::ValueFractional { val : BigDecimal::from_f64(v).unwrap_or(BigDecimal::from(0)) }
    }

    pub fn true_val() -> Self {
        Value::ValueBoolean { val : true }
    }

    pub fn false_val() -> Self {
        Value::ValueBoolean { val : false }
    }


}

pub struct ValueReader {
    val_str : String,
    buf : Vec<char>,
    pos : usize,
}

impl ValueReader {
    pub fn read(val_str : &str, t : &Type) -> Result<Value, TypeSerializationError> {
        let mut vp = ValueReader {
            val_str : String::from(val_str),
            buf : val_str.chars().collect(),
            pos : 0,
        };
        vp.read_value(t)
    }

    fn read_value(&mut self, t : &Type) -> Result<Value, TypeSerializationError> {
        self.read_until(|c| !c.is_whitespace());
        match t {
            Type::String => {
                self.read_string()
            }
            Type::Integral => {
                self.read_integral()
            }
            Type::Fractional => {
                self.read_fractional()
            }
            Type::Boolean => {
                self.read_boolean()
            }
            Type::Property(n, t) => {
                self.read_property(&n, &t)
            }
            Type::PropertySet(ts) => {
                self.read_property_set(&ts)
            }
            Type::List(t) => {
                self.read_list(&t)
            }
            _ => Err(TypeSerializationError::new(
                format!("Invalid value string [{}] with type [{}]", self.val_str, t).as_str())),
        }

    }

    fn read_property(&mut self, n: &String, t: &Box<Type>) -> Result<Value, TypeSerializationError> {
        let prop_name = self.read_until(|c| c == ':');
        if !prop_name.trim().eq(n.as_str()) {
            Err(TypeSerializationError::new(
                format!("Property name [{}] doesn't match expected [{}]", prop_name, n).as_str()))
        } else {
            self.pos = self.pos + 1; // skip colon
            let v = self.read_value(t)?;
            Ok(Value::ValueProperty { name: prop_name, val: Box::new(v) })
        }
    }

    fn read_property_set(&mut self, ts : &Vec<Type>) -> Result<Value, TypeSerializationError> {
        let mut vals = vec![];
        let mut consumed_props : Vec<&Type> = vec![];
        let mut unused_props : Vec<&Type> = vec![];
        for t in ts {
            unused_props.push(&t);
        }
        self.assert_char('{',
                         "Expected opening curly-brace, got end of input",
                         "Expected opening curly-brace",
        )?;

        while unused_props.len() > 0 {

            let mut prop_name = self.read_until(|c| c.is_whitespace() || c == ':');

            self.assert_char(':',
                             "Expected colon after property name, got end of input",
                             "Expected colon after property name",
            )?;

            // find correct property and associated type
            let mut found_type = None;
            let mut found_ref : Option<&Type> = None;
            let mut idx = 0;
            for unused in &unused_props {
                match ValueReader::check_prop(prop_name.as_str(), unused) {
                    None => {
                        // continue looping
                    }
                    Some(prop_type) => {
                        found_type = Some(prop_type);
                        found_ref = Some(unused);
                        break;
                    }
                }
                idx = idx + 1;
            }

            if let Some(t) = found_type {
                consumed_props.push(found_ref.unwrap());
                unused_props.remove(idx);
                vals.push(Value::ValueProperty { name : prop_name, val: Box::new(self.read_value(t)?) } );
            } else {
                return Err(TypeSerializationError::new(format!("Unexpected property [{}] in property set", prop_name).as_str()));
            }

            if unused_props.len() > 0 {
                self.assert_char(',',
                                 "Expected comma, got end of input",
                                 "Expected comma",
                )?;
            }
        }

        self.assert_char('}',
                         "Expected closing curly-brace, got end of input",
                         "Expected closing curly-brace",
        )?;

        Ok(Value::ValuePropertySet { vals })
    }

    fn check_prop<'a>(expected_prop_name : &str, t : &'a Type) -> Option<&'a Type> {
        match t {
            Type::Property(prop_name, prop_type) => {
                if expected_prop_name.eq(prop_name.as_str()) {
                    Some(&*prop_type)
                } else {
                    None
                }
            }
            _ => {
                None
            }
        }
    }

    fn read_boolean(&mut self) -> Result<Value, TypeSerializationError> {
        let s = self.read_until(|c| !c.is_alphabetic());
        if s.eq("true") {
            Ok(Value::ValueBoolean { val: true })
        } else if s.eq("false") {
            Ok(Value::ValueBoolean { val: false })
        } else {
            Err(TypeSerializationError::new(format!(
                "Error parsing boolean: Expected 'true' or 'false', got [{}]",
                s.as_str()).as_str()))
        }
    }

    fn read_fractional(&mut self) -> Result<Value, TypeSerializationError> {
        let s = self.read_until(|c| !c.is_digit(10) && c != '.');
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

    fn read_integral(&mut self) -> Result<Value, TypeSerializationError> {
        let s = self.read_until(|c| !c.is_digit(10));
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

    fn read_string(&mut self) -> Result<Value, TypeSerializationError> {
        self.assert_char('"',
                         "Error parsing string: Expected opening double-quote, got end of input",
                         format!(
                             "Error parsing string: Expected opening double-quote in input [{}]",
                             self.val_str.as_str()).as_str())?;
        let s = self.read_until(|c| c == '"');
        self.assert_char('"',
                         "Error parsing string: Expected closing double-quote, got end of input",
                         format!(
                             "Error parsing string: Expected closing double-quote in input [{}]",
                             self.val_str.as_str()).as_str())?;
        Ok(Value::ValueString {
            val: s.clone()
        })
    }

    fn read_list(&mut self, t: &Box<Type>) -> Result<Value, TypeSerializationError> {
        self.assert_char('[',
                         "Error parsing List: unexpected end of input",
                         format!(
                             "Error parsing List: List should start with '[' in input [{}]",
                             self.val_str.as_str()).as_str())?;

        let mut vals = vec!();

        loop {
            let v = self.read_value(t)?;
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
            item_type: *t.clone(),
            vals
        })
    }


    fn assert_char(&mut self, expected_char : char, end_of_input_message : &str, unexpected_char_message : &str) -> Result<(), TypeSerializationError> {
        self.read_until(|c| !c.is_whitespace());

        match self.peek() {
            None => {
                return Err(TypeSerializationError::new(end_of_input_message));
            }
            Some(&c) => {
                if c == expected_char {
                    self.pos = self.pos + 1;
                } else {
                    return Err(TypeSerializationError::new(unexpected_char_message));
                }
            }
        }
        Ok(())
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
    use bigdecimal::{BigDecimal};
    use num_bigint::BigInt;
    use crate::types::Type;
    use crate::values::Value;
    use crate::values::ValueReader;

    #[test]
    fn test_value_reader() {
        assert_eq!(ValueReader::read("\"hello there\"", &Type::String).unwrap(),
                   Value::ValueString { val : String::from("hello there")});
        assert_eq!(ValueReader::read("123456789", &Type::Integral).unwrap(),
                   Value::ValueIntegral { val : BigInt::from(123456789u32 )});
        assert_eq!(ValueReader::read("1234.56789", &Type::Fractional).unwrap(),
                   Value::ValueFractional { val : BigDecimal::from_str("1234.56789").unwrap()});
        // add extra whitespace
        assert_eq!(ValueReader::read("  123456789  ", &Type::Integral).unwrap(),
                   Value::ValueIntegral { val : BigInt::from(123456789u32 )});
        debug_assert_eq!(
            ValueReader::read("[1,2,3,4,5]", &Type::List(Box::new(Type::Integral))).unwrap(),
            Value::ValueList { item_type: Type::Integral, vals : vec!(
                Value::ValueIntegral { val: BigInt::from(1u32) },
                Value::ValueIntegral { val: BigInt::from(2u32) },
                Value::ValueIntegral { val: BigInt::from(3u32) },
                Value::ValueIntegral { val: BigInt::from(4u32) },
                Value::ValueIntegral { val: BigInt::from(5u32) },
            ) }
        );

        // add extra whitespace
        debug_assert_eq!(
            ValueReader::read("  [ \"a\"  ,  \"b\",\"c\"  ,\"d\",  \"e\" ]  ", &Type::List(Box::new(Type::String))).unwrap(),
            Value::ValueList { item_type: Type::String, vals : vec!(
                Value::ValueString { val: String::from("a") },
                Value::ValueString { val: String::from("b") },
                Value::ValueString { val: String::from("c") },
                Value::ValueString { val: String::from("d") },
                Value::ValueString { val: String::from("e") },
            ) }
        );

        debug_assert_eq!(
            ValueReader::read(
                "this.is.a-test-prop:1",
                &Type::Property(String::from("this.is.a-test-prop"), Box::new(Type::Integral))).unwrap(),
            Value::ValueProperty {
                name: String::from("this.is.a-test-prop"),
                val: Box::new(Value::ValueIntegral { val: BigInt::from(1) })
            }
        );

        debug_assert_eq!(
            ValueReader::read(
                "{prop.1:\"a string\",prop.2:3.14}",
                &Type::PropertySet(vec![
                    Type::Property(String::from("prop.1"), Box::new(Type::String)),
                    Type::Property(String::from("prop.2"), Box::new(Type::Fractional)),
                ]),
            ).unwrap(),
            Value::ValuePropertySet {
                vals: vec![
                    Value::ValueProperty {
                        name: String::from("prop.1"),
                        val: Box::new(Value::ValueString { val: String::from("a string") })
                    },
                    Value::ValueProperty {
                        name: String::from("prop.2"),
                        val: Box::new(Value::ValueFractional { val: BigDecimal::from_str("3.14").unwrap() })
                    },
                ]
            }
        );
    }
}