use crate::analyzer::TypeError;
use crate::types::Type;

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

    /// used for inline type reading
    pub fn read_and_return_rest(type_str : &str) -> (Result<Type, TypeError>, String) {
        let mut tr = TypeReader {
            type_str : String::from(type_str),
            buf: type_str.chars().collect(),
            pos: 0,
        };
        let result = tr.read_type();
        let mut rest = String::new();

        if !tr.end_of_input_reached() {
            for c in &tr.buf[tr.pos..] {
                rest.push(*c)
            }
        }

        (result, rest)
    }

    fn read_type(&mut self) -> Result<Type, TypeError> {
        let mut t : Option<Type> = None;
        let type_name = self.read_type_name();
        if type_name.eq(&String::from("Void")) {
            t = Some(Type::Void)
        } else if type_name.eq(&String::from("String")) {
            t = Some(Type::String)
        } else if type_name.eq(&String::from("Integral")) {
            t = Some(Type::Integral)
        } else if type_name.eq(&String::from("Fractional")) {
            t = Some(Type::Fractional)
        } else if type_name.eq(&String::from("Boolean")) {
            t = Some(Type::Boolean)
        } else if type_name.eq(&String::from("Time")) {
            t = Some(Type::Time)
        } else if type_name.eq(&String::from("Resource")) {
            let res_type = self.read_resource_type()?;
            t = Some(Type::Resource(res_type))
        } else if type_name.eq(&String::from("Prop")) {
            if self.read_char('(') {
                let prop_name = self.read_identifier();
                if !prop_name.is_empty() && self.read_char(':') {
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
            let sts : Vec<Type> = self.read_sub_types()?;
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

    fn read_resource_type(&mut self) -> Result<String, TypeError> {
        if !self.read_char('(') {
            Err(TypeError::new(
                format!("Opening parenthesis expected after resource type in input [{}]",
                        self.type_str).as_str()))
        } else {
            let res_type = self.read_identifier();

            if !self.read_char(')') {
                Err(TypeError::new(
                    format!("Closing parenthesis expected after resource type in input [{}]",
                            self.type_str).as_str()))
            } else {
                Ok(res_type)
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
                        self.pos += 1;
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
                    self.pos += 1;
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
                        self.pos += 1;
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
    use super::*;
    use std::collections::HashMap;
    use crate::types::{Function, Signature, Type};
    use crate::values::Value;

    #[test]
    fn test_typereader() {
        assert_type("String", Type::String);
        assert_type("Integral", Type::Integral);
        assert_type("Fractional", Type::Fractional);
        assert_type("Boolean", Type::Boolean);
        assert_type("Time", Type::Time);
        assert_type("Resource(file)", Type::Resource(String::from("file")));
        assert_type("List(String)", Type::List(Box::new(Type::String)));
        assert_type("Void", Type::Void);
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

    #[test]
    fn test_match_to_concrete() {
        let g1 = Type::list_of(Type::generic("A"));
        let c1 = Type::list_of(Type::String);

        match g1.match_to_concrete(&c1) {
            None => {
                assert_eq!(1, 2, "List(A) =~ List(String)")
            }
            Some(mappings) => {
                assert!(mappings.contains_key("A"));
                assert_eq!(mappings.get("A"), Some(&Type::String))
            }
        }

        let g2 = Type::list_of(Type::generic("A"));
        let c2 = Type::String;

        match g2.match_to_concrete(&c2) {
            None => {
                // expected
            }
            Some(_mappings) => {
                assert_eq!(1, 2, "List(A) !~ String")
            }
        }

        let g3 = Type::list_of(Type::generic("A"));
        let c3 = Type::list_of(Type::list_of(Type::Integral));

        match g3.match_to_concrete(&c3) {
            None => {
                assert_eq!(1, 2, "List(A) =~ List(List(Integral))")
            }
            Some(mappings) => {
                assert!(mappings.contains_key("A"));
                assert_eq!(mappings.get("A"), Some(&Type::list_of(Type::Integral)))
            }
        }

        let g4 = Type::PropertySet(vec![Type::prop("name1", Type::String), Type::prop("name2", Type::generic("A"))]);
        let c4 = Type::PropertySet(vec![Type::prop("name2", Type::Fractional), Type::prop("name1", Type::String)]);

        match g4.match_to_concrete(&c4) {
            None => {
                assert_eq!(1, 2, "PropertySet(Property('name1', String),Property('name2',Generic(A))) =~ PropertySet(Property('name2', Fractional),Property('name1',String))")
            }
            Some(mappings) => {
                assert!(mappings.contains_key("A"));
                assert_eq!(mappings.get("A"), Some(&Type::Fractional))
            }
        }
    }

    #[test]
    fn test_type_reification() {
        let my_fn = Function::create(
            "vararg_func",
            Signature {
                value: Type::List(Box::new(Type::Generic(String::from("A")))),
                arguments: vec![Type::VarArgs(Box::new(Type::Generic(String::from("A"))))],
                resource_type: None
            },
            |args| {
                Ok(Value::ValueString { val : String::from("")})
            }
        );
        let mut type_hash : HashMap<String, Type> = HashMap::new();
        type_hash.insert(String::from("A"), Type::String);

        let reified_fn = my_fn.reify_types(&type_hash);

        assert_eq!(reified_fn.name, my_fn.name);
        assert_eq!(reified_fn.sig.value, Type::List(Box::new(Type::String)));
        assert_eq!(reified_fn.sig.arguments.get(0).unwrap(), &Type::VarArgs(Box::new(Type::String)));
    }


    fn assert_type(type_str : &str, expected : Type) {
        debug_assert_eq!(TypeReader::read(type_str), Ok(expected));
    }
}