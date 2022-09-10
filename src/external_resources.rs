use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::types::Type;
use crate::Value;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IOResource {
    pub id : String,
    pub properties : HashMap<String,Value>,
    pub resource_type : ResourceType,
}

impl ToString for IOResource {
    fn to_string(&self) -> String {
        let mut buf = String::new();
        buf.push('#');
        buf.push_str(self.id.as_str());
        buf.push('[');
        buf.push_str(self.resource_type.id.as_str());
        buf.push(']');
        if !self.properties.is_empty() {
            buf.push('{');
            let mut first = true;
            for (prop_name, prop_val) in self.properties.iter() {
                if first {
                    first = false;
                } else {
                    buf.push(',')
                }
                buf.push_str(prop_name.as_str());
                buf.push('=');
                buf.push('\'');
                buf.push_str(prop_val.to_string().as_str());
                buf.push('\'');
            }
            buf.push('}');
        }
        buf
    }
}

impl IOResource {
    pub fn is_type(&self, type_name : &str) -> bool {
        type_name.eq(self.resource_type.id.as_str())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ResourceType {
    pub id : String,
    // use vector to maintain order
    pub property_types : Vec<(String, Type)>,
}

impl ResourceType {
    pub fn new_instance(&self, val : &str, properties : HashMap<String, Value>) -> Result<IOResource, ResourceInstantiationError> {
        let mut e = ResourceInstantiationError::create();
        for (prop_name, prop_type) in &self.property_types {
            match properties.get(prop_name.as_str()) {
                None => {
                    e.properties_not_defined.insert(prop_name.clone(), prop_type.clone());
                }
                Some(v) => {
                    if v.get_type().ne(prop_type) {
                        e.properties_with_incorrect_type.insert(prop_name.clone(), prop_type.clone());
                    }
                }
            }
        }
        if e.has_error() {
            Err(e)
        } else {
            Ok(IOResource {
                id : String::from(val),
                properties,
                resource_type: self.clone(),
            })
        }
    }
}

pub struct BuiltInResources {}

impl BuiltInResources {
    pub fn file_resource_type() -> ResourceType {
        ResourceType {
            id: String::from("file"),
            property_types: vec![],
        }
    }

    pub fn http_resource_type() -> ResourceType {
        ResourceType {
            id: String::from("http"),
            property_types: vec![],
        }
    }
}

#[derive(Debug)]
pub struct ResourceInstantiationError {
    pub properties_not_defined : HashMap<String, Type>,
    pub properties_with_incorrect_type : HashMap<String, Type>,
}

impl ResourceInstantiationError {
    fn create() -> Self {
        ResourceInstantiationError {
            properties_not_defined: Default::default(),
            properties_with_incorrect_type: Default::default()
        }
    }

    fn has_error(&self) -> bool {
        !self.properties_not_defined.is_empty() || !self.properties_with_incorrect_type.is_empty()
    }
}

impl Display for ResourceInstantiationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("")
    }
}

impl Error for ResourceInstantiationError {

}