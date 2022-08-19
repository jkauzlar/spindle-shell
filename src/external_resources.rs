use std::collections::HashMap;
use crate::types::Type;
use crate::Value;

#[derive(Clone)]
pub struct ExternalResource {
    pub id : String,
    pub properties : HashMap<String,Value>,
    pub resource_type : ResourceType,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ResourceType {
    pub id : String,
    // use vector to maintain order
    pub property_types : Vec<(String, Type)>,
}
