use std::collections::HashMap;
use crate::Value;

pub struct ExternalResource {
    pub id : String,
    pub properties : HashMap<String,Value>,
    pub resource_type : ResourceType,
}

pub struct ResourceType {

}
