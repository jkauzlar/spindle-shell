use std::collections::HashMap;

pub trait ValueStore {
    fn resolve(&self, name: &str) -> Option<&(String, String)>;
    fn store(&mut self, name: &str, val_type : String, val : String);
}

pub struct InMemoryValueStore {
    map : HashMap<String, (String, String)>
}

impl InMemoryValueStore {
    pub fn create() -> Self {
        InMemoryValueStore {
            map : HashMap::new()
        }
    }
}

impl ValueStore for InMemoryValueStore {


    fn resolve(&self, name: &str) -> Option<&(String, String)> {
        self.map.get(name)
    }

    fn store(&mut self, name: &str, val_type : String, val : String) {
        self.map.insert(String::from(name), (val_type, val));
    }
}