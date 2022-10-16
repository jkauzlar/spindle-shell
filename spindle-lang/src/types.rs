use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;
use std::sync::{Arc, Mutex};

use crate::analyzer::TypeError;
use crate::evaluator::EvaluationError;
use crate::external_resources::{IOResource, ResourceType};
use crate::type_reader::TypeReader;
use crate::types::Type::Property;
use crate::values::Value;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type {
    String,
    Integral,
    Fractional,
    Boolean,
    Time,
    Resource(String),
    List(Box<Type>),
    Property(String,Box<Type>),
    PropertySet(Vec<Type>), // arguments must be properties
    TypeLiteral,
    Void,
    /// Signature type only; not a Value type
    Generic(String),
    /// Signature type only; not a Value type
    VarArgs(Box<Type>),
}

impl Type {
    pub fn is_generic(&self) -> bool {
        match self {
            Type::Generic(_) => {
                true
            }
            Type::List(t) => {
                t.is_generic()
            }
            Type::Property(_, t) => {
                t.is_generic()
            }
            Type::PropertySet(ps) => {
                for t in ps {
                    if t.is_generic() {
                        return true;
                    }
                }
                false
            }
            Type::VarArgs(t) => {
                t.is_generic()
            }
            _ => {
                false
            }
        }
    }

    /// should be called on the type that contains the generics; other type must be concrete
    pub fn match_to_concrete(&self, other_type : &Type) -> Option<HashMap<String, Type>> {
        let mut concrete_type_map : HashMap<String, Type> = HashMap::new();
        if self.is_generic() {
            match self {
                Type::List(list_type) => {
                    match other_type {
                        Type::List(other_list_type) => {
                            let mappings = list_type.match_to_concrete(other_list_type)?;
                            for (mapping_label, mapping_type) in mappings.iter() {
                                concrete_type_map.insert(mapping_label.clone(), mapping_type.clone());
                            }
                        }
                        _ => {
                            return None;
                        }
                    }
                }
                Type::Property(prop_name, prop_type) => {
                    match other_type {
                        Type::Property(other_prop_name, other_prop_type) => {
                            if prop_name.eq(other_prop_name) {
                                let mappings = prop_type.match_to_concrete(other_prop_type)?;
                                for (mapping_label, mapping_type) in mappings.iter() {
                                    concrete_type_map.insert(mapping_label.clone(), mapping_type.clone());
                                }
                            } else {
                                return None;
                            }
                        }
                        _ => {
                            return None;
                        }
                    }
                }
                Type::PropertySet(props) => {
                    match other_type {
                        Type::PropertySet(other_props) => {
                            if props.len() != other_props.len() {
                                return None;
                            }
                            for other_prop in other_props {
                                match other_prop {
                                    Property(other_prop_name, _) => {
                                        let prop = self.get_property(other_prop_name)?;
                                        let mappings = prop.match_to_concrete(other_prop)?;
                                        for (mapping_label, mapping_type) in mappings.iter() {
                                            concrete_type_map.insert(mapping_label.clone(), mapping_type.clone());
                                        }
                                    }
                                    _ => {
                                        panic!("Property set contains non-property type!")
                                    }
                                }
                            }
                        }
                        _ => {
                            return None;
                        }
                    }
                }
                Type::Generic(type_label) => {
                    concrete_type_map.insert(type_label.clone(), other_type.clone());
                }
                Type::VarArgs(_t) => {
                    panic!("handle varargs separately")
                }
                _ => {
                    panic!("Can't have a generic non-abstract type!")
                }
            }

            Some(concrete_type_map)
        } else if self.eq(other_type) {
            Some(concrete_type_map)
        } else {
            None
        }
    }

    pub fn reify_generics(&self, concrete_types : &HashMap<String, Type>) -> Type {
        let unboxed : Type = self.clone();
        match unboxed {
            Type::Generic(name) => {
                concrete_types.get(name.as_str()).unwrap().clone()
            }
            Type::List(t) => {
                Type::List(Box::new(t.reify_generics(concrete_types)))
            }
            Type::Property(name, t) => {
                Type::Property(name.clone(),
                               Box::new(t.reify_generics(concrete_types)))
            }
            Type::PropertySet(props) => {
                let mut props_vec : Vec<Type> = vec![];
                for p in props {
                    props_vec.push(p.reify_generics(concrete_types))
                }
                Type::PropertySet(props_vec)
            }
            Type::VarArgs(t) => {
                Type::VarArgs(Box::new(t.reify_generics(concrete_types)))
            }
            matched => {
                matched
            }
        }
    }


    pub fn list_of(other_type : Type) -> Self {
        Type::List(Box::new(other_type))
    }

    pub fn generic(lbl : &str) -> Self {
        Type::Generic(String::from(lbl))
    }

    pub fn prop(name : &str, t : Type) -> Self {
        Type::Property(String::from(name), Box::new(t))
    }

    pub fn vararg(t : Type) -> Self {
        Type::VarArgs(Box::new(t))
    }

    pub fn get_property(&self, name : &str) -> Option<Self> {
        match self {
            Type::PropertySet(props) => {
                for prop in props {
                    match prop {
                        Type::Property(prop_name, _) => {
                            if prop_name.as_str().eq(name) {
                                return Some(prop.clone());
                            }
                        }
                        _ => {
                            panic!("Property set contains non-property item!")
                        }
                    }
                }
                None
            }
            _ => {
                panic!("Not a property set!")
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Signature {
    pub value : Type,
    pub arguments : Vec<Type>,
    pub resource_type : Option<ResourceType>,
}

impl Signature {
    pub fn is_generic(&self) -> bool {
        for t in &self.arguments {
            if t.is_generic() {
                return true;
            }
        }

        false
    }
}

impl Display for Signature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut args_strs = vec![];
        for arg in &self.arguments {
            args_strs.push(arg.to_string());
        }
        let args_str = args_strs.join(", ");
        let mut res_str = String::new();
        if let Some(res) = &self.resource_type {
            res_str.push_str(res.id.as_str());
        }
        f.write_str(format!("({})<{}> -> {}", args_str, res_str, self.value).as_str())
    }
}

/// For push functions, this serves to separate a function call into two operations, one being to
/// place another item into the function's state (Push the item); and the other being to collect
/// the final result (Collect), which will be passed either to the user or through the next pipe
#[derive(Clone)]
pub enum PushState {
    Setup,
    Push,
    Collect,
}

#[derive(Clone)]
pub struct FunctionArgs {
    pub res : Option<IOResource>,
    pub vals : Vec<Value>,
    pub state : Arc<Mutex<FunctionState>>,
    pub push_state : PushState,
}

impl FunctionArgs {
    pub fn new(vals : Vec<Value>, state : Arc<Mutex<FunctionState>>) -> Self {
        FunctionArgs {
            res : None,
            vals,
            state,
            push_state: PushState::Push,
        }
    }

    pub fn init_state(&mut self, k : &str, initial_val : Value) {
        let mut st  = self.state.lock().expect("Unable to unlock FunctionArg::state!");
        st.state_map.insert(String::from(k), initial_val);
    }

    pub fn update_state(&mut self, k : &str, f : fn(&Value) -> Value) {
        let mut st  = self.state.lock().expect("Unable to unlock FunctionArg::state!");
        if let Some(updated) = st.state_map.get(k).map(f) {
            st.state_map.insert(String::from(k), updated);
        }
    }

    pub fn with_resource(&mut self, res : IOResource) -> Self {
        self.res = Some(res);
        self.clone()
    }

    pub fn state_value(&self, k : &str) -> Option<Value> {
        let mut st  = self.state.lock().expect("Unable to unlock FunctionArg::state!");
        st.state_map.get(k).map(|v| v.clone())
    }

    pub fn collect(&mut self) -> Self {
        self.push_state = PushState::Collect;
        self.clone()
    }

    pub fn push(&mut self) -> Self {
        self.push_state = PushState::Push;
        self.clone()
    }

    pub fn get_unchecked(&self, idx : usize) -> &Value {
        match self.vals.get(idx) {
            None => {
                panic!()
            }
            Some(v) => {
                v
            }
        }
    }

    pub fn get(&self, idx: usize) -> Option<&Value> {
        self.vals.get(idx)
    }

    pub fn has_resource(&self, res_type : &ResourceType) -> bool {
        if let Some(res) = &self.res {
            return res.resource_type.eq(res_type);
        }
        false
    }
}


#[derive(Clone)]
pub struct FunctionCall {
    func : Function,
    args : Vec<Value>,
    resource : Option<IOResource>,
    state : Arc<Mutex<FunctionState>>,
}

impl FunctionCall {

    pub fn with_resource(&mut self, res : IOResource) -> Self {
        self.resource = Some(res);
        self.clone()
    }

    pub fn derive_with_args(&self, args : Vec<Value>) -> Self {
        Self {
            func: self.func.clone(),
            args,
            resource: self.resource.clone(),
            state: self.state.clone(),
        }
    }

    pub fn mut_state(&self) -> Arc<Mutex<FunctionState>> {
        self.state.clone()
    }

    pub fn run(&self) -> Result<Value, EvaluationError> {
        let mut args = FunctionArgs::new(self.args.clone(), self.state.clone());
        if let Some(res) = &self.resource {
            args = args.with_resource(res.clone()).push()
        }

        self.func.call(args)
    }

    pub fn collect(&self) -> Result<Value, EvaluationError> {
        let mut args = FunctionArgs::new(self.args.clone(), self.state.clone());
        if let Some(res) = &self.resource {
            args = args.with_resource(res.clone()).collect()
        }

        self.func.call(args)
    }


}



/// used to communicate from the client to the function
#[derive(Debug, Clone)]
pub enum CallState {
    New,
    Continue,
    Interrupt,
}

#[derive(Debug, Clone)]
pub enum ReturnState {
    HasMore,
    Stop,
}

#[derive(Debug, Clone)]
pub struct FunctionState {
    pub call_state : CallState,
    pub return_state : ReturnState,
    pub state_map : HashMap<String, Value>,
}

impl FunctionState {
    pub fn new() -> Self {
         Self {
             call_state : CallState::New,
             state_map : HashMap::new(),
             return_state : ReturnState::HasMore,
         }
    }

    pub fn has_more(&self) -> bool {
        match self.return_state {
            ReturnState::HasMore => { true }
            ReturnState::Stop => { false }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function {
    pub(crate) name : String,
    pub(crate) sig : Signature,
    f : fn(FunctionArgs) -> Result<Value,EvaluationError>,
}

impl Function {
    /// Change the generic types to concrete types
    pub fn reify_types(&self, concrete_types : &HashMap<String, Type>) -> Function {
        Function {
            name: self.name.clone(),
            sig: Signature {
                value: self.sig.value.reify_generics(concrete_types),
                arguments: self.sig.arguments.clone()
                    .iter()
                    .map(|t| t.reify_generics(concrete_types))
                    .collect(),
                resource_type: self.sig.resource_type.clone(),
            },
            f: self.f,
        }
    }

    pub fn create(name : &str, sig : Signature, f : fn(FunctionArgs) -> Result<Value, EvaluationError>) -> Function {
        Function {
            name: String::from(name),
            sig,
            f,
        }
    }

    pub fn create_call(&self, args : Vec<Value>) -> FunctionCall {
        FunctionCall {
            func: self.clone(),
            args,
            resource: None,
            state : Arc::new(Mutex::new(FunctionState::new())),
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

    pub fn call(&self, args : FunctionArgs) -> Result<Value, EvaluationError> {
        (self.f)(args)
    }

}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(format!("{}:{}", self.name, self.sig).as_str())
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
            Type::Resource(t) => {
                f.write_str(format!("Resource({})", t).as_str())
            }
            Type::List(t) => {
                f.write_str(format!("List({})", t).as_str())
            }

            Type::Generic(t) => {
                f.write_str(format!("[{}]", t).as_str())
            }

            Type::VarArgs(t) => {
                f.write_str(format!("({})*", t).as_str())
            }
            Type::Property(name, t) => {
                f.write_str(format!("Prop({}:{})", name, t).as_str())
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
            Type::Void => {
                f.write_str("Void")
            }
            Type::TypeLiteral => {
                f.write_str("Type")
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


