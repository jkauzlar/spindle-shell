use std::sync::{Arc, Mutex, MutexGuard};
use crate::analyzer::TypeError;
use crate::environment::Environment;
use crate::types::{Function, Type};
use crate::values::Value;

pub enum Term {
    PipedTerms(Box<Term>, Box<Term>),
    FunctionTerm(String, Vec<Term>),
    SetterTerm(String, Box<Term>),
    ValueTerm(Value)
}

impl Term {

    pub fn resolve_type(&self, env : Arc<Mutex<Environment>>, carry_type : Option<Type>) -> Result<Type, TypeError> {
        match self {
            Term::PipedTerms(left_term, right_term) => {

                todo!()
            }
            Term::FunctionTerm(func_name, args) => {
                let mut arg_types : Vec<Type> = vec![];
                for arg_term in args {
                    arg_types.push(arg_term.resolve_type(env.clone(), None)?);
                }
                if carry_type.is_some() {
                    arg_types.push(carry_type.unwrap());
                }
                // resolve the function against arg types and resource (if in scope)

                let func = resolve_function(env.lock().unwrap(), func_name, &arg_types, None);


                todo!()
            }
            Term::SetterTerm(identifier, term) => {
                Ok(term.resolve_type(env.clone(), None)?)
            }
            Term::ValueTerm(v) => {
                Ok(v.get_type())
            }
        }
    }

}

fn resolve_function(env : MutexGuard<Environment>, fn_name : &String, arg_types : &Vec<Type>, res : Option<Resource>) -> Function {

    todo!()
}
