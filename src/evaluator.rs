use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Write};
use crate::analyzer::{Pipe, Sem, SemanticExpression, Typed, TypeError};
use crate::Environment;
use crate::types::Type;
use crate::Value::ValueList;
use crate::values::{Value, ValueReader};

pub struct Evaluator<'a> {
    env : &'a mut Box<Environment>
}

impl Evaluator<'_> {

    pub fn eval(env : &mut Box<Environment>, sem_expr : SemanticExpression) -> Result<Value, EvaluationError> {
        let mut evaluator = Evaluator { env };
        evaluator.eval_sem_expr(Box::new(sem_expr), None)
    }

    fn eval_sem_expr(&mut self, sem_expr : Box<SemanticExpression>, carry_val : Option<Value>) -> Result<Value, EvaluationError> {
        match *sem_expr {
            SemanticExpression::PipedCommand { sem, pipe } => {
                let left_val = self.eval_sem(Box::new(sem), carry_val)?;
                match pipe {
                    None => {
                        Ok(left_val)
                    }
                    Some((p, right_expr)) => {
                        let right_val = self.eval_sem_expr(right_expr, Some(left_val))?;
                        Ok(right_val)
                    }
                }
            }
            SemanticExpression::Setter(id, expr) => {
                let v = self.eval_sem_expr(expr, None)?;
                self.env.store_value(id.as_str(), v);

                // suppress output
                Ok(ValueReader::read("", Type::String).unwrap())
            }
        }
    }

    fn eval_sem(&mut self, sem : Box<Sem>, carry_val : Option<Value>) -> Result<Value, EvaluationError> {
        match *sem {
            Sem::FnCall(func, args) => {
                let mut arg_vals : Vec<Value> = vec!();
                for arg in args {
                    arg_vals.push(self.eval_sem(Box::new(arg), None)?)
                }
                if let Some(v) = carry_val {
                    arg_vals.push(v);
                }
                Ok(*func.create_call(arg_vals).run())
            }
            Sem::ValueIntegral(v) => {
                Ok(v)
            }
            Sem::ValueFractional(v ) => {
                Ok(v)
            }
            Sem::ValueString(v) => {
                Ok(v)
            }
            Sem::ValueUrl(v) => {
                Ok(v)
            }
            Sem::ValueBoolean(v) => {
                Ok(v)
            }
            Sem::ValueTime(v) => {
                Ok(v)
            }
            Sem::Variable(id, sem) => {
                self.eval_sem(sem, None)
            }
            Sem::ValueList(sem_vec) => {
                if sem_vec.len() == 0 {
                    return Err(EvaluationError::new("List cannot be empty"));
                }
                let mut vals = vec!();
                let mut type_opt : Option<Type> = None;
                for s in sem_vec {
                    match type_opt {
                        None => {
                            type_opt = Some(s.get_type().clone());
                        }
                        Some(_) => {}
                    }
                    match self.eval_sem(Box::new(s), None) {
                        Ok(v) => {
                            vals.push(v);
                        }
                        err @ Err(_) => {
                            return err;
                        }
                    }
                }
                Ok(Value::ValueList {
                    item_type: type_opt.unwrap(),
                    vals
                })
            }
        }
    }
}

#[derive(Debug)]
pub struct EvaluationError {
    message : String
}

impl EvaluationError {
    pub fn new(message : &str) -> EvaluationError {
        EvaluationError { message : String::from(message) }
    }
}

impl Display for EvaluationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.message.as_str())
    }
}

impl Error for EvaluationError { }
