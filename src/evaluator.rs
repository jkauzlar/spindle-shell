use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::analyzer::{Sem, SemanticExpression, Typed};
use crate::Environment;
use crate::types::{FunctionArgs, Type};
use crate::Value::{ValueProperty, ValuePropertySet};
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
                let left_val = self.eval_sem(&Box::new(sem), carry_val)?;
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
                Ok(ValueReader::read("\"\"", &Type::String).unwrap())
            }
        }
    }

    fn eval_sem(&mut self, sem : &Sem, carry_val : Option<Value>) -> Result<Value, EvaluationError> {
        let sem : Sem = sem.clone();
        match sem {
            Sem::Void => {
                Ok(Value::ValueVoid)
            }
            Sem::ValueType(t) => {
                Ok(Value::ValueTypeLiteral(t))
            }
            Sem::FnCall(func, res, args) => {
                let mut arg_vals : Vec<Value> = vec!();
                for arg in args {
                    arg_vals.push(self.eval_sem(&Box::new(arg.clone()), carry_val.clone())?)
                }
                let mut fcall = func.create_call(arg_vals);
                if let Some(res) = res {
                    fcall = fcall.with_resource(res)
                }
                fcall.run()
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
            Sem::ValueBoolean(v) => {
                Ok(v)
            }
            Sem::ValueTime(v) => {
                Ok(v)
            }
            Sem::Variable(id, sem) => {
                self.eval_sem(&sem, carry_val.clone())
            }
            Sem::ValueProperty(id, sem) => {
                let v = self.eval_sem(&sem, carry_val.clone())?;
                Ok(ValueProperty {
                    name: id,
                    val: Box::new(v),
                })
            }
            Sem::ValuePropertySet(sems) => {
                let mut vals = vec![];
                for s in sems {
                    vals.push(self.eval_sem(&Box::new(s), carry_val.clone())?);
                }
                Ok(ValuePropertySet { vals })
            }
            Sem::ValueList(sem_vec) => {
                if sem_vec.is_empty() {
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
                    match self.eval_sem(&Box::new(s.clone()), carry_val.clone()) {
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

            Sem::ValueCarry(_) => {
                match carry_val {
                    None => {
                        Err(EvaluationError::new("Carry-over value expected"))
                    }
                    Some(v) => {
                        Ok(v)
                    }
                }
            }
            Sem::ValueResource(_) => {
                // todo interpret resource as function
                Ok(Value::ValueString { val : String::from("")})
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
