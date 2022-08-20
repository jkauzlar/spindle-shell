use std::fmt::{Display, Formatter};
use crate::environment::Environment;
use crate::parser::{Expr};
use crate::tokens::{Token, EnumTypedVariant, TokenType};
use crate::types::{Function, Type};
use crate::values::{Value};

pub struct SemanticAnalyzer<'a> {
    env : &'a Box<Environment>
}

impl SemanticAnalyzer<'_> {
    pub fn analyze(env : &Box<Environment>, expr : Expr) -> Result<SemanticExpression, TypeError> {
        let mut semantic_analyzer = SemanticAnalyzer { env };
        semantic_analyzer.create_sem_tree(Box::new(expr), None)
    }

    fn create_sem_tree(&mut self, expr : Box<Expr>, carry_type : Option<Type>) -> Result<SemanticExpression, TypeError> {
        match *expr {
            Expr::Setter(id, expr) => {
                match self.create_sem_tree(expr, carry_type) {
                    Ok(sem_expr) => {
                        Ok(SemanticExpression::Setter(
                            id,
                            Box::new(sem_expr)))
                    }
                    Err(err) => {
                        Err(err)
                    }
                }
            }
            Expr::Pipeline(left_expr, pipe, right_expr) => {
                let left_sem = self.analyze_expr(&left_expr, carry_type)?;
                let sem_pipe = SemanticAnalyzer::translate_pipe(pipe);
                match self.create_sem_tree(right_expr, Some(left_sem.get_type())) {
                    Ok(right_sem) => {
                        Ok(SemanticExpression::PipedCommand {
                            sem: left_sem,
                            pipe: Some((sem_pipe, Box::new(right_sem))),
                        })
                    }
                    Err(err) => {
                        Err(err)
                    }
                }
            }
            _ => {
                match self.analyze_expr(&expr, carry_type) {
                    Ok(sem) => {
                        Ok(SemanticExpression::PipedCommand {
                            sem,
                            pipe: None,
                        })
                    }
                    Err(err) => {
                        Err(err)
                    }
                }
            }
        }
    }

    fn analyze_expr(&mut self, expr : &Expr, carry_type : Option<Type>) -> Result<Sem, TypeError> {
        match expr {
            Expr::FnCall(fn_name, args) => {
                let mut sem_args = vec![];
                for arg in args {
                    sem_args.push(self.analyze_expr(arg, None)?);
                }
                match self.resolve_fn(fn_name, &sem_args, carry_type) {
                    None => {
                        Err(TypeError::new("todo type error"))
                    }
                    Some(f) => {
                        Ok(Sem::FnCall(f, sem_args))
                    }
                }
            }
            Expr::Binary(op, left, right) => {
                if let Some(_) = carry_type {
                    return Err(TypeError::new("Illegal attempt to pipe into a non-function"));
                }
                let left_arg = self.analyze_expr(left, None)?;
                let right_arg = self.analyze_expr(right, None)?;
                match self.resolve_binary_fn(op, &left_arg, &right_arg) {
                    None => {
                        Err(TypeError::new("todo type error"))
                    }
                    Some(f) => {
                        Ok(Sem::FnCall(f, vec![left_arg, right_arg]))
                    }
                }
            }
            Expr::Unary(op, expr) => {
                if let Some(_) = carry_type {
                    return Err(TypeError::new("Illegal attempt to pipe into a non-function"));
                }
                let arg = self.analyze_expr(expr, None)?;
                match self.resolve_unary_fn(op, &arg) {
                    None => {
                        Err(TypeError::new("todo type error"))
                    }
                    Some(f) => {
                        Ok(Sem::FnCall(f, vec![arg]))
                    }
                }
            }
            Expr::VariableReference(id) => {
                if let Some(_) = carry_type {
                    return Err(TypeError::new("Illegal attempt to pipe into a non-function"));
                }
                match self.resolve_var(id) {
                    None => {
                        Err(TypeError::new("todo type error"))
                    }
                    Some(sem_value) => {
                        Ok(Sem::Variable(id.clone(), Box::new(sem_value)))
                    }
                }
            }
            Expr::ValueIntegral(v) => {
                if let Some(_) = carry_type {
                    return Err(TypeError::new("Illegal attempt to pipe into a non-function"));
                }
                Ok(Sem::ValueIntegral(v.clone()))
            }
            Expr::ValueFractional(v) => {
                if let Some(_) = carry_type {
                    return Err(TypeError::new("Illegal attempt to pipe into a non-function"));
                }
                Ok(Sem::ValueFractional(v.clone()))
            }
            Expr::ValueString(v) => {
                if let Some(_) = carry_type {
                    return Err(TypeError::new("Illegal attempt to pipe into a non-function"));
                }
                Ok(Sem::ValueString(v.clone()))
            }
            Expr::ValueBoolean(v) => {
                if let Some(_) = carry_type {
                    return Err(TypeError::new("Illegal attempt to pipe into a non-function"));
                }
                Ok(Sem::ValueBoolean(v.clone()))
            }
            Expr::ValueUrl(v) => {
                if let Some(_) = carry_type {
                    return Err(TypeError::new("Illegal attempt to pipe into a non-function"));
                }
                Ok(Sem::ValueUrl(v.clone()))
            }
            _ => {  Err(TypeError::new(""))}
        }
    }

    pub fn translate_pipe(pipe_tkn : Token) -> Pipe {
        match pipe_tkn {
            Token::Pipe => Pipe::Value,
            Token::PushPipe => Pipe::Push,
            Token::PullPipe => Pipe::Pull,
            Token::StreamPipe => Pipe::Stream,
            _ => panic!("Encountered a non-pipe token while translating pipes!")
        }
    }

    fn resolve_fn(&self, fn_name: &String, args: &Vec<Sem>, carry_type : Option<Type>) -> Option<Function> {
        let mut type_vec : Vec<Type> = args.iter().map(|sem| sem.get_type()).collect();
        if let Some(t) = carry_type {
            type_vec.push(t);
        }
        self.env.find_function(
            fn_name,
            type_vec)
    }

    fn resolve_var(&self, var_name : &String) -> Option<Sem> {
        self.env.resolve_value(var_name)
    }

    fn resolve_binary_fn(&self, op : &Token, left_arg : &Sem, right_arg : &Sem) -> Option<Function> {
        if op.has_type(&TokenType::BinaryOp) || op.has_type(&TokenType::BooleanOp) {
            self.env.find_function(&op.get_string_rep(),
                                   vec![left_arg.get_type(), right_arg.get_type()])
        } else {
            panic!("Encountered a non-binary token while analyzing binary operations!");
        }
    }

    fn resolve_unary_fn(&self, op : &Token, arg : &Sem) -> Option<Function> {
        if op.has_type(&TokenType::UnaryOp) {
            self.env.find_function(&op.get_string_rep(), vec![arg.get_type()])
        } else {
            panic!("Encountered a non-unary token while analyzing unary operations!");
        }
    }

}

pub enum SemanticExpression {
    PipedCommand {
        sem : Sem,
        pipe : Option<(Pipe, Box<SemanticExpression>)>,
    },
    Setter(String, Box<SemanticExpression>)
}

impl Display for SemanticExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SemanticExpression::PipedCommand { sem, pipe } => {
                match pipe {
                    None => {
                        f.write_str(sem.to_string().as_str())
                    }
                    Some((pipe_type, right_expr)) => {
                        f.write_str(
                            format!("{} {} {}",
                                    sem.to_string(),
                                    pipe_type.to_string(),
                                    right_expr.to_string()).as_str())
                    }
                }
            }
            SemanticExpression::Setter(var_name, expr) => {
                f.write_str(format!("{} <- {}", var_name, expr.to_string()).as_str())
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Pipe {
    Value,
    Push,
    Pull,
    Stream,
}


impl Display for Pipe {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Pipe::Value => { f.write_str("|") }
            Pipe::Push => { f.write_str("<|")}
            Pipe::Pull => { f.write_str("|>")}
            Pipe::Stream => { f.write_str("<|>")}
        }
    }
}

pub enum Sem {
    FnCall(Function, Vec<Sem>),
    ValueIntegral(Value),
    ValueFractional(Value),
    ValueString(Value),
    ValueUrl(Value),
    ValueBoolean(Value),
    ValueTime(Value),
    Variable(String, Box<Sem>),
}

impl Display for Sem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Sem::FnCall(func, args) => {
                let mut args_strs = vec![];
                for arg in args {
                    args_strs.push(arg.to_string());
                }
                let args_str = args_strs.join(", ");

                f.write_str(format!("{} : [{}]", func.to_string(), args_str).as_str())
            }
            Sem::ValueIntegral(v) => {
                f.write_str(format!("({} : {})", v.to_string(), Type::Integral.to_string()).as_str())
            }
            Sem::ValueFractional(v) => {
                f.write_str(format!("({} : {})", v.to_string(), Type::Fractional.to_string()).as_str())
            }
            Sem::ValueString(v) => {
                f.write_str(format!("({} : {})", v.to_string(), Type::String.to_string()).as_str())
            }
            Sem::ValueUrl(v) => {
                f.write_str(format!("({} : {})", v.to_string(), Type::URL.to_string()).as_str())
            }
            Sem::ValueBoolean(v) => {
                f.write_str(format!("({} : {})", v.to_string(), Type::Boolean.to_string()).as_str())
            }
            Sem::ValueTime(v) => {
                f.write_str(format!("({} : {})", v.to_string(), Type::Time.to_string()).as_str())
            }
            Sem::Variable(name, sem) => {
                f.write_str(format!("(${} : {})", name, sem.get_type().to_string()).as_str())
            }
        }
    }
}

pub struct TypeError {
    message : String
}

impl TypeError {
    pub fn new(msg : &str) -> TypeError {
        TypeError {
            message : String::from(msg)
        }
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.message.as_str())
    }
}

pub trait Typed {
    fn get_type(&self) -> Type;
}

impl Typed for SemanticExpression {
    fn get_type(&self) -> Type {
        match self {
            SemanticExpression::PipedCommand { sem, pipe } => {
                match pipe {
                    None => { sem.get_type() }
                    Some((_, expr)) => {expr.get_type()}
                }
            }
            SemanticExpression::Setter(_, v) => {v.get_type()}
        }
    }
}

impl Typed for Sem {
    fn get_type(&self) -> Type {
        match self {
            Sem::FnCall(f, _) => { f.sig().value.clone()}
            Sem::ValueIntegral(_) => { Type::Integral}
            Sem::ValueFractional(_) => { Type::Fractional}
            Sem::ValueString(_) => { Type::String}
            Sem::ValueUrl(_) => { Type::URL }
            Sem::ValueBoolean(_) => { Type::Boolean }
            Sem::ValueTime(_) => { Type::Time}
            Sem::Variable(_, sem) => { sem.get_type().clone()}
        }
    }
}
