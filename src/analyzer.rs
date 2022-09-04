use std::fmt::{Display, Formatter, Write};
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

                match sem_pipe {
                    Pipe::Value => {
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
                    Pipe::Push => {
                        // check that the outgoing type supports collection
                        Err(TypeError::new("Unsupported pipe"))
                    }
                    Pipe::Pull => {
                        // check that the incoming type is streamable
                        match left_sem {
                            Sem::FnCall(f, _) => {


                            }
                            _ => {

                            }
                        }

                        Err(TypeError::new("Unsupported pipe"))
                    }
                    Pipe::Stream => {
                        // check that the incoming type is streamable
                        // check that the outgoing type supports collection
                        Err(TypeError::new("Unsupported pipe"))
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

    fn apply_coercions(args : Vec<Sem>, coercions : Vec<Function>) -> Vec<Sem> {
        let mut coerced_args = vec![];
        let mut idx = 0;
        for arg in args {
            let coercion = &coercions[idx];

            if coercion.name().eq("id") {
                coerced_args.push(arg);
            } else {
                coerced_args.push(Sem::FnCall(coercion.clone(), vec![arg]));
            }

            idx = idx + 1;
        }

        coerced_args
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
                    Some(sem) => {
                        Ok(sem)
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
                    Some(sem) => {
                        Ok(sem)
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
                    Some(sem) => {
                        Ok(sem)
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
            Expr::ValueProperty(prop_name, prop_exr) => {
                if let Some(_) = carry_type {
                    return Err(TypeError::new("Illegal attempt to pipe into a non-function"));
                }
                let arg_sem = self.analyze_expr(prop_exr, None)?;
                Ok(Sem::ValueProperty(prop_name.clone(), Box::new(arg_sem)))
            }
            Expr::ValuePropertySet(props) => {
                if let Some(_) = carry_type {
                    return Err(TypeError::new("Illegal attempt to pipe into a non-function"));
                }
                let mut prop_sems = vec![];
                for prop in props {
                    let expr = self.analyze_expr(prop, None)?;
                    match expr {
                        p @ Sem::ValueProperty(_, _ ) => {
                            prop_sems.push(p);
                        }
                        _ => {
                            return Err(TypeError::new("Illegal attempt to insert non-property into property set"));
                        }
                    }
                }

                Ok(Sem::ValuePropertySet(prop_sems))
            }
            Expr::ValueList(exprs) => {
                if let Some(_) = carry_type {
                    return Err(TypeError::new("Illegal attempt to pipe into a non-function"));
                }
                if exprs.len() == 0 {
                    return Err(TypeError::new("List cannot be empty"));
                }
                let mut list_type : Option<Type> = None;
                let mut sems = vec![];
                for e in exprs {
                    match self.analyze_expr(e, None) {
                        Ok(sem) => {
                            match list_type.clone() {
                                None => {
                                    list_type = Some(sem.get_type().clone());
                                }
                                Some(t) => {
                                    if t != sem.get_type() {
                                        return Err(TypeError::new("List item expressions must have the same type"));
                                    }
                                }
                            }
                            sems.push(sem);
                        }
                        err @ Err(_) => {
                            return err;
                        }
                    }
                }
                return Ok(Sem::ValueList(sems));
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

    fn resolve_fn(&self, fn_name: &String, args: &Vec<Sem>, carry_type : Option<Type>) -> Option<Sem> {
        if let Some(t) = carry_type {
            let mut args_with_carry = vec![];
            for s in args {
                args_with_carry.push(s.clone());
            }
            args_with_carry.push(Sem::ValueCarry(t));
            self.env.find_function(fn_name, &args_with_carry)
        } else {
            self.env.find_function(fn_name, args)
        }
    }

    fn resolve_var(&self, var_name : &String) -> Option<Sem> {
        self.env.resolve_value(var_name)
    }

    fn resolve_binary_fn(&self, op : &Token, left_arg : &Sem, right_arg : &Sem) -> Option<Sem> {
        if op.has_type(&TokenType::BinaryOp) || op.has_type(&TokenType::BooleanOp) {
            self.env.find_function(&op.get_string_rep(),
                                   &vec![left_arg.clone(), right_arg.clone()])
        } else {
            panic!("Encountered a non-binary token while analyzing binary operations!");
        }
    }

    fn resolve_unary_fn(&self, op : &Token, arg : &Sem) -> Option<Sem> {
        if op.has_type(&TokenType::UnaryOp) {
            self.env.find_function(&op.get_string_rep(), &vec![arg.clone()])
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

#[derive(Clone)]
pub enum Sem {
    FnCall(Function, Vec<Sem>),
    ValueCarry(Type),
    ValueIntegral(Value),
    ValueFractional(Value),
    ValueString(Value),
    ValueUrl(Value),
    ValueBoolean(Value),
    ValueTime(Value),
    ValueList(Vec<Sem>),
    ValueProperty(String, Box<Sem>),
    ValuePropertySet(Vec<Sem>),
    Variable(String, Box<Sem>),
}

impl Sem {
    pub fn from_value(val : Value) -> Self {
        match val {
            Value::ValueString { .. } => {
                Sem::ValueString(val)
            }
            Value::ValueIntegral { .. } => {
                Sem::ValueIntegral(val)
            }
            Value::ValueFractional { .. } => {
                Sem::ValueFractional(val)
            }
            Value::ValueBoolean { .. } => {
                Sem::ValueBoolean(val)
            }
            Value::ValueTime { .. } => {
                Sem::ValueTime(val)
            }
            Value::ValueUrl { .. } => {
                Sem::ValueUrl(val)
            }
            Value::ValueList { item_type, vals } => {
                let mut sem_vec = vec!();
                for v in vals {
                    sem_vec.push(Sem::from_value(v));
                }
                Sem::ValueList(sem_vec)
            }
            Value::ValueProperty { name, val } => {
                let sem = Sem::from_value(*val.clone());
                Sem::ValueProperty(name, Box::new(sem))
            }
            Value::ValuePropertySet { vals } => {
                let mut sems = vec![];
                for val in vals {
                    sems.push(Sem::from_value(val));
                }

                Sem::ValuePropertySet(sems)
            }
        }
    }
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
            Sem::ValueList(sems) => {
                let mut sems_strs = vec![];
                for sem in sems {
                    sems_strs.push(sem.to_string());
                }
                let sems_str = sems_strs.join(", ");

                f.write_str(format!("[{}]", sems_str).as_str())
            }
            Sem::ValueProperty(id,sem) => {
                f.write_str(format!("Property({}:{})", id, sem.to_string()).as_str())
            }
            Sem::ValuePropertySet(props) => {
                let mut prop_strs = vec![];
                for prop in props {
                    prop_strs.push(prop.to_string());
                }
                let mut buf = String::new();
                buf.push_str("PropertySet(");
                buf.push_str(prop_strs.join(", ").as_str());
                buf.push(')');

                f.write_str(buf.as_str())

            }
            Sem::ValueCarry(t) => {
                f.write_str(format!("[Carry-over: {}]", t.to_string()).as_str())
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
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
            Sem::ValueList(sems) => {
                // a list cannot be created empty, therefore the unwrap is safe
                Type::List(Box::new(sems.get(0).unwrap().get_type().clone()))
            }
            Sem::ValueProperty(name, sem) => {
                Type::Property(name.clone(), Box::new(sem.get_type().clone())) }
            Sem::ValuePropertySet(props) => {
                Type::PropertySet(props.iter().map(|p| p.get_type()).collect())
            }
            Sem::ValueCarry(t) => {
                t.clone()
            }
        }
    }
}
