use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter, Write};
use crate::external_resources::{BuiltInResources, IOResource};
use crate::parser::Expr::{Pipeline, Setter};
use crate::tokens::{EnumTypedVariant, Token, TokenType};
use crate::types::Type;
use crate::values::Value;

/// ## Grammar
///
/// ```text
/// EXECUTABLE  := SETTER (';' SETTER)+
/// SETTER      := identifier '<-' PIPELINE | PIPELINE ('->' identifier)?
/// PIPELINE    := FN_RESOURCE (pipe FN_RESOURCE)+
/// FN_RESOURCE := EXPR | resource
/// EXPR        := EQUALITY
/// EQUALITY    := COMPARISON ( EQUALITY_OP COMPARISON)?
/// COMPARISON  := SUM (BOOLEAN_OP SUM)?
/// SUM         := PRODUCT (('+' | '-') PRODUCT)*
/// PRODUCT     := UNARY (('\*' | '/') UNARY)*
/// UNARY       := ('-' FNCALL) | FNCALL
/// FNCALL      := identifier (PROPERTY)* | PROPERTY
/// PROP_SET    := '{' PROPERTY (',' PROPERTY)* '}' | PROPERTY
/// PROPERTY    := identifier ':' VALUE | VALUE
/// VALUE       := scalar | variable | resource | '[' EXPR (',' EXPR)* ']' | '(' EXPR ')'
///```

pub struct Parser {
    tkns : Vec<Token>,
    idx : usize,
}

impl Parser {
    pub fn parse(tkns : Vec<Token>) -> Result<Command, ParserError> {
        let mut parser = Parser {
            tkns,
            idx : 0,
        };
        parser.parse_command()
    }

    fn parse_command(&mut self) -> Result<Command, ParserError> {
        let exprs = self.parse_executable()?;
        Ok(Command {
            id: None,
            exprs
        })
    }

    fn parse_executable(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut exprs = vec![];
        loop {
            match self.parse_setter() {
                Ok(expr) => {
                    exprs.push(expr);
                    if self.check_current(&Token::EndOfStatement) {
                        self.pop();
                    } else if self.peek() != None {
                        return Err(self.unexpected_token_error("parse_executable"));
                    }
                },
                Err(e) => {
                    return Err(e);
                }
            }
            if !self.has_more() {
                break;
            }
        }
        Ok(exprs)
    }


    fn parse_setter(&mut self) -> Result<Expr, ParserError> {
        return if let Some(id) = self.check_identifier() {
            let cloned_id = id.clone();
            if self.check_next(&Token::LeftSetter) {
                self.pop(); // pop identifier
                self.pop(); // pop left setter
                match self.parse_pipeline() {
                    Ok(expr) => {
                        Ok(Setter(cloned_id, Box::new(expr)))
                    }
                    Err(_) => {
                        Err(ParserError::new("parse_pipeline returned nothing 1"))
                    }
                }
            } else {
                self.parse_right_setter()
            }
        } else {
            self.parse_right_setter()
        }
    }

    fn parse_right_setter(&mut self) -> Result<Expr, ParserError> {
        match self.parse_pipeline() {
            Ok(pipeline) => {
                if self.check_current(&Token::RightSetter) {
                    self.pop(); // pop right setter
                    if let Some(id) = self.check_identifier() {
                        let local_id = id.clone();
                        self.pop(); // pop identifier
                        Ok(Setter(local_id, Box::new(pipeline)))
                    } else {
                        Err(ParserError::new("Expected identifer following setter '->'"))
                    }
                } else {
                    Ok(pipeline)
                }
            }
            Err(err) => {
                Err(err)
            }
        }
    }

    fn parse_pipeline(&mut self) -> Result<Expr, ParserError> {
        match self.parse_expr() {
            Ok(expr) => {
                if let Some(tkn) = self.peek() {
                    if tkn.has_type(&TokenType::Pipe) {
                        let local_tkn = tkn.clone();
                        self.pop();
                        Ok(
                            Pipeline(
                                Box::new(expr),
                                local_tkn,
                                Box::new(self.parse_pipeline()?))
                        )
                    } else {
                        Ok(expr)
                    }
                } else {
                    Ok(expr)
                }
            }
            Err(err) => {
                Err(err)
            }
        }
    }

    fn parse_fn_resource(&mut self) -> Result<Expr, ParserError> {
        if let Some(tkn) = self.peek() {
            if tkn.has_type(&TokenType::Resource) {
                if let Expr::ValueResource(res) = self.match_resource()? {
                    return Ok(Expr::FnResource(res));
                } else {
                    return Err(self.unexpected_token_error("in parse_fn_resource"));
                }
            }
        }
        self.parse_expr()
    }

    fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Result<Expr, ParserError> {
        let left_expr = self.parse_comparison()?;
        if let Some(tkn) = self.peek() {
            if tkn == &Token::Equals || tkn == &Token::NotEquals {
                let local_tkn = tkn.clone();
                self.pop();
                let right_expr = self.parse_comparison()?;
                Ok(Expr::Binary(
                    local_tkn,
                    Box::new(left_expr),
                    Box::new(right_expr)))
            } else {
                Ok(left_expr)
            }
        } else {
            Ok(left_expr)
        }
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParserError> {
        let left_expr = self.parse_sum()?;
        if let Some(tkn) = self.peek() {
            if tkn.has_type(&TokenType::BooleanOp) {
                let local_tkn = tkn.clone();
                self.pop();
                let right_expr = self.parse_sum()?;
                Ok(Expr::Binary(
                    local_tkn,
                    Box::new(left_expr),
                    Box::new(right_expr)))
            } else {
                Ok(left_expr)
            }
        } else {
            Ok(left_expr)
        }
    }

    fn parse_sum(&mut self) -> Result<Expr, ParserError> {
        let left_expr = self.parse_product()?;
        if let Some(tkn) = self.peek() {
            if tkn == &Token::Plus || tkn == &Token::Minus {
                let local_tkn = tkn.clone();
                self.pop();
                let right_expr = self.parse_sum()?;
                Ok(Expr::Binary(
                    local_tkn,
                    Box::new(left_expr),
                    Box::new(right_expr)))
            } else {
                Ok(left_expr)
            }
        } else {
            Ok(left_expr)
        }
    }

    fn parse_product(&mut self) -> Result<Expr, ParserError> {
        let left_expr = self.parse_unary()?;
        if let Some(tkn) = self.peek() {
            if tkn == &Token::Multiply || tkn == &Token::Divide {
                let local_tkn = tkn.clone();
                self.pop();
                let right_expr = self.parse_product()?;
                Ok(Expr::Binary(
                    local_tkn,
                    Box::new(left_expr),
                    Box::new(right_expr)))
            } else {
                Ok(left_expr)
            }
        } else {
            Ok(left_expr)
        }
    }

    fn parse_unary(&mut self) -> Result<Expr, ParserError> {
        if let Some(tkn) = self.peek() {
            return if tkn.has_type(&TokenType::UnaryOp) {
                let local_tkn = tkn.clone();
                self.pop();
                let result = self.parse_fncall();
                match result {
                    Ok(expr) => {
                        Ok(Expr::Unary(local_tkn, Box::new(expr)))
                    }
                    Err(err) => {
                        Err(err)
                    }
                }
            } else {
                self.parse_fncall()
            }
        }
        Err(ParserError::new("todo error message: parse_unary"))
    }

    fn parse_fncall(&mut self) -> Result<Expr, ParserError> {
        if let Some(tkn) = self.peek() {
            let local_tkn = tkn.clone();
            return match local_tkn {
                Token::Identifier(id) => {
                    if !self.check_next(&Token::Colon) {
                        self.pop(); // pop identifier
                        let mut args = vec![];
                        while let Ok(expr) = self.parse_propset() {
                            args.push(expr);
                        }
                        Ok(Expr::FnCall(id, args))
                    } else {
                        self.parse_propset()
                    }
                }
                _ => {
                    self.parse_propset()
                }
            }
        }
        Err(ParserError::new("todo error message: parse_fncall"))
    }

    fn parse_propset(&mut self) -> Result<Expr, ParserError> {
        if self.check_current(&Token::LeftCurleyBrace) {
            self.pop();
            let prop = self.parse_property()?;
            let mut prop_vec = vec![prop];
            while self.check_current(&Token::Comma) {
                self.pop();
                prop_vec.push(self.parse_property()?);
            }

            if self.check_current(&Token::RightCurleyBrace) {
                self.pop();
            } else {
                return Err(ParserError::new("Expected right curley-brace to close property set"))
            }

            Ok(Expr::ValuePropertySet(prop_vec))
        } else {
            self.parse_property()
        }
    }

    fn parse_property(&mut self) -> Result<Expr, ParserError> {
        if let Some(tkn) = self.peek() {
            let local_tkn = tkn.clone();
            return match local_tkn {
                Token::Identifier(id) => {
                    if self.check_next(&Token::Colon) {
                        self.pop(); // pop identifier
                        self.pop(); // pop colon
                        if let Ok(expr) = self.parse_property() {
                            Ok(Expr::ValueProperty(id, Box::new(expr)))
                        } else {
                            Err(ParserError::new(format!(
                                "Expression expected in property definition for property [{}]", id)
                                .as_str()))
                        }
                    } else {
                        self.parse_property()
                    }
                }
                _ => {
                    self.parse_value()
                }
            }
        }
        Err(ParserError::new("todo error message: parse_property"))
    }

    fn parse_value(&mut self) -> Result<Expr, ParserError> {
        if let Some(tkn) = self.peek() {
            let local_tkn = tkn.clone();
            if local_tkn == Token::LeftParens {
                self.pop();
                let expr_result = self.parse_expr();
                return match self.peek() {
                    None => {
                        Err(ParserError::new("Expected closing parenthesis"))
                    }
                    Some(right_parens) => {
                        if right_parens == &Token::RightParens {
                            self.pop(); // discard closing parenthesis and return expression
                            expr_result
                        } else {
                            Err(self.unexpected_token_error("expected closing parenthesis"))
                        }
                    }
                }
            } else if local_tkn.has_type(&TokenType::Resource) {
                return self.match_resource();
            } else if local_tkn.has_type(&TokenType::Variable) {
                return match self.pop() {
                    Token::Variable(id) => {
                        Ok(Expr::VariableReference(id))
                    }
                    _ => {
                        Err(self.unexpected_token_error(
                            "Impossible! already tested for Token::Variable, but not a Token::Variable!"))
                    }
                }
            } else if local_tkn == Token::LeftSquareBracket {
                self.pop();
                return if let Ok(expr) = self.parse_expr() {
                    let mut exprs = vec!(expr);
                    while self.check_current(&Token::Comma) {
                        self.pop(); // pop comma
                        match self.parse_expr() {
                            Ok(expr) => {
                                exprs.push(expr);
                            }
                            err @ Err(..) => {
                                return err;
                            }
                        }
                    }
                    if self.check_current(&Token::RightSquareBracket) {
                        self.pop();
                        Ok(Expr::ValueList(exprs))
                    } else {
                        Err(self.unexpected_token_error("Expected a closing bracket after list"))
                    }
                } else if self.check_current(&Token::RightSquareBracket) {
                    Err(ParserError::new("List cannot be empty"))
                } else {
                    Err(self.unexpected_token_error("Expected list value"))
                }
            } else if local_tkn.has_type(&TokenType::Value) {
                self.pop();
                return match local_tkn {
                    Token::Integral(i) => {
                        Ok(Expr::ValueIntegral(i))
                    }
                    Token::Fractional(v) => {
                        Ok(Expr::ValueFractional(v))
                    }
                    Token::String(v) => {
                        Ok(Expr::ValueString(v))
                    }
                    Token::Boolean(v) => {
                        Ok(Expr::ValueBoolean(v))
                    }
                    Token::TypeLiteral(t) => {
                        Ok(Expr::ValueType(t))
                    }
                    _ => {
                        Err(ParserError::new("unsupported value type!"))
                    }
                }
            }
        }
        Err(ParserError::new("end of parse_value with no result"))
    }

    /// make sure current token is a resource token before calling
    fn match_resource(&mut self) -> Result<Expr, ParserError>  {
        return match self.pop() {
            Token::HttpResource(url) => {
                match BuiltInResources::http_resource_type()
                    .new_instance(url.as_str(), HashMap::default()) {
                    Ok(res) => {
                        Ok(Expr::ValueResource(res))
                    }
                    Err(err) => {
                        Err(ParserError::new(err.to_string().as_str()))
                    }
                }
            }
            Token::FileResource(path) => {
                match BuiltInResources::file_resource_type()
                    .new_instance(path.as_str(), HashMap::default()) {
                    Ok(res) => {
                        Ok(Expr::ValueResource(res))
                    }
                    Err(err) => {
                        Err(ParserError::new(err.to_string().as_str()))
                    }
                }
            }
            _ => {
                Err(self.unexpected_token_error(
                    "Impossible! already tested for TokenType::Resource, but not a TokenType::Resource!"))
            }
        }
    }

    fn has_more(&self) -> bool {
        self.idx < self.tkns.len()
    }

    fn pop(&mut self) -> Token {
        if let Some(next) = self.peek() {
            let local_token = next.clone();
            self.idx += 1;
            return local_token;
        }
        panic!("Always check if peek exists before calling pop()!")
    }

    fn check_current(&self, tkn : &Token) -> bool {
        if let Some(next) = self.peek() {
            if next == tkn {
                return true;
            }
        }
        false
    }

    fn check_next(&self, tkn : &Token) -> bool {
        if let Some(next) = self.lookahead() {
            if next == tkn {
                return true;
            }
        }
        false
    }

    fn check_identifier(&self) -> Option<&String> {
        if let Some(Token::Identifier(val)) = self.peek() {
            Some(val)
        } else {
            None
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tkns.get(self.idx)
    }

    fn lookahead(&self) -> Option<&Token> {
        self.tkns.get(self.idx + 1)
    }

    fn unexpected_token_error(&self, marker : &str) -> ParserError {
        return if let Some(tkn) = self.peek() {
            ParserError::new(format!("Unexpected token [{}] at [{}]", tkn, marker).as_str())
        } else {
            ParserError::new("Unexpected end of input")
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Command {
    pub id : Option<String>,
    pub exprs : Vec<Expr>,
}

impl Display for Command {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut result_str = String::new();
        if let Some(id) = &self.id {
            result_str.push_str(format!("{} :: ", id).as_str());
        }
        for expr in &self.exprs {
            result_str.push_str(format!("{};", expr).as_str())
        }
        f.write_str(result_str.as_str())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Setter(String, Box<Expr>),
    Pipeline(Box<Expr>, Token, Box<Expr>),
    FnCall(String, Vec<Expr>),
    FnResource(IOResource),
    Binary(Token, Box<Expr>, Box<Expr>),
    Unary(Token, Box<Expr>),
    VariableReference(String),
    ValueIntegral(Value),
    ValueFractional(Value),
    ValueString(Value),
    ValueBoolean(Value),
    ValueList(Vec<Expr>),
    ValueProperty(String, Box<Expr>),
    ValuePropertySet(Vec<Expr>),
    ValueResource(IOResource),
    ValueType(Type),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Setter(id, expr) => {
                f.write_str(format!("{} <- {}", id, expr).as_str())
            }
            Expr::Pipeline(l_expr, token, r_expr) => {
                f.write_str(format!("{} {} {}", l_expr, token, r_expr).as_str())
            }
            Expr::FnCall(fname, args) => {
                let mut arg_str = String::new();
                for arg in args {
                    arg_str.push('(');
                    arg_str.push_str(arg.to_string().as_str());
                    arg_str.push(')');
                    arg_str.push(' ');
                }
                f.write_str(format!("{}:{}", fname, arg_str).as_str())
            }
            Expr::Binary(op, left, right) => {
                f.write_str(format!("({}) {} ({})", left, op, right).as_str())
            }
            Expr::Unary(op, expr) => {
                f.write_str(format!("{}({})", op, expr).as_str())
            }
            Expr::VariableReference(id) => {
                f.write_str(format!("${}", id).as_str())
            }
            Expr::ValueIntegral(v) => {
                f.write_str(v.to_string().as_str())
            }
            Expr::ValueString(v) => {
                f.write_str(v.to_string().as_str())
            }
            Expr::ValueFractional(v) => {
                f.write_str(v.to_string().as_str())
            }
            Expr::ValueBoolean(v) => {
                f.write_str(v.to_string().as_str())
            }
            Expr::ValueList(exprs) => {
                let mut buf = String::new();
                buf.push('[');
                let mut first = true;
                for expr in exprs {
                    if first {
                        first = false;
                    } else {
                        buf.push(',');
                    }
                    buf.push_str(expr.to_string().as_str());
                }
                buf.push(']');
                f.write_str(buf.as_str())
            }
            Expr::ValueProperty(prop_name, prop_val) => {
                f.write_str(format!("(\"{}\": {})", prop_name, prop_val).as_str())
            }
            Expr::ValuePropertySet(props) => {
                let mut buf = String::new();
                buf.push('{');
                let mut first = true;
                for prop in props {
                    if first {
                        first = false;
                    } else {
                        buf.push(',');
                    }
                    buf.push_str(prop.to_string().as_str());
                }
                buf.push('}');
                f.write_str(buf.as_str())
            }
            Expr::ValueResource(res) => {
                f.write_str(res.to_string().as_str())
            }
            Expr::FnResource(res) => {
                f.write_str(format!("fn({})", res.to_string()).as_str())
            }
            Expr::ValueType(t) => {
                f.write_str(format!("'{}", t.to_string()).as_str())
            }
        }
    }
}

#[derive(Debug)]
pub struct ParserError {
    message : String,
}

impl ParserError {
    pub fn new(message : &str) -> ParserError {
        ParserError {
            message : String::from(message)
        }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.message.as_str())
    }
}

impl Error for ParserError { }
