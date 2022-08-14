use std::error::Error;
use std::fmt::{Display, Formatter};
use crate::parser::Expr::{Pipeline, Setter};
use crate::scanner::{EnumTypedVariant, Token, TokenType};
use crate::values::{Value};

/// ```
// INPUT       := (identifier '::')? EXECUTABLE
// EXECUTABLE  := SETTER (';' SETTER)+
// SETTER      := identifier '<-' PIPELINE | PIPELINE ('->' identifier)?
// PIPELINE    := EXPR (pipe EXPR)+
// EXPR        := EQUALITY
// EQUALITY    := COMPARISON ( EQUALITY_OP COMPARISON)?
// COMPARISON  := SUM (BOOLEAN_OP SUM)?
// SUM         := PRODUCT (('+' | '-') PRODUCT)*
// PRODUCT     := UNARY (('\*' | '/') UNARY)*
// UNARY       := ('-' FNCALLINNER) | FNCALLINNER
// FNCALLINNER := identifier (VALUE)* | VALUE
// VALUE       := scalar | variable | '(' EXPR ')'````
// ```

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
        match self.check_identifier() {
            None => {
                if let Ok(exprs) = self.parse_executable() {
                    Ok(Command {
                        id: None,
                        exprs
                    })
                } else {
                    Err(ParserError::new("todo error message parse_input"))
                }
            }
            Some(id) => {
                let local_id = id.clone();
                if self.check_next(&Token::CommandSpecifier) {
                    self.pop(); // pop the identifier
                    self.pop(); // pop the command specifier
                    if let Ok(exprs) = self.parse_executable() {
                        Ok(Command {
                            id: Some(local_id),
                            exprs
                        })
                    } else {
                        Ok(Command {
                            id: Some(local_id),
                            exprs: vec![]
                        })
                    }
                } else {
                    Ok(Command {
                       id: Some(local_id),
                        exprs: self.parse_executable()?
                    })
                }
            }
        }
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


    // EXPR        := EQUALITY
    // EQUALITY    := COMPARISON ( EQUALITY_OP COMPARISON)?
    // COMPARISON  := SUM (BOOLEAN_OP SUM)?
    // SUM         := PRODUCT (('+' | '-') PRODUCT)*
    // PRODUCT     := UNARY (('\*' | '/') UNARY)*
    // UNARY       := ('-' FNCALLINNER) | FNCALLINNER
    // FNCALLINNER := identifier (VALUE)* | VALUE
    // VALUE       := scalar | variable | '(' EXPR ')'````

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
                let result = self.parse_fncall_inner();
                match result {
                    Ok(expr) => {
                        Ok(Expr::Unary(local_tkn, Box::new(expr)))
                    }
                    Err(err) => {
                        Err(err)
                    }
                }
            } else {
                self.parse_fncall_inner()
            }
        }
        Err(ParserError::new("todo error message: parse_unary"))
    }

    fn parse_fncall_inner(&mut self) -> Result<Expr, ParserError> {
        if let Some(tkn) = self.peek() {
            let local_tkn = tkn.clone();
            return match local_tkn {
                Token::Identifier(id) => {
                    self.pop(); // pop identifier
                    let mut args = vec![];
                    while let Ok(expr) = self.parse_value() {
                        args.push(expr);
                    }
                    Ok(Expr::FnCall(id, args))
                }
                _ => {
                    self.parse_value()
                }
            }
        }
        Err(ParserError::new("todo error message: parse_fncall_inner"))
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
            } else if local_tkn.has_type(&TokenType::Variable){
                return match self.pop() {
                    Token::Variable(id) => {
                        Ok(Expr::VariableIdentifier(id))
                    }
                    _ => { Err(self.unexpected_token_error(
                        "Impossible! already tested for Token::Variable, but not a Token::Variable!"))
                    }
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
                    Token::Url(v) => {
                        Ok(Expr::ValueUrl(v))
                    }
                    _ => {
                        Err(ParserError::new("unsupported value type!"))
                    }
                }
            }
        }
        return Err(ParserError::new("end of parse_value with no result"))
    }

    fn has_more(&self) -> bool {
        self.idx < self.tkns.len()
    }

    fn pop(&mut self) -> Token {
        if let Some(next) = self.peek() {
            let local_token = next.clone();
            self.idx = self.idx + 1;
            return local_token;
        }
        panic!("Always check if next exists before calling pop()!")
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
        self.tkns.get(self.idx).clone()
    }

    fn lookahead(&self) -> Option<&Token> {
        self.tkns.get(self.idx + 1).clone()
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
            result_str.push_str(format!("{};\r\n", expr).as_str())
        }
        f.write_str(result_str.as_str())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Setter(String, Box<Expr>),
    Pipeline(Box<Expr>, Token, Box<Expr>),
    FnCall(String, Vec<Expr>),
    Binary(Token, Box<Expr>, Box<Expr>),
    Unary(Token, Box<Expr>),
    VariableIdentifier(String),
    ValueIntegral(Value),
    ValueFractional(Value),
    ValueString(Value),
    ValueBoolean(Value),
    ValueUrl(Value),
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
                f.write_str(format!("{} ({})", op, expr).as_str())
            }
            Expr::VariableIdentifier(id) => {
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
            Expr::ValueUrl(v) => {
                f.write_str(v.to_string().as_str())
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
