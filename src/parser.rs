use std::error::Error;
use std::fmt::{Display, Formatter, Write};
use crate::scanner::{Token};

/// ```
// INPUT       := (identifier '::')? EXECUTABLE
// EXECUTABLE  := SETTER (';' SETTER)+
// SETTER      := identifier '<-' PIPELINE | PIPELINE ('->' identifier)?
// PIPELINE    := COMMAND (pipe COMMAND)+
// COMMAND     := FNCALL | '(' EXPR ')'
// FNCALL      := identifier (EXPR)*
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
        let id = self.check_identifier();
        if let Some(_) = id {
           self.pop();
        }
        if let Ok(exprs) = self.parse_executable() {
            Ok(Command {
                id: Some(String::new()),
                exprs
            })
        } else {
            Err(ParserError::new(""))
        }
    }

    fn parse_executable(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut exprs = vec![];
        loop {
            match self.parse_setter() {
                Ok(expr) => {
                    exprs.push(expr);
                    if let Some(next) = self.peek() {
                        if next == &Token::EndOfStatement {
                            self.pop();
                        } else {
                            return Err(self.unexpected_token_error());
                        }
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
        Err(ParserError::new(""))
    }

    fn has_more(&self) -> bool {
        self.idx < self.tkns.len()
    }

    fn pop(&mut self) {
        self.idx = self.idx + 1;
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

    fn unexpected_token_error(&self) -> ParserError {
        return if let Some(tkn) = self.peek() {
            ParserError::new(format!("Unexpected token [{}]", tkn).as_str())
        } else {
            ParserError::new("Unexpected end of input")
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Command {
    id : Option<String>,
    exprs : Vec<Expr>,
}

impl Display for Command {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut result_str = String::new();
        if let Some(id) = &self.id {
            result_str.push_str(format!("{} :: ", id).as_str());
        }
        for expr in &self.exprs {
            result_str.push_str(format!("{};\n", expr).as_str())
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
    Value(Token),
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
                    arg_str.push_str(arg.to_string().as_str());
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
            Expr::Value(v) => {
                f.write_str(format!("{}", v).as_str())
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
        f.write_str(self.message.as_str());
        Result::Ok(())
    }
}

impl Error for ParserError { }
