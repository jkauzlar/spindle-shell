use std::error::Error;
use std::fmt::{Display, Formatter};
use std::str::FromStr;
use bigdecimal::{BigDecimal, ParseBigDecimalError};
use num_bigint::{BigInt, ParseBigIntError};
use crate::types::Type;
use crate::Value;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    String(Value),          // ('"'.*'"') | '---'(.*)
    Boolean(Value),         // 'true' | 'false'
    Integral(Value),        // ([0-9]+)
    Fractional(Value),      // ([0-9]+'.'[0-9]*')
    Time(Value),            // TODO
    EndOfStatement,         // ';'
    FileResource(String),   // @[^ ]+
    HttpResource(String),   // @http(s)?://[^ ]+
    LeftParens,             // '('
    RightParens,            // ')'
    ThreePeriods,           // '...'
    Identifier(String),     // ((\.)?[A-Za-z0-9_]+)+
    Equals,                 // '=='
    NotEquals,              // '!='
    GreaterThan,            // '>'
    LessThan,               // '<'
    GreaterThanEquals,      // '>='
    LessThanEquals,         // '<='
    Plus,                   // '+'
    Minus,                  // '-'
    Multiply,               // '*'
    Divide,                 // '/'
    Pipe,                   // '|'
    PullPipe,               // '|>'
    PushPipe,               // '<|'
    StreamPipe,             // '<|>'
    LeftSetter,             // '<-'
    RightSetter,            // '->'
    LeftSquareBracket,      // '['
    RightSquareBracket,     // ']'
    LeftCurleyBrace,        // '{'
    RightCurleyBrace,       // '}'
    Comma,                  // ','
    Colon,                  // ':'
    QuestionMark,           // '?'
    Variable(String),       // '$'(Identifier)
    TypeLiteral(Type),            // '\'' until type has been fully consumed
}

impl Token {
    pub fn new_string(str_val : String) -> Token {
        Token::String(Value::ValueString { val: str_val } )
    }

    pub fn new_integral(str_val : String) -> Result<Token, ParseBigIntError>  {
        match BigInt::from_str(str_val.as_str()) {
            Ok(big_int) => {
                Ok(Token::Integral(Value::ValueIntegral { val : big_int }))
            }
            Err(err) => {
                Err(err)
            }
        }
    }

    pub fn new_fractional(str_val : String) -> Result<Token, ParseBigDecimalError> {
        match BigDecimal::from_str(str_val.as_str()) {
            Ok(big_dec) => {
                Ok(Token::Fractional(Value::ValueFractional { val : big_dec } ))
            }
            Err(err) => {
                Err(err)
            }
        }
    }

    pub fn new_boolean(b : bool) -> Token {
        Token::Boolean(Value::ValueBoolean { val : b } )
    }


    pub(crate) fn get_string_rep(&self) -> String {
        match self {
            Token::String(val) => { val.to_string() }
            Token::Integral(val) => { val.to_string()}
            Token::Fractional(val) => { val.to_string()}
            Token::Boolean(val) => { val.to_string()}
            Token::Time(val ) => { val.to_string()}
            Token::EndOfStatement => { String::from(";") }
            Token::LeftParens => { String::from("(")}
            Token::RightParens => { String::from(")")}
            Token::Identifier(val) => { format!("Identifier ({})", val.as_str()) }
            Token::Equals => { String::from("==") }
            Token::NotEquals => { String::from("!=") }
            Token::GreaterThan => { String::from(">")}
            Token::LessThan => { String::from("<")}
            Token::GreaterThanEquals => { String::from(">=")}
            Token::LessThanEquals => { String::from("<=")}
            Token::Plus => { String::from("+")}
            Token::Minus => { String::from("-")}
            Token::Multiply => { String::from("*")}
            Token::Divide => {String::from("/")}
            Token::Colon => {String::from(":")}
            Token::Pipe => {String::from("|")}
            Token::LeftSquareBracket => { String::from("[") }
            Token::RightSquareBracket => { String::from("]") }
            Token::LeftCurleyBrace => { String::from("{") }
            Token::RightCurleyBrace => { String::from("}")}
            Token::Comma => { String::from(",") }
            Token::PullPipe => {String::from("|>")}
            Token::PushPipe => { String::from("<|")}
            Token::StreamPipe => {String::from("<|>")}
            Token::LeftSetter => {String::from("<-")}
            Token::RightSetter => { String::from("->")}
            Token::Variable(val) => {format!("${}", val.as_str()) }
            Token::ThreePeriods => { String::from("...")}
            Token::FileResource(val) => { format!("@{}", val)}
            Token::HttpResource(val) => { format!("@{}", val)}
            Token::QuestionMark => { String::from("?")}
            Token::TypeLiteral(t) => { format!("'{}", t)}
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.get_string_rep().as_str())
    }
}

#[derive(Eq, PartialEq)]
pub enum TokenType {
    Value,
    Variable,
    Identifer,
    Pipe,
    UnaryOp,
    BinaryOp,
    BooleanOp,
    Resource,
    Other,
}

pub trait EnumTypedVariant<A>
    where A : PartialEq + Eq {
    fn get_type(&self) -> Vec<A>;
    fn has_type(&self, t : &A) -> bool {
        self.get_type().contains(t)
    }
}

impl EnumTypedVariant<TokenType> for Token {
    fn get_type(&self) -> Vec<TokenType> {
        match self {
            Token::String(_) => { vec![TokenType::Value]}
            Token::Integral(_) => { vec![TokenType::Value]}
            Token::Fractional(_) => { vec![TokenType::Value]}
            Token::Boolean(_) => { vec![TokenType::Value]}
            Token::Time(_) => { vec![TokenType::Value]}
            Token::TypeLiteral(_) => { vec![TokenType::Value] }
            Token::Identifier(_) => { vec![TokenType::Identifer]}
            Token::Variable(_) => { vec![TokenType::Variable]}
            Token::Equals => {vec![TokenType::BooleanOp]}
            Token::NotEquals => {vec![TokenType::BooleanOp]}
            Token::GreaterThan => {vec![TokenType::BooleanOp]}
            Token::LessThan => {vec![TokenType::BooleanOp]}
            Token::GreaterThanEquals => {vec![TokenType::BooleanOp]}
            Token::LessThanEquals => {vec![TokenType::BooleanOp]}
            Token::Plus => {vec![TokenType::BinaryOp]}
            Token::Minus => {vec![TokenType::BinaryOp, TokenType::UnaryOp]}
            Token::Multiply => {vec![TokenType::BinaryOp]}
            Token::Divide => {vec![TokenType::BinaryOp]}
            Token::Pipe => {vec![TokenType::Pipe]}
            Token::PullPipe => {vec![TokenType::Pipe]}
            Token::PushPipe => {vec![TokenType::Pipe]}
            Token::StreamPipe => {vec![TokenType::Pipe]}
            Token::HttpResource(_) => {vec![TokenType::Resource]}
            Token::FileResource(_) => {vec![TokenType::Resource]}
            _ => vec![TokenType::Other]
        }
    }
}


#[derive(Debug)]
pub struct ScannerError {
    message : String,
}

impl ScannerError {
    pub fn new(message : &str) -> ScannerError {
        ScannerError {
            message : String::from(message)
        }
    }
}

impl Display for ScannerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.message.as_str());
        Result::Ok(())
    }
}

impl Error for ScannerError { }
