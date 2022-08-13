use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;

use bigdecimal::{BigDecimal, ParseBigDecimalError};
use num_bigint::{BigInt, ParseBigIntError};
use reqwest::Url;

use crate::values::{Value};

pub struct Scanner {
    chars: Vec<char>,
    length : usize,
    pos : usize,
    tokens : Vec<Token>,
}

impl Scanner {

    pub fn scan(inp : &str) -> Result<Vec<Token>, ScannerError> {
        let mut scanner = Scanner {
            chars: inp.chars().collect(),
            length : inp.chars().count(),
            pos: 0,
            tokens: vec![]
        };
        match scanner.run() {
            Ok(_) => {
                Ok(scanner.tokens)
            }
            Err(err) => {
                Err(err)
            }
        }
    }


    fn run(&mut self) -> Result<(), ScannerError> {

        loop {
            if self.pos >= self.chars.len() {
                break;
            }
            let &c = self.chars.get(self.pos).unwrap();

            if Scanner::is_space(c) {
                self.read_while(Scanner::is_space);
            } else if c == '.' || Scanner::is_digit(c) {
                let mut digits = self.read_while(Scanner::is_digit);
                if self.read_if(|c| c == '.') {
                    let decimal_digits = self.read_while(Scanner::is_digit);
                    digits.push('.');
                    digits.push_str(decimal_digits.as_str());
                    match Token::new_fractional(digits) {
                        Ok(tkn) => {
                            self.push_token(tkn);
                        }
                        Err(err) => {
                            return Err(ScannerError::new(err.to_string().as_str()));
                        }
                    }
                } else {
                    match Token::new_integral(digits) {
                        Ok(tkn) => {
                            self.push_token(tkn);
                        }
                        Err(err) => {
                            return Err(ScannerError::new(err.to_string().as_str()));
                        }
                    }
                }
                if self.check_here(Scanner::is_alpha) {
                    return Err(ScannerError::new(
                        format!("Invalid character [{}] immediately following numeric scalar", c).as_str()));
                }
            } else if c == '"' {
                self.pop();
                if let Some(a_str) = self.read_until(true, false, |c| c == '"') {
                    self.pop(); // read the closing double-quote
                    self.push_token(Token::new_string(a_str));
                } else {
                    return Err(ScannerError::new(format!("Missing closing '\"'").as_str()));
                }
            } else if Scanner::is_alpha(c) || c == '_' {
                match self.read_identifier() {
                    Ok(id_str) => {
                        if id_str.eq(&String::from("true")) {
                            self.push_token(Token::new_boolean(true));
                        } else if id_str.eq(&String::from("false")) {
                            self.push_token(Token::new_boolean(false));
                        } else {
                            self.push_token(Token::Identifier(id_str));
                        }
                    }
                    Err(err) => {
                        return Err(err);
                    }
                }
            } else if c == '=' {
                if self.check_next(|c| c == '=') {
                    self.read_count(2);
                    self.push_token(Token::Equals);
                } else {
                    return Err(ScannerError::new("Invalid token [=]"));
                }
            } else if c == '!' {
                if self.check_next(|c| c == '=') {
                    self.read_count(2);
                    self.push_token(Token::NotEquals);
                } else if self.check_next(Scanner::is_alpha) {
                    self.pop(); // read exclamation point
                    if let Ok(id_str) = self.read_identifier() {
                        self.push_token(Token::CommandEval(id_str));
                    } else {
                        return Err(ScannerError::new("Invalid token [!]"));
                    }
                }
            } else if c == '>' {
                self.pop();
                self.if_next(|c| c == '=', Token::GreaterThanEquals, Token::GreaterThan);
            } else if c == '<' {
                self.pop();
                if self.check_here(|c| c == '=') {
                    self.pop();
                    self.push_token(Token::LessThanEquals);
                } else if self.check_here(|c| c == '|') {
                    self.pop();
                    self.if_next(|c| c == '>', Token::StreamPipe, Token::PushPipe);
                } else if self.check_here(|c| c == '-') {
                    self.pop();
                    self.push_token(Token::LeftSetter);
                } else {
                    self.push_token(Token::LessThan);
                }
            } else if c == '|' {
                self.pop();
                self.if_next(|c| c == '>', Token::PullPipe, Token::Pipe);
            } else if c == '+' {
                self.pop();
                self.push_token(Token::Plus);
            } else if c == '-' {
                self.pop();
                if self.check_here(|c| c == '>') {
                    self.pop();
                    self.push_token(Token::RightSetter);
                } else if self.check_here(|c| c == '-') {
                    self.pop();
                    if self.check_here(|c| c == '-') {
                        self.pop();
                        if let Some(rest) = self.read_remaining() {
                            self.push_token(Token::new_string(rest));
                        }
                    } else {
                        if let Ok(id) = self.read_identifier() {
                            self.push_token(Token::MarkedArg(id));
                        } else {
                            return Err(ScannerError::new("Argument marker '--' must be followed by valid identifier"));
                        }
                    }
                } else {
                    self.push_token(Token::Minus);
                }
            } else if c == '*' {
                self.pop();
                self.push_token(Token::Multiply);
            } else if c == '/' {
                self.pop();
                self.push_token(Token::Divide);
            } else if c == ';' {
                self.pop();
                self.push_token(Token::EndOfStatement);
            } else if c == '(' {
                self.pop();
                self.push_token(Token::LeftParens);
            } else if c == ')' {
                self.pop();
                self.push_token(Token::RightParens);
            } else if c == '@' {
                self.pop();
                if let Some(url_str) = self.read_until(false, true, Scanner::is_space) {
                    match Token::new_url(url_str) {
                        Ok(tkn) => {
                            self.push_token(tkn);
                        },
                        Err(err) => {
                            return Err(err);
                        },
                    }
                }
            } else if c == '$' {
                self.pop();
                if let Ok(id) = self.read_identifier() {
                    self.push_token(Token::Variable(id));
                } else {
                    return Err(ScannerError::new("Valid identifier expected after variable marker '$'"));
                }
            } else if c == ':' {
                self.pop();
                if let Some(&next) = self.peek() {
                    if next == ':' {
                        self.pop();
                        self.push_token(Token::CommandSpecifier);
                    } else {
                        return Err(ScannerError::new("Invalid character [:]"));
                    }
                } else {
                    return Err(ScannerError::new("Invalid character [:]"));
                }
            } else {
                return Err(ScannerError::new(format!("Invalid character [{}]", c).as_str()));
            }
        }

        Ok(())
    }

    fn if_next(&mut self, test:fn(char) -> bool, first : Token, second : Token) {
        if self.check_here(test) {
            self.pop();
            self.push_token(first);
        } else {
            self.push_token(second);
        }
    }

    fn read_identifier(&mut self) -> Result<String, ScannerError>{
        let identifier_opt = self.read_until(
            false,
            true,
            |c| !Scanner::is_alpha_numeric(c) && c != '.' && c != '_');
        if let Some(id_str) = identifier_opt {
            Ok(id_str)
        } else {
            if let Some(current_char) = self.peek() {
                Err(ScannerError::new(format!("Invalid character in identifier [{}]",
                                              current_char).as_str()))
            } else {
                Err(ScannerError::new("Invalid character in identifier [EOF]"))
            }
        }
    }

    fn push_token(&mut self, tkn : Token) {
        self.tokens.push(tkn)
    }

    fn read_count(&mut self, count : usize) -> Option<String> {
        let mut buf = String::new();
        let mut count_down = count;
        while count_down > 0 {
            if let Some(c) = self.pop() {
                buf.push(c);
            } else {
                return None;
            }
            count_down = count_down - 1;
        }
        Some(buf)
    }

    fn read_while(&mut self, test : fn(char) -> bool) -> String {
        let mut str = String::new();
        loop {
            if let Some(&c) = self.chars.get(self.pos) {
                if test(c) {
                    str.push(c);
                    self.pos = self.pos + 1;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        str
    }

    fn read_remaining(&mut self) -> Option<String> {
        self.read_until(true, true, |c| false)
    }

    fn read_until(&mut self, allow_escape_before_pred : bool, allow_eof : bool, test: fn(char) -> bool) -> Option<String> {
        let mut buf = String::new();
        let mut do_stop = false;
        let mut no_match = false;
        let mut prev_escape = false;

        loop {
            if self.pos < self.length {
                if let Some(&c) = self.peek() {
                    if test(c) && !prev_escape {
                        do_stop = true;
                    } else {
                        if allow_escape_before_pred {
                            if prev_escape && c == '\\' {
                                buf.push('\\');
                                // used up the escape by escaping a '\' character
                                prev_escape = false;
                            } else {
                                prev_escape = c == '\\';
                            }
                            if c != '\\' {
                                buf.push(c);
                            }
                        } else {
                            buf.push(c);
                        }
                    }
                    self.pos = self.pos + 1;
                } else {
                    panic!("We already tested for size, so we shouldn't get here");
                }
            } else {
                no_match = !allow_eof;
                do_stop = true;
            }

            // do not consume the character matched in the predicate
            if do_stop && !no_match && !allow_eof {
                self.pos = self.pos - 1;
            }

            if do_stop {
                break;
            }
        }

        if no_match {
            None
        } else {
            Some(buf)
        }
    }


    fn peek(&mut self) -> Option<&char> {
        self.chars.get(self.pos)
    }

    fn pop(&mut self) -> Option<char> {
        if let Some(&c) = self.peek() {
            self.pos = self.pos + 1;
            return Some(c);
        }
        None
    }

    fn peek_next(&mut self) -> Option<&char> {
        self.chars.get(self.pos + 1)
    }

    fn check_here(&mut self, test: fn(char) -> bool) -> bool {
        if let Some(&c) = self.peek() {
            test(c)
        } else {
            false
        }
    }

    fn check_next(&mut self, test: fn(char) -> bool) -> bool {
        if let Some(&c) = self.peek_next() {
            test(c)
        } else {
            false
        }
    }

    fn read_if(&mut self, test : fn(char) -> bool) -> bool {
        if let Some(&c) = self.chars.get(self.pos) {
            if test(c) {
                self.pos = self.pos + 1;
                return true
            }
        }
        false
    }

    fn is_space(c : char) -> bool {
        c.is_whitespace()
    }

    fn is_digit(c : char) -> bool {
        c.is_digit(10)
    }

    fn is_alpha(c: char) -> bool {
        c.is_alphabetic()
    }

    fn is_alpha_numeric(c : char) -> bool {
        c.is_alphanumeric()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    String(Value),
    Boolean(Value),
    Integral(Value),
    Fractional(Value),
    Time(Value),
    Url(Value),
    EndOfStatement,
    LeftParens,
    RightParens,
    Identifier(String),
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    GreaterThanEquals,
    LessThanEquals,
    Plus,
    Minus,
    Multiply,
    Divide,
    Pipe,
    PullPipe,
    PushPipe,
    StreamPipe,
    LeftSetter,
    RightSetter,
    CommandSpecifier,
    CommandEval(String),
    Variable(String),
    MarkedArg(String),
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

    pub fn new_url(uri_str : String) -> Result<Token, ScannerError> {
        match Url::parse(uri_str.as_str()) {
            Ok(url) => {
                Ok(Token::Url(Value::ValueUrl { val : url }))
            }
            Err(err) => {
                Err(ScannerError::new(err.to_string().as_str()))
            }
        }
    }

    pub(crate) fn get_string_rep(&self) -> String {
        match self {
            Token::String(val) => { val.to_string() }
            Token::Integral(val) => { val.to_string()}
            Token::Fractional(val) => { val.to_string()}
            Token::Boolean(val) => { val.to_string()}
            Token::Url(val) => {val.to_string()}
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
            Token::Pipe => {String::from("|")}
            Token::PullPipe => {String::from("|>")}
            Token::PushPipe => { String::from("<|")}
            Token::StreamPipe => {String::from("<|>")}
            Token::LeftSetter => {String::from("<-")}
            Token::RightSetter => { String::from("->")}
            Token::CommandSpecifier => {String::from("::")}
            Token::CommandEval(val) => {format!("!{}", val.as_str())}
            Token::Variable(val) => {format!("${}", val.as_str()) }
            Token::MarkedArg(val) => {format!("--{}", val.as_str())}
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
            Token::Url(_) => { vec![TokenType::Value]}
            Token::Time(_) => { vec![TokenType::Value]}
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

#[cfg(test)]
mod tests {
    use bigdecimal::{BigDecimal, FromPrimitive};
    use num_bigint::{BigInt, ParseBigIntError};
    use reqwest::Url;

    use crate::Scanner;
    use crate::scanner::{ScannerError, Token};
    use crate::values::Value;

    fn assert_vec_items(tkns : &Vec<Token>, expected_tokens : Vec<Token>) {
        let mut idx = 0;
        for tkn in tkns {
            let expected = expected_tokens.get(idx).unwrap();
            assert_eq!(tkn, expected);
            idx = idx + 1;
        }
    }

    fn check_scan(expr : &str, expected_tokens : Vec<Token>) {
        match Scanner::scan(expr) {
            Ok(result) => {
                assert_vec_items(&result, expected_tokens);
            },
            Err(err) => {
                println!("ScannerError [{}] in input [{}]", err, expr);
                assert_eq!(1, 2);
            }
        }
    }

    #[test]
    fn test_arithmetic() {
        check_scan(
            "1+2 * (3 - 4) / 5",
            vec![
                Token::new_integral(String::from("1")).unwrap(),
                Token::Plus,
                Token::new_integral(String::from("2")).unwrap(),
                Token::Multiply,
                Token::LeftParens,
                Token::new_integral(String::from("3")).unwrap(),
                Token::Minus,
                Token::new_integral(String::from("4")).unwrap(),
                Token::RightParens,
                Token::Divide,
                Token::new_integral(String::from("5")).unwrap(),
            ]
        );
    }

    #[test]
    fn test_numbers() {
        check_scan("3.14159", vec![Token::new_fractional(String::from("3.14159")).unwrap()]);
        check_scan(".14159", vec![Token::new_fractional(String::from(".14159")).unwrap()]);
        check_scan("100.123456", vec![Token::new_fractional(String::from("100.123456")).unwrap()]);
        check_scan("1", vec![Token::new_integral(String::from("1")).unwrap()]);
        check_scan("101010", vec![Token::new_integral(String::from("101010")).unwrap()]);
    }

    #[test]
    fn test_identifiers() {
        check_scan("a", vec![Token::Identifier(String::from("a"))]);
        check_scan("a1", vec![Token::Identifier(String::from("a1"))]);
        check_scan("a_b_c", vec![Token::Identifier(String::from("a_b_c"))]);
        check_scan("a.b.c", vec![Token::Identifier(String::from("a.b.c"))]);
        check_scan("_longer_identifier_that_starts_with_an.underscore something_else ",
                   vec![Token::Identifier(String::from("_longer_identifier_that_starts_with_an.underscore")),
                   Token::Identifier(String::from("something_else"))]);
    }

    #[test]
    fn test_ops() {
        check_scan("+ - * /", vec![Token::Plus, Token::Minus, Token::Multiply, Token::Divide]);
        check_scan("< <= >= > == !=", vec![Token::LessThan, Token::LessThanEquals,
                                           Token::GreaterThanEquals, Token::GreaterThan, Token::Equals, Token::NotEquals]);
        check_scan("-> <- | |> <| <|>", vec![Token::RightSetter, Token::LeftSetter,
                                             Token::Pipe, Token::PullPipe, Token::PushPipe, Token::StreamPipe]);

    }

    #[test]
    fn test_strings() {
        check_scan (" \"string\"", vec![Token::new_string(String::from("string"))]);
        check_scan("identifier --- this is a string", vec![
            Token::Identifier(String::from("identifier")), Token::new_string(String::from(" this is a string"))]);
    }

    #[test]
    fn test_uris() {
        check_scan("@https://www.google.com something_else",
                   vec![
                       Token::Url(Value::ValueUrl { val : Url::parse("https://www.google.com").unwrap() } ),
                       Token::Identifier(String::from("something_else"))])
    }

    #[test]
    fn test_vars() {
        check_scan("1 + $my_var == 2", vec![
            Token::new_integral( String::from("1")).unwrap(),
            Token::Plus,
            Token::Variable(String::from("my_var")),
            Token::Equals,
            Token::new_integral(String::from("2")).unwrap(),
        ])
    }

}