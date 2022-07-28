use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Write};
use std::iter::Scan;
use std::str::{Chars, FromStr};
use bigdecimal::BigDecimal;
use num_bigint::BigInt;
use reqwest::Url;

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
                    if let Ok(dec_str) = BigDecimal::from_str(digits.as_str()) {
                        self.push_token(Token::Fractional(dec_str));
                    }
                } else {
                    if let Ok(big_int) = BigInt::from_str(digits.as_str()) {
                        self.push_token(Token::Integral(big_int));
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
                    self.push_token(Token::String(a_str));
                } else {
                    return Err(ScannerError::new(format!("Missing closing '\"'").as_str()));
                }
            } else if Scanner::is_alpha(c) || c == '_' {
                if let id_str = self.read_identifier()? {
                    if id_str.eq(&String::from("true")) {
                        self.push_token(Token::Boolean(true));
                    } else if id_str.eq(&String::from("false")) {
                        self.push_token(Token::Boolean(false));
                    } else {
                        self.push_token(Token::Identifier(id_str));
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
                            self.push_token(Token::String(rest));
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
                    match Url::parse(url_str.as_str()) {
                        Ok(url) => {
                            self.push_token(Token::Uri(url));
                        },
                        Err(err) => {
                            return Err(ScannerError::new(format!("Invalid Uri format in string [{}]: {}", url_str, err.to_string()).as_str()));
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
                println!("read_until: stopping with [{}] and no_match [{}]", buf, no_match);
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

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    EndOfStatement,
    LeftParens,
    RightParens,
    String(String),
    Integral(BigInt),
    Fractional(BigDecimal),
    Boolean(bool),
    Identifier(String),
    Uri(Url),
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
    use num_bigint::BigInt;
    use reqwest::Url;
    use crate::Scanner;
    use crate::scanner::{ScannerError, Token};

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
                Token::Integral(BigInt::from(1)),
                Token::Plus,
                Token::Integral(BigInt::from(2)),
                Token::Multiply,
                Token::LeftParens,
                Token::Integral(BigInt::from(3)),
                Token::Minus,
                Token::Integral(BigInt::from(4)),
                Token::RightParens,
                Token::Divide,
                Token::Integral(BigInt::from(5)),
            ]
        );
    }

    #[test]
    fn test_numbers() {
        check_scan("3.14159", vec![Token::Fractional(BigDecimal::from_f32(3.14159f32).unwrap())]);
        check_scan(".14159", vec![Token::Fractional(BigDecimal::from_f32(0.14159f32).unwrap())]);
        check_scan("100.123456", vec![Token::Fractional(BigDecimal::from_f64(100.123456f64).unwrap())]);
        check_scan("1", vec![Token::Integral(BigInt::from(1))]);
        check_scan("101010", vec![Token::Integral(BigInt::from(101010))]);
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
        check_scan (" \"string\"", vec![Token::String(String::from("string"))]);
        check_scan("identifier --- this is a string", vec![
            Token::Identifier(String::from("identifier")), Token::String(String::from(" this is a string"))]);
    }

    #[test]
    fn test_uris() {
        check_scan("@https://www.google.com something_else",
                   vec![
                       Token::Uri(Url::parse("https://www.google.com").unwrap()),
                       Token::Identifier(String::from("something_else"))])
    }

    #[test]
    fn test_vars() {
        check_scan("1 + $my_var == 2", vec![
            Token::Integral(BigInt::from(1)),
            Token::Plus,
            Token::Variable(String::from("my_var")),
            Token::Equals,
            Token::Integral(BigInt::from(2)),
        ])
    }

}