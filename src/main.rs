use std::iter::Scan;

use crate::parser::{Command, Parser, ParserError};
use crate::scanner::{Scanner};
use crate::shell::{ Shell, ShellApplicationEnvironment, ShellCommand};

mod scanner;
mod environment;
mod parser;
mod values;
mod types;
mod shell;

struct App { }

impl ShellApplicationEnvironment for App {
    fn handle(&mut self, inp: &str) -> ShellCommand {
        match Scanner::scan(inp) {
            Ok(tkns) => {
                match Parser::parse(tkns) {
                    Ok(cmd) => {
                        ShellCommand::OUT(cmd.to_string())
                    }
                    Err(err) => {
                        ShellCommand::ERR(err.to_string())
                    }
                }
            }
            Err(err) => {
                ShellCommand::ERR(err.to_string())
            }
        }
    }

    fn supply(&self) -> String {
        String::from("prompt")
    }
}

fn main() {
    let mut app = App { };

    Shell::new(Box::new(app)).run();
}
