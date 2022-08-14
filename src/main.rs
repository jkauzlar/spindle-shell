use crate::analyzer::{SemanticAnalyzer};
use crate::environment::Environment;
use crate::functions::get_builtins;

use crate::parser::{Parser};
use crate::scanner::{Scanner};
use crate::shell::{ Shell, ShellApplicationEnvironment, ShellCommand};
use crate::value_store::InMemoryValueStore;

mod scanner;
mod environment;
mod parser;
mod values;
mod types;
mod shell;
mod analyzer;
mod value_store;
mod functions;

struct App {
    env : Box<Environment>
}

impl ShellApplicationEnvironment for App {
    fn handle_input(&mut self, inp: &str) -> ShellCommand {
        let mut buf = String::new();
        match Scanner::scan(inp) {
            Ok(tkns) => {
                buf.push_str("Scanner output: ");
                for tkn in &tkns {
                    buf.push_str(tkn.to_string().clone().as_str());
                    buf.push_str(" ");
                }
                buf.push_str("\r\n");
                match Parser::parse(tkns) {
                    Ok(cmd) => {
                        for expr in cmd.exprs {
                            buf.push_str("Parser output: ");
                            buf.push_str(expr.to_string().as_str());
                            buf.push_str("\r\n");
                            match SemanticAnalyzer::analyze(&self.env, expr) {
                                Ok(sem_expr) => {
                                    buf.push_str(sem_expr.to_string().as_str());
                                    buf.push_str("\r\n");
                                }
                                Err(err) => {
                                    return ShellCommand::ERR(err.to_string());
                                }
                            }
                        }
                        ShellCommand::OUT(buf.to_string())
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

    fn supply_prompt(&self) -> String {
        String::from(" > ")
    }
}

fn main() {
    let mut app = App {
        env: Box::new(Environment::new(Box::new(InMemoryValueStore::create())))
    };

    for func in get_builtins() {
        app.env.put_function(func);
    }

    Shell::new(Box::new(app)).run();
}
