extern crate core;

use spindle_shell_lib::{Shell, ShellApplicationEnvironment, ShellCommand};
use crate::analyzer::{SemanticAnalyzer};
use crate::environment::Environment;
use crate::evaluator::{Evaluator};
use crate::functions::get_builtins;

use crate::parser::{Parser};
use crate::scanner::{Scanner};
use crate::value_store::InMemoryValueStore;
use crate::values::Value;

mod scanner;
mod environment;
mod parser;
mod values;
mod types;
mod analyzer;
mod value_store;
mod functions;
mod evaluator;
mod external_resources;
mod tokens;

struct App {
    env : Box<Environment>
}

impl App {
    fn parse_user_command(inp : &str) -> Option<UserCommand> {
        if inp.starts_with(":quit ") {
            Some(UserCommand::QUIT)
        } else {
            None
        }
    }
}

enum UserCommand {
    QUIT,
}

impl ShellApplicationEnvironment for App {
    fn handle_input(&mut self, inp: &str) -> ShellCommand {
        let mut buf = String::new();

        if inp.starts_with(':') {
            if let Some(user_command)  = App::parse_user_command(inp) {
                return match user_command {
                    UserCommand::QUIT => ShellCommand::QUIT
                }
            }
        }

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
                                    buf.push_str("Analyzer output: ");
                                    buf.push_str(sem_expr.to_string().as_str());
                                    buf.push_str("\r\n");
                                    match Evaluator::eval(&mut self.env, sem_expr) {
                                        Ok(v) => {
                                            buf.push_str("Evaluator output: ");
                                            buf.push_str(v.to_string().as_str());
                                            self.env.store_value("_", v.clone());
                                            // buf.push_str("\r\n");
                                        }
                                        Err(err) => {
                                            ShellCommand::ERR(err.to_string());
                                        }
                                    }
                                }
                                Err(err) => {
                                    buf.push_str("error: ");
                                    buf.push_str(err.to_string().as_str());
                                    buf.push_str("\r\n");
                                    return ShellCommand::ERR(buf.to_string());
                                }
                            }
                        }
                        ShellCommand::OUT(buf.to_string())
                    }
                    Err(err) => {
                        buf.push_str("error: ");
                        buf.push_str(err.to_string().as_str());
                        buf.push_str("\r\n");
                        return ShellCommand::ERR(buf.to_string());
                    }
                }
            }
            Err(err) => {
                buf.push_str("error: ");
                buf.push_str(err.to_string().as_str());
                buf.push_str("\r\n");
                return ShellCommand::ERR(buf.to_string());
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
