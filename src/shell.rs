use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::io::{stdout, stdin, Write};
use crossterm::{
    ExecutableCommand, QueueableCommand,
    terminal, cursor, style::{self, Stylize}
};
use crossterm::style::PrintStyledContent;

pub trait ShellApplicationEnvironment {
    fn handle(&mut self, inp : &str) -> ShellCommand;
    fn supply(&self) -> String;
}

pub enum ShellCommand {
    OUT(String),
    ERR(String),
    QUIT,
}

pub struct Shell {
    app_env : Box<dyn ShellApplicationEnvironment>,
    history : Vec<String>,
}

impl Shell {
    
    pub fn new(app_env : Box<dyn ShellApplicationEnvironment>) -> Self {
        Shell {
            app_env,
            history: vec![]
        }
    }

    pub fn run(&mut self) -> Result<(), ShellError> {
        let mut stdout = stdout();

        loop {
            let prompt = format!("{} >", self.app_env.supply());
            stdout.queue(style::PrintStyledContent(prompt.stylize()));
            stdout.flush();

            let mut buf = String::new();
            let result = stdin().read_line(&mut buf);
            // remove trailing newline
            buf.remove(buf.len() - 1);

            match result {
                Ok(_) => {
                    let command = self.app_env.handle(buf.as_str());
                    match command {
                        ShellCommand::OUT(res_str) => {
                            stdout.queue(style::PrintStyledContent(res_str.stylize()));
                        }
                        ShellCommand::ERR(err_str) => {
                            stdout.queue(style::PrintStyledContent(err_str.stylize()));
                        }
                        ShellCommand::QUIT => {
                            break;
                        }
                    }
                }
                Err(err) => {
                    return Err(ShellError::new(err.to_string()))
                }
            }
        }

        stdout.queue(PrintStyledContent("Goodbye".stylize()));
        stdout.flush();

        Ok(())
    }
}

#[derive(Debug)]
pub struct ShellError {
    message : String,
}

impl ShellError {
    pub fn new(message : String) -> Self {
        ShellError {
            message
        }
    }
}

impl Display for ShellError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.message.as_str())
    }
}

impl Error for ShellError { }