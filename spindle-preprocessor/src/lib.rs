use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

pub struct SpindlePreprocessor {

}

impl SpindlePreprocessor {
    pub fn run(inp : &str) -> Result<PreprocCommand, PreprocessorError> {
        PreprocCommandParser::parse_from(inp)
    }
}

pub enum PreprocCommand {
    NoCommand,
    Quit,
    Types(String),
    Timer(String),
    Functions(String),
    Describe(String),
    Help,
}

impl PreprocCommand {
    pub fn get_command_infos() -> Vec<PreprocCommandInfo> {
        let mut info = vec![];
        info.push(PreprocCommandInfo::new(":[h]elp", "show this help text"));
        info.push(PreprocCommandInfo::new(":[q]uit", "exit shell"));
        info.push(PreprocCommandInfo::new(":fns | :functions <search>?", "list available functions filtered optionally by argument string"));
        info.push(PreprocCommandInfo::new(":desc | :describe <search>", "describe functions filtered optionally by argument string"));
        info.push(PreprocCommandInfo::new(":[t]ype(s) <expression>",
                                          "do not execute, but show only the types of the expression"));
        info.push(PreprocCommandInfo::new(":time(r) <expression>",
                                          "Print the execution time of the expression in microseconds"));

        info
    }
}

pub struct PreprocCommandInfo {
    pub pp_cmd_name : String,
    pub pp_cmd_desc : String,
}

impl PreprocCommandInfo {
    pub fn new(name : &str, desc : &str) -> Self {
        Self {
            pp_cmd_name : String::from(name),
            pp_cmd_desc : String::from(desc),
        }
    }
}

struct PreprocCommandParser {
    chars : Vec<char>,
    idx : usize,
}

impl PreprocCommandParser {
    fn parse_from(inp: &str) -> Result<PreprocCommand, PreprocessorError> {
        Self::new(inp).parse()
    }

    fn new(inp: &str) -> Self {
        PreprocCommandParser {
            chars : inp.chars().collect(),
            idx: 0,
        }
    }

    fn parse(&mut self) -> Result<PreprocCommand, PreprocessorError> {
        if let Some(c) = self.peek() {
            if c == &':' {
                self.pop();
                match self.parse_command() {
                    ok @ Ok(_) => {
                        if self.peek().is_some() {
                            let rest = self.read_remaining();
                            Err(PreprocessorError::new(format!("Unexpected characters [{}] following command", rest).as_str()))
                        } else {
                            ok
                        }
                    }
                    err @ Err(_) => {
                        err
                    }
                }
            } else {
                Ok(PreprocCommand::NoCommand)
            }
        } else {
            Ok(PreprocCommand::NoCommand)
        }
    }

    fn parse_command(&mut self) -> Result<PreprocCommand, PreprocessorError> {
        let cmd_str = self.read_while(|c| c.is_alphabetic());
        if cmd_str.as_str().eq("help") || cmd_str.as_str().eq("h") {
            Ok(PreprocCommand::Help)
        } else if cmd_str.as_str().eq("quit") || cmd_str.as_str().eq("q") {
            Ok(PreprocCommand::Quit)
        } else if cmd_str.as_str().eq("type") || cmd_str.as_str().eq("types") || cmd_str.as_str().eq("t") {
            Ok(PreprocCommand::Types(self.read_remaining()))
        } else if cmd_str.as_str().eq("time") || cmd_str.as_str().eq("timer") {
            Ok(PreprocCommand::Timer(self.read_remaining()))
        } else if cmd_str.as_str().eq("fns") || cmd_str.as_str().eq("functions") {
            Ok(PreprocCommand::Functions(String::from(self.read_remaining().trim())))
        } else if cmd_str.as_str().eq("desc") || cmd_str.as_str().eq("describe") {
            Ok(PreprocCommand::Describe(String::from(self.read_remaining().trim())))
        } else if cmd_str.as_str().eq("") {
            Err(PreprocessorError::new("Shell command name should follow colon. Type ':help' for list of available commands."))
        } else {
            Err(PreprocessorError::new(format!("Unknown preprocessor command [{}]", cmd_str.clone()).as_str()))
        }
    }

    fn read_remaining(&mut self) -> String {
        let mut buf = String::new();
        while let Some(&c) = self.peek() {
            buf.push(c);
            self.pop();
        }
        buf
    }

    fn read_while(&mut self, cond:fn(char) -> bool) -> String {
        let mut buf = String::new();
        loop {
            match self.peek() {
                None => {break;}
                Some(&c) => {
                    if cond(c) {
                        buf.push(c);
                        self.pop();
                    } else {
                        break;
                    }
                }
            }
        }
        buf
    }

    fn pop(&mut self) {
        self.idx = self.idx + 1;
    }

    fn peek(&self) -> Option<&char> {
        self.chars.get(self.idx)
    }
}

#[derive(Debug)]
pub struct PreprocessorError {
    message : String,
}

impl PreprocessorError {
    pub fn new(msg : &str) -> PreprocessorError {
        PreprocessorError {
            message : String::from(msg)
        }
    }
}

impl Display for PreprocessorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.message.as_str())
    }
}

impl Error for PreprocessorError { }




