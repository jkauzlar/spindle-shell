#![allow(unused_results)]
extern crate core;

use std::io::{Error, Stdout};
use crossterm::{QueueableCommand, style};
use crossterm::style::{Print, Stylize};
use regex::internal::Inst;
use tokio::time::Instant;
use spindle_preprocessor::{PreprocCommand, SpindlePreprocessor};
use spindle_shell_lib::{Shell, ShellApplicationEnvironment, ShellCommand, ShellDisplayable};
use spindle_lang::analyzer::SemanticAnalyzer;
use spindle_lang::environment::Environment;
use spindle_lang::evaluator::Evaluator;
use crate::file_functions::FileFunctions;
use spindle_lang::functions::{get_builtins, get_coercions};

use spindle_lang::parser::Parser;
use spindle_lang::scanner::Scanner;
use spindle_lang::value_store::InMemoryValueStore;
use spindle_lang::values::Value;

mod file_functions;
mod http_functions;

struct App {
    env : Box<Environment>
}

enum ShellMessage {
    ErrorMessage(String),
    NormalMessage(String),
    TabularInfo {
        headers : Vec<String>,
        rows : Vec<Vec<String>>,
    }
}

impl ShellDisplayable for ShellMessage {
    fn pretty_print_shell(&self, stdout: &mut Stdout) {
        match self {
            ShellMessage::ErrorMessage(msg) => {
                stdout.queue(style::PrintStyledContent(msg.clone().dark_red()));
            }
            ShellMessage::NormalMessage(msg) => {
                stdout.queue(style::PrintStyledContent(msg.clone().white()));
            }
            ShellMessage::TabularInfo { headers, rows } => {
                pretty_print_table(stdout, headers, rows);
            }
        }
    }
}

fn repeated_str(c : char, size : usize) -> String {
    let mut buf = String::new();
    for idx in 0..size {
        buf.push(c)
    }

    buf
}

fn write_row_buf(line_width : usize, col_starts : &Vec<usize>, vals : &Vec<String>) -> String {
    let mut buf = repeated_str(' ', line_width);
    for col_idx in 0..col_starts.len() {
        let val = &vals[col_idx];
        let range = col_starts[col_idx]..(col_starts[col_idx] + val.len());
        buf.replace_range(range, val)
    }

    buf
}


fn pretty_print_table(stdout: &mut Stdout, headers : &Vec<String>, rows: &Vec<Vec<String>>) -> Result<(), Error>{
    let mut column_starts : Vec<usize> = vec![];
    let mut header_widths: Vec<usize> = vec![];
    let mut column_widths: Vec<usize> = vec![];
    let num_columns = headers.len();
    let column_range = 0..num_columns;

    // initialize column widths based on header widths
    for idx in column_range.clone() {
        let header_len = headers[idx].len();
        header_widths.push(header_len);
        column_widths.push(header_len);
    }

    // increase column widths if any cells are wider
    for row in rows {
        for col_idx in 0..row.len() {
            let col = &row[col_idx];
            if column_widths[col_idx] < col.len() {
                column_widths[col_idx] = col.len()
            }
        }
    }

    // figure out where each column starts
    let margin_width = 2;
    for col_idx in column_range.clone() {
        if col_idx > 0 {
            let last_column_start = column_starts[col_idx - 1];
            let last_column_width = column_widths[col_idx - 1];
            column_starts.push(last_column_start + last_column_width + margin_width)
        } else {
            column_starts.push(margin_width);
        }
    }

    let line_width = column_starts[num_columns - 1] + column_widths[num_columns - 1];

    // write headers
    let headers_buf = write_row_buf(line_width, &column_starts, headers);
    stdout.queue(Print(headers_buf.as_str()));
    stdout.queue(Print("\r\n"));

    // write underlines of headers
    let underlines : Vec<String> = headers.iter().map(|h| repeated_str('-', h.len())).collect();
    let underlines_buf = write_row_buf(line_width, &column_starts, &underlines);
    stdout.queue(Print(underlines_buf.as_str()));
    stdout.queue(Print("\r\n"));

    // write data
    for row in rows {
        let row_buf = write_row_buf(line_width, &column_starts, row);
        stdout.queue(Print(row_buf.as_str()))?;
        stdout.queue(Print("\r\n"))?;
    }

    Ok(())
}


impl ShellApplicationEnvironment for App {
    fn handle_input(&mut self, inp: &str) -> ShellCommand {
        let mut input_buffer = String::from(inp);
        let mut print_type_only = false;
        let mut print_exec_time = false;
        let mut buf = String::new();

        match SpindlePreprocessor::run(input_buffer.as_str()) {
            Err(preproc_err) => {
                return ShellCommand::ERR(Box::new(ShellMessage::ErrorMessage(preproc_err.to_string())));
            }
            Ok(PreprocCommand::Quit) => {
                return ShellCommand::QUIT;
            }
            Ok(PreprocCommand::Help) => {
                let mut rows : Vec<Vec<String>> = vec![];
                for cmd in PreprocCommand::get_command_infos() {
                    rows.push(vec![cmd.pp_cmd_name.clone(), cmd.pp_cmd_desc.clone()]);
                }
                let tbl = ShellMessage::TabularInfo {
                    headers: vec![String::from("Command"), String::from("Description")],
                    rows
                };
                return ShellCommand::OUT(Box::new(tbl));
            }
            Ok(PreprocCommand::Types(expr)) => {
                input_buffer.clear();
                input_buffer.push_str(expr.clone().as_str());
                print_type_only = true;
            }
            Ok(PreprocCommand::Timer(expr)) => {
                input_buffer.clear();
                input_buffer.push_str(expr.clone().as_str());
                print_exec_time = true;
            }
            Ok(PreprocCommand::NoCommand) => {
                // continue as normal
            }
        }

        let start_time = Instant::now();

        return match Scanner::scan(input_buffer.as_str()) {
            Ok(tkns) => {
                match Parser::parse(tkns) {
                    Ok(cmd) => {
                        for expr in cmd.exprs {
                            match SemanticAnalyzer::analyze(&self.env, expr) {
                                Ok(sem_expr) => {
                                    if print_type_only {
                                        buf.push_str(sem_expr.to_string().as_str());
                                        buf.push_str("\r\n");
                                    } else {
                                        match Evaluator::eval(&mut self.env, sem_expr) {
                                            Ok(v) => {
                                                let stop_time = Instant::now();
                                                buf.push_str(v.to_string().as_str());
                                                buf.push_str(";\n\r");
                                                self.env.store_value("_", v.clone());
                                                let exec_time = stop_time.duration_since(start_time).as_micros();
                                                if print_exec_time {
                                                    buf.push_str(format!("Execution Time: {}μs\r\n", exec_time).as_str());
                                                }
                                            }
                                            Err(err) => {
                                                return ShellCommand::ERR(Box::new(ShellMessage::ErrorMessage(err.to_string())));
                                            }
                                        }
                                    }
                                }
                                Err(err) => {
                                    buf.push_str("error: ");
                                    buf.push_str(err.to_string().as_str());
                                    buf.push_str("\r\n");
                                    return ShellCommand::ERR(Box::new(ShellMessage::ErrorMessage(err.to_string())));
                                }
                            }
                        }
                        ShellCommand::OUT(Box::new(ShellMessage::NormalMessage(buf.to_string())))
                    }
                    Err(err) => {
                        buf.push_str("error: ");
                        buf.push_str(err.to_string().as_str());
                        buf.push_str("\r\n");
                        ShellCommand::ERR(Box::new(ShellMessage::ErrorMessage(err.to_string())))
                    }
                }
            }
            Err(err) => {
                buf.push_str("error: ");
                buf.push_str(err.to_string().as_str());
                buf.push_str("\r\n");
                ShellCommand::ERR(Box::new(ShellMessage::ErrorMessage(err.to_string())))
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

    app.env.put_functions(get_builtins());
    app.env.put_functions(get_coercions());
    app.env.put_functions(FileFunctions::get_file_functions());

    Shell::new(Box::new(app)).run();
}
