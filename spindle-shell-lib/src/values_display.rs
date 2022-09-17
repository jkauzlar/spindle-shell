use std::io::Stdout;
use crossterm::{QueueableCommand, style};
use crossterm::style::Stylize;
use crate::ShellDisplayable;
use spindle_lang::values::Value;

impl ShellDisplayable for Value {
    fn pretty_print_shell(&self, stdout: &mut Stdout) {
        stdout.queue(style::PrintStyledContent(self.to_string().dark_magenta()));
    }
}