mod scanner;
mod environment;

use std::io::{stdout, stdin, Write};
use crossterm::{
    ExecutableCommand, QueueableCommand,
    terminal, cursor, style::{self, Stylize}, Result
};
use crossterm::style::PrintStyledContent;
use crate::scanner::Scanner;

fn main() -> Result<()> {
    let mut stdout = stdout();

    loop {
        let mut prompt = format!("{} >", "");
        stdout.queue(style::PrintStyledContent("> ".stylize()));
        stdout.flush();

        let mut buf = String::new();
        let result = stdin().read_line(&mut buf);

        // remove trailing newline
        buf.remove(buf.len() - 1);

        match result {
            Ok(_) => {
                if String::from("exit").eq(&buf) {
                    break;
                } else {
                    Scanner::scan(&buf);
                    stdout.queue(PrintStyledContent(buf.stylize()));
                    stdout.flush()?;
                }
            }
            Err(err) => {}
        }
    }

    stdout.queue(PrintStyledContent("Goodbye".stylize()));
    stdout.flush()?;
    Ok(())
}
