#![allow(unused_results)]
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::io;
use std::io::{stdout, Stdout, Write};

use crossterm::{cursor, execute, QueueableCommand, style::{self, Stylize}, terminal};
use crossterm::cursor::MoveToColumn;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers, read};
use crossterm::style::{Print, PrintStyledContent};
use crossterm::terminal::{EnterAlternateScreen, LeaveAlternateScreen};

mod values_display;


pub trait ShellApplicationEnvironment {
    fn handle_input(&mut self, inp : &str) -> ShellCommand;
    fn supply_prompt(&self) -> String;
}

pub struct MergedShellDisplayable {
    first : Box<dyn ShellDisplayable>,
    second : Box<dyn ShellDisplayable>,
}

impl MergedShellDisplayable {
    fn new(first : Box<dyn ShellDisplayable>, second : Box<dyn ShellDisplayable>) -> Self {
        Self {
            first, second,
        }
    }
}

impl ShellDisplayable for MergedShellDisplayable {
    fn pretty_print_shell(&self, stdout: &mut Stdout) {
        self.first.pretty_print_shell(stdout);
        self.second.pretty_print_shell(stdout);
    }
}

pub trait ShellDisplayable {
    fn pretty_print_shell(&self, stdout : &mut Stdout);
}

pub enum ShellCommand {
    OUT(Box<dyn ShellDisplayable>),
    ERR(Box<dyn ShellDisplayable>),
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
        let mut stdout = io::stdout();
        // raw mode needed for arrow keys to be interpreted correctly
        terminal::enable_raw_mode();
        execute!(stdout, EnterAlternateScreen);
        let mut exit_shell = false;

        while !exit_shell {

            let prompt = self.app_env.supply_prompt();
            stdout.queue(style::PrintStyledContent(prompt.yellow()));
            stdout.flush();

            let mut col : u16 = 0;
            let mut row : u16 = 0;

            match cursor::position() {
                Ok((c, r)) => {
                    col = c;
                    row = r;
                }
                Err(_) => {}
            }
            let mut line_editor = LineEditor::new();
            let mut keep_reading = true;

            while keep_reading && !exit_shell {
                line_editor.flush();
                match read() {
                    Ok(evt) => {
                        match evt {
                            Event::Key(k) => {
                                match k.code {
                                    KeyCode::Backspace => {
                                        line_editor.backspace();
                                    }
                                    KeyCode::Enter => {
                                        keep_reading = false;
                                    }
                                    KeyCode::Left => {
                                        line_editor.left();
                                    }
                                    KeyCode::Right => {
                                        line_editor.right();
                                    }
                                    KeyCode::Up => {
                                    }
                                    KeyCode::Down => {
                                    }
                                    KeyCode::Home => {
                                        line_editor.home();
                                    }
                                    KeyCode::End => {
                                        line_editor.end();
                                    }
                                    KeyCode::PageUp => {}
                                    KeyCode::PageDown => {}
                                    KeyCode::Tab => {}
                                    KeyCode::BackTab => {}
                                    KeyCode::Delete => {
                                        line_editor.delete();
                                    }
                                    KeyCode::Insert => {}
                                    KeyCode::F(_) => {}
                                    KeyCode::Char(c) => {
                                        if ctrl(k) {
                                            if c == 'c' {
                                                exit_shell = true;
                                            }
                                        } else {
                                            line_editor.put(c);
                                        }
                                    }
                                    KeyCode::Null => {}
                                    KeyCode::Esc => {}
                                }
                            }
                            Event::Mouse(m) => {}
                            Event::Resize(w, h) => {
                                line_editor.update_term_size();
                            }
                        }
                    }
                    Err(_) => {}
                }
            }
            stdout.queue(Print("\n\r"));
            stdout.flush();

            if !exit_shell && line_editor.text().trim().len() > 0 {
                let command = self.app_env.handle_input(line_editor.text().as_str());
                match command {
                    ShellCommand::OUT(displayable) => {
                        displayable.pretty_print_shell(&mut stdout)
                    }
                    ShellCommand::ERR(displayable) => {
                        displayable.pretty_print_shell(&mut stdout)
                    }
                    ShellCommand::QUIT => {
                        break;
                    }
                }
                stdout.queue(Print("\n\r"));
            }

        }

        stdout.queue(PrintStyledContent("\n\nGoodbye!\n\n".yellow()));
        stdout.queue(MoveToColumn(0));
        stdout.flush();

        execute!(stdout, LeaveAlternateScreen);

        terminal::disable_raw_mode();
        Ok(())
    }
}

fn ctrl(k : KeyEvent) -> bool {
    k.modifiers.contains(KeyModifiers::CONTROL)
}

fn alt(k : KeyEvent) -> bool {
    k.modifiers.contains(KeyModifiers::ALT)
}

fn shift(k : KeyEvent) -> bool {
    k.modifiers.contains(KeyModifiers::SHIFT)
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

struct LineEditor {
    stdout : Stdout,
    buff : String,
    start_row : u16,
    start_col : u16,
    cursor_row : u16,
    cursor_col : u16,
    cursor_buff_idx : usize,
    term_cols : u16,
    term_rows : u16,
}

impl LineEditor {

    pub fn new() -> Self {
        let (start_col, start_row) = cursor::position().unwrap_or((0, 0));
        let (term_cols, term_rows) = terminal::size().unwrap_or((0,0));
        LineEditor {
            stdout : stdout(),
            buff: String::new(),
            start_row,
            start_col,
            cursor_row: start_row,
            cursor_col: start_col,
            cursor_buff_idx: 0,
            term_cols,
            term_rows
        }
    }

    pub fn update_term_size(&mut self) {
        let (term_cols, term_rows) = terminal::size().unwrap_or((0,0));
        self.term_cols = term_cols;
        self.term_rows = term_rows;
    }

    pub fn reset(&mut self) {
        self.buff.clear();
        self.cursor_buff_idx = 0;
        self.cursor_row = self.start_row;
        self.cursor_col = self.start_col;
        self.update_cursor_from_buffer_index();
    }

    pub fn flush(&mut self) {
        self.stdout.flush();
    }

    pub fn text(&self) -> String {
        String::from(self.buff.as_str())
    }

    pub fn put(&mut self, c : char) {
        if self.is_cursor_at_end() {
            self.buff.push(c);
        } else {
            self.buff.insert(self.cursor_buff_idx, c);
        }

        self.stdout.queue(PrintStyledContent(c.stylize()));
        self.cursor_buff_idx = self.cursor_buff_idx + 1;


        let mut substring = String::from(&self.buff.clone()[self.cursor_buff_idx..]);
        self.stdout.queue(PrintStyledContent(substring.stylize()));

        self.update_cursor_from_buffer_index();
    }

    pub fn backspace(&mut self) {
        if !self.is_cursor_at_start() {
            self.cursor_buff_idx = self.cursor_buff_idx - 1;
            self.buff.remove(self.cursor_buff_idx);

            let mut substring = String::from(&self.buff.clone()[self.cursor_buff_idx..]);
            substring.push(' '); // everything shifted left, so overwrite last deleted character
            self.update_cursor_from_buffer_index();
            self.stdout.queue(PrintStyledContent(substring.stylize()));

            self.update_cursor_from_buffer_index();
        }
    }

    pub fn delete(&mut self) {
        if !self.is_cursor_at_end() {
            self.buff.remove(self.cursor_buff_idx);

            let mut substring = String::from(&self.buff.clone()[self.cursor_buff_idx..]);
            substring.push(' '); // everything shifted left, so overwrite last deleted character
            self.stdout.queue(PrintStyledContent(substring.stylize()));

            self.update_cursor_from_buffer_index();
        }
    }

    pub fn is_cursor_at_start(&self) -> bool {
        self.cursor_buff_idx == 0
    }

    pub fn is_cursor_at_end(&self) -> bool {
        self.cursor_buff_idx == self.buff.len()
    }

    pub fn left(&mut self) {
        if !self.is_cursor_at_start() {
            self.cursor_buff_idx = self.cursor_buff_idx - 1;
            self.update_cursor_from_buffer_index();
        }
    }

    pub fn right(&mut self) {
        if !self.is_cursor_at_end() {
            self.cursor_buff_idx = self.cursor_buff_idx + 1;
            self.update_cursor_from_buffer_index();
        }
    }

    pub fn home(&mut self) {
        self.cursor_buff_idx = 0;
        self.update_cursor_from_buffer_index();
    }

    pub fn end(&mut self) {
        self.cursor_buff_idx = self.buff.len();
        self.update_cursor_from_buffer_index();
    }

    fn update_cursor_from_buffer_index(&mut self) {
        let first_row_width = self.term_cols - self.start_col;
        if self.cursor_buff_idx < first_row_width as usize {
            self.cursor_col = self.start_col + self.cursor_buff_idx as u16;
            self.cursor_row = self.start_row;
        } else {
            let extra_rows = (self.buff.len() as u16 - first_row_width) / self.term_cols;
            self.cursor_col = (self.buff.len() as u16 - (extra_rows * self.term_cols)) as u16;
            self.cursor_row = self.start_row + extra_rows;
        }

        self.stdout.queue(cursor::MoveTo(self.cursor_col, self.cursor_row));
    }
}
