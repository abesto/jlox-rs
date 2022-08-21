use std::io::Write;

use anyhow::{anyhow, Result};

use crate::interpreter::Interpreter;
use crate::parser::Parser;
use crate::scanner::Scanner;

// TODO write basic smoke tests by driving the interpreter with streams
// TODO get rid of had_error and had_runtime_error
// TODO it'd be better to not pass &source in to Parser and Interpreter, and instead translate offsets
//      back to line / char here

pub struct Lox {
    had_error: bool,
    had_runtime_error: bool,
}

impl Lox {
    pub fn new() -> Self {
        Self {
            had_error: false,
            had_runtime_error: false,
        }
    }

    pub fn run_file(&mut self, path: &str) -> Result<()> {
        let contents = std::fs::read(path)?;
        self.run(&mut Interpreter::new(), contents)?;
        if self.had_error {
            std::process::exit(65);
        }
        if self.had_runtime_error {
            std::process::exit(70);
        }
        Ok(())
    }

    pub fn run_prompt(&mut self) -> Result<()> {
        let mut interpreter = Interpreter::new();
        loop {
            print!("> ");
            std::io::stdout().flush()?;
            self.had_error = false;
            self.had_runtime_error = false;
            let mut line = String::new();
            if std::io::stdin().read_line(&mut line)? > 0 {
                if let Err(e) = self.run(&mut interpreter, line.into_bytes()) {
                    eprintln!("{}", e);
                }
            } else {
                break;
            }
        }
        Ok(())
    }

    fn run(&mut self, interpreter: &mut Interpreter, source: Vec<u8>) -> Result<()> {
        match Scanner::new(&source).scan_tokens() {
            Err(errors) => {
                for error in errors {
                    eprintln!("{}", error);
                }
                self.had_error = true;
                Err(anyhow!("Scanning failed, see errors above."))
            }
            Ok(tokens) => match Parser::new(&source, tokens).parse() {
                Err(errors) => {
                    for error in errors {
                        eprintln!("{}", error);
                    }
                    self.had_error = true;
                    Err(anyhow!("Parsing failed, see errors above."))
                }
                Ok(ast) => match interpreter.interpret(source, &ast) {
                    Ok(()) => Ok(()),
                    Err(e) => {
                        self.had_runtime_error = true;
                        Err(anyhow!(e))
                    }
                },
            },
        }
    }
}
