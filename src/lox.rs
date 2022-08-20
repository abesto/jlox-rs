use std::io::Write;

use anyhow::{bail, Result};

use crate::parser::Parser;
use crate::scanner::Scanner;

pub struct Lox {
    had_error: bool,
}

impl Lox {
    pub fn new() -> Self {
        Self { had_error: false }
    }

    pub fn run_file(&mut self, path: &str) -> Result<()> {
        let contents = std::fs::read(path)?;
        self.run(contents)?;
        if self.had_error {
            std::process::exit(65);
        }
        Ok(())
    }

    pub fn run_prompt(&mut self) -> Result<()> {
        loop {
            print!("> ");
            std::io::stdout().flush()?;
            let mut line = String::new();
            if std::io::stdin().read_line(&mut line)? > 0 {
                if let Err(e) = self.run(line.into_bytes()) {
                    eprintln!("{}", e);
                }
            } else {
                break;
            }
        }
        Ok(())
    }

    fn run(&mut self, source: Vec<u8>) -> Result<()> {
        match Scanner::new(&source).scan_tokens() {
            Err(errors) => {
                for error in errors {
                    eprintln!("{}\n", error);
                }
                bail!("Scanning failed, see errors above.");
            }
            Ok(tokens) => match Parser::new(&source, tokens).parse() {
                Err(errors) => {
                    for error in errors {
                        eprintln!("{}\n", error);
                    }
                    bail!("Parsing failed, see errors above.");
                }
                Ok(ast) => {
                    println!("{:#?}", ast);
                    Ok(())
                }
            },
        }
    }
}
