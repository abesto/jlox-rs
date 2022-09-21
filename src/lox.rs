use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use thiserror::Error;

use crate::environment::Environment;
use crate::interpreter::{Interpreter, Value};
use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::types::ResolveErrorLocation;

#[derive(Debug, Error)]
pub enum Error {
    #[error("{}Scanning failed, see errors above.", .0.iter().map(|e| format!("{}\n", e)).collect::<String>())]
    Scanner(Vec<crate::scanner::Error>),

    #[error("{}Parsing failed, see errors above.", .0.iter().map(|e| format!("{}\n", e)).collect::<String>())]
    Parser(Vec<crate::parser::Error>),

    #[error(transparent)]
    Runtime(#[from] crate::interpreter::Error),

    #[error(transparent)]
    Io(#[from] std::io::Error),
}

type Result<T = (), E = Error> = std::result::Result<T, E>;

pub struct Lox {}

impl Lox {
    pub fn new() -> Self {
        Self {}
    }

    fn prepare_global_env() -> Rc<RefCell<Environment>> {
        let mut env = Environment::root();
        env.define(
            "clock",
            Some(Rc::new(RefCell::new(Value::NativeFunction {
                name: "clock".to_string(),
                arity: 0,
                fun: |_, _, _| {
                    Value::Number(
                        SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_secs_f64(),
                    )
                },
            }))),
        );

        env.define(
            "type",
            Some(Rc::new(RefCell::new(Value::NativeFunction {
                name: "type".to_string(),
                arity: 1,
                fun: |_, _, args| Value::String(args[0].borrow().type_of()),
            }))),
        );

        Rc::new(RefCell::new(env))
    }

    pub fn run_file(&mut self, path: &str) -> Result {
        let contents = std::fs::read(path)?;
        self.run(
            &mut Interpreter::new(),
            contents,
            Self::prepare_global_env(),
        )?;
        Ok(())
    }

    pub fn run_prompt(&mut self) -> Result<()> {
        let mut interpreter = Interpreter::new();
        let environment = Self::prepare_global_env();
        loop {
            print!("> ");
            std::io::stdout().flush()?;
            let mut line = String::new();
            if std::io::stdin().read_line(&mut line)? > 0 {
                match self.run(&mut interpreter, line.into_bytes(), Rc::clone(&environment)) {
                    Err(e) => {
                        eprintln!("{}", e);
                    }
                    Ok(Some(v)) => println!("{}", v),
                    Ok(None) => (),
                }
            } else {
                break;
            }
        }
        Ok(())
    }

    fn run(
        &mut self,
        interpreter: &mut Interpreter,
        source: Vec<u8>,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Option<Value>> {
        match Scanner::new(&source).scan_tokens() {
            Err(mut errors) => {
                for error in errors.iter_mut() {
                    error.resolve(&source);
                }
                Err(Error::Scanner(errors))
            }
            Ok(tokens) => match Parser::new(tokens).parse() {
                Err(mut errors) => {
                    for error in errors.iter_mut() {
                        error.resolve(&source);
                    }
                    Err(Error::Parser(errors))
                }
                Ok(ast) => match interpreter.interpret(&ast, env) {
                    Ok(value) => Ok(value.map(|v| v.borrow().clone())),
                    Err(mut e) => {
                        e.resolve(&source);
                        Err(Error::Runtime(e))
                    }
                },
            },
        }
    }
}
