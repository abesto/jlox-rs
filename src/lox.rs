use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use thiserror::Error;

use crate::environment::Environment;
use crate::interpreter::{Interpreter, Value};
use crate::parser::Parser;
use crate::resolver::Resolver;
use crate::scanner::Scanner;
use crate::types::ErrorLocationResolver;

#[derive(Debug, Error)]
pub enum Error {
    #[error("{}Scanning failed, see errors above.", .0.iter().map(|e| format!("{}\n", e)).collect::<String>())]
    Scanner(Vec<crate::scanner::Error>),

    #[error("{}Parsing failed, see errors above.", .0.iter().map(|e| format!("{}\n", e)).collect::<String>())]
    Parser(Vec<crate::parser::Error>),

    #[error("{}Variable resolution failed, see errors above.", .0.iter().map(|e| format!("{}\n", e)).collect::<String>())]
    Resolver(Vec<crate::resolver::Error>),

    // Box because Clippy says the sizes of variants is otherwise large
    #[error(transparent)]
    Runtime(#[from] Box<crate::interpreter::Error>),

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
                interpreter.command += 1;
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
        let error_location_resolver = ErrorLocationResolver::new(&source);

        let tokens = Scanner::new(&source, interpreter.command)
            .scan_tokens()
            .map_err(|e| error_location_resolver.resolve(e))
            .map_err(Error::Scanner)?;

        let program = Parser::new(tokens)
            .parse()
            .map_err(|e| error_location_resolver.resolve(e))
            .map_err(Error::Parser)?;

        let bindings = Resolver::new()
            .resolve(&program)
            .map_err(|e| error_location_resolver.resolve(e))
            .map_err(Error::Resolver)?;
        interpreter.update_bindings(bindings);

        interpreter
            .interpret(&program, env)
            .map_err(|e| error_location_resolver.resolve(e))
            .map_err(|e| Error::Runtime(Box::new(e)))
            .map(|opt| opt.map(|v| v.borrow().clone()))
    }
}
