mod adt;
mod lox;
mod scanner;
mod token;
mod types;

use anyhow::Result;
use lox::Lox;

fn main() -> Result<()> {
    // Yes really manual parsing, if the book says to use a (Java) library I'll use Clap, until then we. do. it. by. The. BOOK.
    let args: Vec<String> = std::env::args().collect();

    #[allow(clippy::comparison_chain)]
    if args.len() > 2 {
        println!("Usage: {} [script]", args[0]);
        std::process::exit(64);
    } else if args.len() == 2 {
        Lox::new().run_file(&args[1]).unwrap();
    } else {
        Lox::new().run_prompt().unwrap();
    }

    Ok(())
}
