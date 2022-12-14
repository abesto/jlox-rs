use jlox_rs_lib::{Error, Lox};

fn main() {
    // Yes really manual parsing, if the book says to use a (Java) library I'll use Clap, until then we. do. it. by. The. BOOK.
    let args: Vec<String> = std::env::args().collect();

    #[allow(clippy::comparison_chain)]
    if args.len() > 2 {
        println!("Usage: {} [script]", args[0]);
        std::process::exit(64);
    } else if args.len() == 2 {
        if let Err(e) = Lox::new().run_file(&args[1], Box::new(std::io::stdout())) {
            eprintln!("{}", e);
            std::process::exit(match e {
                Error::Runtime(_) => 70,
                Error::Io(_) => 74,
                _ => 65,
            });
        }
    } else {
        Lox::new().run_prompt(Box::new(std::io::stdout())).unwrap();
    }
}
