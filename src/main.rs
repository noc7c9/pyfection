extern crate pyfection;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.get(1) {
        None => {
            println!("Usage: {} PATH", args[0]);
        },
        Some(filepath) => {
            print!("{}", pyfection::process_file(filepath));
            return;
        }
    }
}
