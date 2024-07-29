use crate::scanner::{Scanner, ScannerMethods, Symbols};

mod scanner;
mod parser;


fn main() {

    let mut lexer = Scanner::new("code test end ", false);
    let symb = lexer.get_next_symbol();

    println!("Hello, world!");
}
