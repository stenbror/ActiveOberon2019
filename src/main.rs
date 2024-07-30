mod scanner;
mod parser;

use crate::scanner::{Scanner, ScannerMethods, Symbols};
use crate::parser::{Parser, ParseMethods, SyntaxNode};

fn main() {

    let mut parser = Parser::new("code test end ", false);
    parser.advance();

    println!("Hello, world!");
}
