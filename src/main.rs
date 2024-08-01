mod scanner;
mod parser;

use inline_colorization::*;

use crate::scanner::{Scanner, ScannerMethods, Symbols};
use crate::parser::{Parser, ParseMethods, SyntaxNode};

const COMPILE_DATE_STRING: &str = compile_time::date_str!();

fn main() {

    let version = option_env!("PROJECT_VERSION").unwrap_or(env!("CARGO_PKG_VERSION"));

    let mut parser = Parser::new("code test end ", false);
    parser.advance();
    let symb = parser.parse_expression();

    println!("\r\n {color_bright_green}Active Oberon (2019 Revision){color_reset} Compiler, Version {version} [Build: {COMPILE_DATE_STRING}]\r\n");
}
