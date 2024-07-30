use crate::scanner::{Scanner, ScannerMethods, Symbols};


#[derive(Clone, PartialEq, Debug)]
pub enum SyntaxNode {
    None
}

pub trait ParseMethods {
    fn new(text: &'static str, strict: bool) -> Self;
    fn advance(&mut self) -> ();
}

pub struct Parser {
    lexer: Scanner,
    symbol: Result<Symbols, (Box<std::string::String>, usize, usize)>
}

impl ParseMethods for Parser {

    fn new(text: &'static str, strict: bool) -> Parser {
        Parser {
            lexer: Scanner::new(text, strict),
            symbol: Ok(Symbols::EndOfFile)
        }
    }

    fn advance(&mut self) -> () {
        self.symbol = self.lexer.get_next_symbol()
    }
}
