use crate::scanner::{Scanner, ScannerMethods, Symbols};
use std::rc::Rc;

#[derive(Clone, PartialEq, Debug)]
pub enum SyntaxNode {
    None,

    Mul(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    Slash(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    Div(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    Mod(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    And(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    DotMul(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    DotSlash(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    BackSlash(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    Power(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    PlusMul(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
}

pub trait ParseMethods {
    fn new(text: &'static str, strict: bool) -> Self;
    fn advance(&mut self) -> ();

    fn parse_primary_expression(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)>;
    fn parse_unary_expression(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)>;
    fn parse_factor(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)>;
    fn parse_term(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)>;
    fn parse_simple_expression(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)>;
    fn parse_range_expression(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)>;
    fn parse_expression(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)>;
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

    fn parse_primary_expression(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)> {
        todo!()
    }

    fn parse_unary_expression(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)> {
        todo!()
    }

    fn parse_factor(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)> {
        todo!()
    }

    fn parse_term(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)> {
        let mut left = self.parse_factor()?;
        loop {
            match &self.symbol.clone()? {
                Symbols::Mul(p) => {
                    self.advance();
                    let right = self.parse_factor()?;
                    left = SyntaxNode::Mul(p.clone(), left.into(), right.into());
                },
                Symbols::Slash(p) => {
                    self.advance();
                    let right = self.parse_factor()?;
                    left = SyntaxNode::Slash(p.clone(), left.into(), right.into());
                },
                Symbols::Div(p) => {
                    self.advance();
                    let right = self.parse_factor()?;
                    left = SyntaxNode::Div(p.clone(), left.into(), right.into());
                },
                Symbols::Mod(p) => {
                    self.advance();
                    let right = self.parse_factor()?;
                    left = SyntaxNode::Mod(p.clone(), left.into(), right.into());
                },
                Symbols::And(p) => {
                    self.advance();
                    let right = self.parse_factor()?;
                    left = SyntaxNode::And(p.clone(), left.into(), right.into());
                },
                Symbols::DotMul(p) => {
                    self.advance();
                    let right = self.parse_factor()?;
                    left = SyntaxNode::DotMul(p.clone(), left.into(), right.into());
                },
                Symbols::DotSlash(p) => {
                    self.advance();
                    let right = self.parse_factor()?;
                    left = SyntaxNode::DotSlash(p.clone(), left.into(), right.into());
                },
                Symbols::BackSlash(p) => {
                    self.advance();
                    let right = self.parse_factor()?;
                    left = SyntaxNode::BackSlash(p.clone(), left.into(), right.into());
                },
                Symbols::Power(p) => {
                    self.advance();
                    let right = self.parse_factor()?;
                    left = SyntaxNode::Power(p.clone(), left.into(), right.into());
                },
                Symbols::PlusMul(p) => {
                    self.advance();
                    let right = self.parse_factor()?;
                    left = SyntaxNode::PlusMul(p.clone(), left.into(), right.into());
                },
                _ => break
            }
        }

        return Ok(left);
    }

    fn parse_simple_expression(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)> {
        todo!()
    }

    fn parse_range_expression(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)> {
        todo!()
    }

    fn parse_expression(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)> {
        todo!()
    }
}
