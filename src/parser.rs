use crate::scanner::{Scanner, ScannerMethods, Symbols};
use std::rc::Rc;

#[derive(Clone, PartialEq, Debug)]
pub enum SyntaxNode {
    None,

    Ident(usize, Box<String>),
    Number(usize, Box<String>),
    String(usize, Box<String>),
    Nil(usize),
    Imag(usize),
    True(usize),
    False(usize),
    _Self(usize),
    Result(usize),
    AliasOf(usize, Rc<SyntaxNode>),
    Alias(usize),
    SizeOf(usize, Rc<SyntaxNode>),
    Size(usize),
    AddressOf(usize, Rc<SyntaxNode>),
    Address(usize),

    DesignatorWithFlags(usize, Rc<SyntaxNode>, Rc<SyntaxNode>, Rc<SyntaxNode>),
    Designator(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    UnaryPlus(usize, Rc<SyntaxNode>),
    UnaryMinus(usize, Rc<SyntaxNode>),
    UnaryNot(usize, Rc<SyntaxNode>),
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
    Plus(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    Minus(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    Or(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    RangeMul(usize),
    Range(usize, Rc<SyntaxNode>, Rc<SyntaxNode>, Rc<SyntaxNode>),
    Equal(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    UnEqual(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    Less(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    LessEqual(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    Greater(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    GreaterEqual(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    In(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    Is(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    DotEqual(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    DotUnEqual(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    DotLess(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    DotLessEqual(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    DotGreater(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    DotGreaterEqual(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    QuestionMarkQuestionMark(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    ExclamationMarkExclamationMark(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    LessLessQ(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    GreaterGreaterQ(usize, Rc<SyntaxNode>, Rc<SyntaxNode>),
    DesignatorElements(usize, Rc<Vec<SyntaxNode>>),
    DesignatorDotName(usize, Box<String>),
    DesignatorTranspose(usize),
    DesignatorArrow(usize),
    DesignatorCall(usize, Rc<SyntaxNode>),
    DesignatorIndex(usize, Rc<SyntaxNode>),
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

    fn parse_flags(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)>;
    fn parse_designator_operations(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)>;
    fn parse_expression_list(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)>;
    fn parse_index_list(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)>;
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
        match &self.symbol.clone()? {
            Symbols::Nil(p) => {
                self.advance();
                return Ok(SyntaxNode::Nil(p.clone()));
            },
            Symbols::Imag(p) => {
                self.advance();
                return Ok(SyntaxNode::Imag(p.clone()));
            },
            Symbols::True(p) => {
                self.advance();
                return Ok(SyntaxNode::True(p.clone()));
            },
            Symbols::False(p) => {
                self.advance();
                return Ok(SyntaxNode::False(p.clone()));
            },
            Symbols::_Self(p) => {
                self.advance();
                return Ok(SyntaxNode::_Self(p.clone()));
            },
            Symbols::Result(p) => {
                self.advance();
                return Ok(SyntaxNode::Result(p.clone()));
            },
            Symbols::Address(p) => {
                self.advance();
                match &self.symbol.clone()? {
                    Symbols::Of(_) => {
                        self.advance();
                        let right = self.parse_factor()?;
                        return Ok(SyntaxNode::AddressOf(p.clone(), right.into()));
                    },
                    _ => {
                        return Ok(SyntaxNode::Address(p.clone()));
                    }
                }
            },
            Symbols::Alias(p) => {
                self.advance();
                match &self.symbol.clone()? {
                    Symbols::Of(_) => {
                        self.advance();
                        let right = self.parse_factor()?;
                        return Ok(SyntaxNode::AliasOf(p.clone(), right.into()));
                    },
                    _ => {
                        return Ok(SyntaxNode::Alias(p.clone()));
                    }
                }
            },
            Symbols::Size(p) => {
                self.advance();
                match &self.symbol.clone()? {
                    Symbols::Of(_) => {
                        self.advance();
                        let right = self.parse_factor()?;
                        return Ok(SyntaxNode::SizeOf(p.clone(), right.into()));
                    },
                    _ => {
                        return Ok(SyntaxNode::Size(p.clone()));
                    }
                }
            },
            Symbols::LeftParen(_) => {
                self.advance();
                let right = self.parse_expression()?;
                match &self.symbol.clone()? {
                    Symbols::RightParen(_) => {
                        self.advance();
                        return Ok(right);
                    },
                    _ => {
                        let (p, l) = self.lexer.get_location();
                        return Err( (Box::new(String::from("Missing ')' in exprfession!")), p, l) );
                    }
                }
            },
            Symbols::Ident(p, s) => {
                self.advance();
                return Ok(SyntaxNode::Ident(p.clone(), s.clone()));
            },
            Symbols::Number(p, s) => {
                self.advance();
                return Ok(SyntaxNode::Number(p.clone(), s.clone()));
            },
            Symbols::String(p, s) => {
                self.advance();
                return Ok(SyntaxNode::String(p.clone(), s.clone()));
            },
            _ => {
                    let (p, l) = self.lexer.get_location();
                    return Err( (Box::new(String::from("Unknown literal or missing literal!")), p, l) );
            }
        }
    }

    fn parse_unary_expression(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)> {
        let left = self.parse_primary_expression()?;
        match &self.symbol.clone()? {
            Symbols::LeftParen(p) | Symbols::LeftBracket(p) | Symbols::Period(p) | Symbols::Transpose(p) | Symbols::Arrow(p) => {
                let right = self.parse_designator_operations()?;
                match &self.symbol.clone()? {
                    Symbols::LeftCurly(_) => {
                        let flags = self.parse_flags()?;
                        return Ok(SyntaxNode::DesignatorWithFlags(p.clone(), left.into(), right.into(), flags.into()));
                    },
                    _ => {
                        return Ok(SyntaxNode::Designator(p.clone(), left.into(), right.into()));
                    }
                }
            },
            Symbols::LeftCurly(p) => {
                let flags = self.parse_flags()?;
                return Ok(SyntaxNode::DesignatorWithFlags(p.clone(), left.into(), Rc::new(SyntaxNode::None), flags.into()));
            },
            _ => {
                return Ok(left);
            }
        }
    }

    fn parse_factor(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)> {
        match &self.symbol.clone()? {
            Symbols::Plus(p) => {
                self.advance();
                let right = self.parse_unary_expression()?;
                return Ok(SyntaxNode::UnaryPlus(p.clone(), right.into()));
            },
            Symbols::Minus(p) => {
                self.advance();
                let right = self.parse_unary_expression()?;
                return Ok(SyntaxNode::UnaryMinus(p.clone(), right.into()));
            },
            Symbols::Not(p) => {
                self.advance();
                let right = self.parse_unary_expression()?;
                return Ok(SyntaxNode::UnaryNot(p.clone(), right.into()));
            },
            _ => { return self.parse_unary_expression(); }
        }
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
        let mut left = self.parse_term()?;
        loop {
            match &self.symbol.clone()? {
                Symbols::Plus(p) => {
                    self.advance();
                    let right = self.parse_term()?;
                    left = SyntaxNode::Plus(p.clone(), left.into(), right.into());
                },
                Symbols::Minus(p) => {
                    self.advance();
                    let right = self.parse_term()?;
                    left = SyntaxNode::Minus(p.clone(), left.into(), right.into());
                },
                Symbols::Or(p) => {
                    self.advance();
                    let right = self.parse_term()?;
                    left = SyntaxNode::Or(p.clone(), left.into(), right.into());
                },
                _=> break
            }
        }
        return Ok(left);
    }

    fn parse_range_expression(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)> {
        let mut left = SyntaxNode::None;
        let mut right = SyntaxNode::None;
        let mut next = SyntaxNode::None;
        match  &self.symbol.clone()? {
            Symbols::Mul(p) => {
                self.advance();
                return Ok(SyntaxNode::RangeMul(p.clone()));
            },
            Symbols::UpTo(p) => {
                self.advance();
                match &self.symbol.clone()? {
                    Symbols::By(_) | Symbols::RightCurly(_) | Symbols::Comma(_) => {
                        right = self.parse_simple_expression()?;
                    },
                    _ => ()
                }
                match &self.symbol.clone()? {
                    Symbols::By(_) => {
                        self.advance();
                        next = self.parse_simple_expression()?;
                    },
                    _ => ()
                }
                return Ok(SyntaxNode::Range(p.clone(), left.into(), right.into(), next.into()));
            }
            _ => {
                left = self.parse_simple_expression()?;
                match &self.symbol.clone()? {
                    Symbols::UpTo(p) => {
                        self.advance();
                        match &self.symbol.clone()? {
                            Symbols::By(_) | Symbols::RightCurly(_) | Symbols::Comma(_) => {
                                right = self.parse_simple_expression()?;
                            },
                            _ => ()
                        }
                        match &self.symbol.clone()? {
                            Symbols::By(_) => {
                                self.advance();
                                next = self.parse_simple_expression()?;
                            },
                            _ => ()
                        }
                        return Ok(SyntaxNode::Range(p.clone(), left.into(), right.into(), next.into()));
                    },
                    _ => { 
                        return Ok(left);
                    }
                }
            }
        }
    }

    fn parse_expression(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)> {
        let mut left = self.parse_range_expression()?;
        match &self.symbol.clone()? {
            Symbols::Equal(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::Equal(p.clone(), left.into(), right.into()));
            },
            Symbols::UnEqual(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::UnEqual(p.clone(), left.into(), right.into()));
            },
            Symbols::Less(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::Less(p.clone(), left.into(), right.into()));
            },
            Symbols::LessEqual(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::LessEqual(p.clone(), left.into(), right.into()));
            },
            Symbols::Greater(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::Greater(p.clone(), left.into(), right.into()));
            },
            Symbols::GreaterEqual(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::GreaterEqual(p.clone(), left.into(), right.into()));
            },
            Symbols::In(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::In(p.clone(), left.into(), right.into()));
            },
            Symbols::Is(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::Is(p.clone(), left.into(), right.into()));
            },
            Symbols::DotEqual(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::DotEqual(p.clone(), left.into(), right.into()));
            },
            Symbols::DotUnEqual(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::DotUnEqual(p.clone(), left.into(), right.into()));
            },
            Symbols::DotLess(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::DotLess(p.clone(), left.into(), right.into()));
            },
            Symbols::DotLessEqual(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::DotLessEqual(p.clone(), left.into(), right.into()));
            },
            Symbols::DotGreater(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::DotGreater(p.clone(), left.into(), right.into()));
            },
            Symbols::DotGreaterEqual(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::DotGreaterEqual(p.clone(), left.into(), right.into()));
            },
            Symbols::QuestionMarkQuestionMark(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::QuestionMarkQuestionMark(p.clone(), left.into(), right.into()));
            },
            Symbols::ExclamationMarkExclamationMark(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::ExclamationMarkExclamationMark(p.clone(), left.into(), right.into()));
            },
            Symbols::LessLessQ(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::LessLessQ(p.clone(), left.into(), right.into()));
            },
            Symbols::GreaterGreaterQ(p) => {
                self.advance();
                let right = self.parse_range_expression()?;
                return Ok(SyntaxNode::GreaterGreaterQ(p.clone(), left.into(), right.into()));
            },
            _ => { return Ok(left); }
        }
    }

    fn parse_flags(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)> {
        todo!()
    }

    fn parse_designator_operations(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)> {
        let mut nodes = Vec::<SyntaxNode>::new();
        let ( pos, _ ) = self.lexer.get_location();

        loop {
            match &self.symbol.clone()? {
                Symbols::Transpose(p) => {
                    self.advance();
                    nodes.push(SyntaxNode::DesignatorTranspose(p.clone()));
                },
                Symbols::Arrow(p) => {
                    self.advance();
                    nodes.push(SyntaxNode::DesignatorArrow(p.clone()));
                },
                Symbols::Period(p) => {
                    self.advance();
                    match &self.symbol.clone()? {
                        Symbols::Ident(p, s) => {
                            self.advance();
                            nodes.push(SyntaxNode::DesignatorDotName(p.clone(), s.clone()));
                        },
                        _ => {
                            let (p, l) = self.lexer.get_location();
                            return Err( (Box::new(String::from("Expecting Literal ident after '.' in designator!")), p, l) );
                        }
                    }
                },
                Symbols::LeftParen(p) => {
                    self.advance();
                    let right = match &self.symbol.clone()? {
                        Symbols::RightParen(_) => SyntaxNode::None,
                        _ => { self.parse_expression_list()? }
                    };
                    match &self.symbol.clone()? {
                        Symbols::RightParen(_) => {
                            self.advance();
                            nodes.push(SyntaxNode::DesignatorCall(p.clone(), right.into()))
                        },
                        _ => {
                            let (p, l) = self.lexer.get_location();
                            return Err( (Box::new(String::from("Expecting ')' in call designator!")), p, l) );
                        }
                    }
                },
                Symbols::LeftBracket(p) => {
                    self.advance();
                    let right = match &self.symbol.clone()? {
                        Symbols::RightBracket(_) => SyntaxNode::None,
                        _ => { self.parse_index_list()? }
                    };
                    match &self.symbol.clone()? {
                        Symbols::RightBracket(_) => {
                            self.advance();
                            nodes.push(SyntaxNode::DesignatorIndex(p.clone(), right.into()))
                        },
                        _ => {
                            let (p, l) = self.lexer.get_location();
                            return Err( (Box::new(String::from("Expecting ')' in call designator!")), p, l) );
                        }
                    }
                },
                _ => {
                    break;
                }
            }
        }

        return Ok(SyntaxNode::DesignatorElements(pos.clone(), Rc::new(nodes.into())));
    }

    fn parse_expression_list(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)> {
        todo!()
    }

    fn parse_index_list(&mut self) -> Result<SyntaxNode, (Box<std::string::String>, usize, usize)> {
        todo!()
    }
}



#[cfg(test)]
mod tests {

    use std::rc::Rc;

    use crate::scanner::{Scanner, ScannerMethods, Symbols};
    use crate::parser::{Parser, ParseMethods, SyntaxNode};

    #[test]
    fn primary_expression_nil() {
        let mut parser = Parser::new("nil", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::Nil(0), s);
            },
            _ => {
                assert!(false);
            }
        }
    }

    #[test]
    fn primary_expression_imag() {
        let mut parser = Parser::new("IMAG", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::Imag(0), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }

    #[test]
    fn primary_expression_true() {
        let mut parser = Parser::new("true", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::True(0), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }

    #[test]
    fn primary_expression_false() {
        let mut parser = Parser::new("FALSE", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::False(0), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }

    #[test]
    fn primary_expression_self() {
        let mut parser = Parser::new("self", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::_Self(0), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }

    #[test]
    fn primary_expression_result() {
        let mut parser = Parser::new("RESULT", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::Result(0), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }

    #[test]
    fn primary_expression_alias() {
        let mut parser = Parser::new("ALIAS", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::Alias(0), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }

    #[test]
    fn primary_expression_alias_of() {
        let mut parser = Parser::new("ALIAS OF test", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::AliasOf(0, Rc::new( SyntaxNode::Ident(9, Box::new(String::from("test"))) )), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }

    #[test]
    fn primary_expression_address() {
        let mut parser = Parser::new("address", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::Address(0), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }

    #[test]
    fn primary_expression_address_of() {
        let mut parser = Parser::new("address OF test", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::AddressOf(0, Rc::new( SyntaxNode::Ident(11, Box::new(String::from("test"))) )), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }

    #[test]
    fn primary_expression_sizeof() {
        let mut parser = Parser::new("size", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::Size(0), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }

    #[test]
    fn primary_expression_sizeof_of() {
        let mut parser = Parser::new("size of test", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::SizeOf(0, Rc::new( SyntaxNode::Ident(8, Box::new(String::from("test"))) )), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }

    #[test]
    fn primary_literal_ident() {
        let mut parser = Parser::new("variable_1", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::Ident(0, Box::new(String::from("variable_1"))), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }

    #[test]
    fn primary_literal_ident_parenthesis() {
        let mut parser = Parser::new("(variable_1)", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::Ident(1, Box::new(String::from("variable_1"))), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }

    #[test]
    fn factor_unary_plus() {
        let mut parser = Parser::new("+a", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::UnaryPlus(0, Rc::new(SyntaxNode::Ident(1, Box::new(String::from("a"))))), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }

    #[test]
    fn factor_unary_minus() {
        let mut parser = Parser::new("-a", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::UnaryMinus(0, Rc::new(SyntaxNode::Ident(1, Box::new(String::from("a"))))), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }

    #[test]
    fn factor_unary_not() {
        let mut parser = Parser::new("~a", false);
        parser.advance();
        let res = parser.parse_expression();

        match res {
            Ok(s) => {
                assert_eq!(SyntaxNode::UnaryNot(0, Rc::new(SyntaxNode::Ident(1, Box::new(String::from("a"))))), s);
            },
            _ => {
                assert!(false);
            }
        } 
    }
}