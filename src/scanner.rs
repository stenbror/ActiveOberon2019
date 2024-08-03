
/*
scanner reflects the following EBNF
		Symbol 				= String | Token | Number | Keyword | Identifier.
		Token 				=  | '#' | '&' | '(' ['*' any '*' ')'] | ')' | '*'['*'] | '+'['*'] | ',' | '-' | '.' [ '.' | '*' | '/' | '=' | '#' | '>'['='] | '<' ['=']
											  | '/' | ':' ['='] | ';'  | '<' ['=' | '<' ['?'] ] | '=' | '>' [ '=' | '>' ['?']]  
											  | '[' | ']' | '^' | '{' | '|' | '}' | '~' | '\' |  '`' |  '?' ['?'] | '!' ['!']
		Identifier			= Letter {Letter | Digit | '_'}.
		Letter					= 'A' | 'B' | .. | 'Z' | 'a' | 'b' | .. | 'z'.
		Digit						= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' .
		String					= '"' {Character} '"' | "'" {Character} "'".
		Character			= Digit [HexDigit] 'X'.
		Number				= Integer | Real.
		Integer				= Digit {Digit} | Digit {HexDigit} 'H' | '0x' {HexDigit}.
		Real						= Digit {Digit} '.' {Digit} [ScaleFactor].
		ScaleFactor		= ('E' | 'D') ['+' | '-'] digit {digit}.
		HexDigit			= Digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'.
*/

#[derive(Clone, PartialEq, Debug)]
pub enum Symbols {

    /* Reserved keywords */
    Await(usize),
    Begin(usize),
    By(usize),
    Const(usize),
    Case(usize),
    Cell(usize),
    CellNet(usize),
    Code(usize),
    CodeText(usize, Box<String>),
    Definition(usize),
    Do(usize),
    Div(usize),
    End(usize),
    Enum(usize),
    Else(usize),
    Elsif(usize),
    Exit(usize),
    Extern(usize),
    False(usize),
    For(usize),
    Finally(usize),
    If(usize),
    Ignore(usize),
    Imag(usize),
    In(usize),
    Is(usize),
    Import(usize),
    Loop(usize),
    Module(usize),
    Mod(usize),
    Nil(usize),
    Of(usize),
    Or(usize),
    Out(usize),
    Operator(usize),
    Procedure(usize),
    Port(usize),
    Repeat(usize),
    Return(usize),
    _Self(usize),
    New(usize),
    Result(usize),
    Then(usize),
    True(usize),
    To(usize),
    Type(usize),
    Until(usize),
    Var(usize),
    While(usize),
    With(usize),

    /* Types */
    Any(usize),
    Array(usize),
    Object(usize),
    Pointer(usize),
    Record(usize),
    Address(usize),
    Size(usize),
    Alias(usize),

    /* Operators or delimiters */
    UnEqual(usize), // #
    And(usize), // &
    LeftParen(usize), // (
    RightParen(usize), // )
    Mul(usize), // *
    Power(usize), // **
    Plus(usize), // +
    PlusMul(usize), // +*
    Comma(usize), // ,
    Minus(usize), // -
    Period(usize), // .
    UpTo(usize), // ..
    DotMul(usize), // .*
    DotPlus(usize), // .+
    DotSlash(usize), // ./
    DotEqual(usize), // .=
    DotUnEqual(usize), // .#
    DotGreater(usize), // .>
    DotGreaterEqual(usize), // .>=
    DotLess(usize), // .<
    DotLessEqual(usize), // .<=
    Slash(usize), // /
    Colon(usize), // :
    Becomes(usize), // :=
    SemiColon(usize), // ;
    Less(usize), // <
    LessEqual(usize), // <=
    Equal(usize), // =
    Greater(usize), // >
    GreaterEqual(usize), // >=
    LeftBracket(usize), // [
    RightBracket(usize), // ]
    Bar(usize), // |
    Arrow(usize), // ^
    LeftCurly(usize), // {
    RightCurly(usize), // }
    BackSlash(usize), // \
    Not(usize), // ~
    Transpose(usize), // `
    QuestionMark(usize), // ?
    QuestionMarkQuestionMark(usize), // ??
    ExclamationMark(usize), // !
    ExclamationMarkExclamationMark(usize), // !!
    LessLess(usize), // <<
    LessLessQ(usize), // <<?
    GreaterGreater(usize), // >>
    GreaterGreaterQ(usize), // >>?
    GreatterGreaterQ(usize), // >>?

    /* Literals */
    Ident(usize, Box<String>),
    Number(usize, Box<String>),
    String(usize, Box<String>),
    Char(usize),

    /* System */
    EndOfFile
}

pub trait ScannerMethods {
    fn new(text: &'static str, strict: bool) -> Self;
    fn get_char(&mut self) -> char;
    fn next_char(&mut self) -> ();
    fn get_next_symbol(&mut self) -> Result<Symbols, (Box<std::string::String>, usize, usize)>;
    fn is_capitalize_reserved_keyword(&self, text: &str, pos: usize) -> Option<Symbols>;
    fn is_reserved_keyword(&self, text: &str, pos: usize) -> Option<Symbols>;
    fn get_location(&self) -> (usize, usize);
}



pub struct Scanner {
    source: Vec<char>,
    position: usize,
    lineno: usize,
    strict: bool
}

impl ScannerMethods for Scanner {


    fn new(text: &'static str, strict: bool) -> Scanner {
        Scanner {
        source: text.chars().collect(),
        position: 0,
        lineno: 1,
        strict: strict
        }
    }

    fn get_char(&mut self) -> char {
        if self.position >= self.source.len() { return '\0'; } 
        let ch = self.source[self.position];
        if ch == '\r' || ch == '\n' { /* Handle lineshift and increment lineno */
            if ch == '\n' {
                self.lineno = self.lineno + 1;
                self.next_char();
                return self.get_char();
            }
            else {
                self.lineno = self.lineno + 1;
                self.next_char();
                if self.position >= self.source.len() { return '\0'; } 
                let ch2 = self.source[self.position];
                if ch2 == '\n' {
                    self.next_char();
                }
                return self.get_char();
            }
        }
        return ch;
    }

    fn next_char(&mut self) -> () {
        self.position = self.position + 1
    }

    fn is_capitalize_reserved_keyword(&self, text: &str, pos: usize) -> Option<Symbols> {
        match text {
            "CELL" => Some(Symbols::Cell(pos)),
            "CELLNET" => Some(Symbols::CellNet(pos)),
            "AWAIT" => Some(Symbols::Await(pos)),
            "BEGIN" => Some(Symbols::Begin(pos)),
            "BY" => Some(Symbols::By(pos)),
            "CONST" => Some(Symbols::Const(pos)),
            "CASE" => Some(Symbols::Case(pos)),
            "CODE" => Some(Symbols::Code(pos)),
            "DEFINITION" => Some(Symbols::Definition(pos)),
            "DO" => Some(Symbols::Do(pos)),
            "DIV" => Some(Symbols::Div(pos)),
            "END" => Some(Symbols::End(pos)),
            "ENUM" => Some(Symbols::Enum(pos)),
            "ELSE" => Some(Symbols::Else(pos)),
            "ELSIF" => Some(Symbols::Elsif(pos)),
            "EXIT" => Some(Symbols::Exit(pos)),
            "EXTERN" => Some(Symbols::Extern(pos)),
            "FALSE" => Some(Symbols::False(pos)),
            "FOR" => Some(Symbols::For(pos)),
            "FINALLY" => Some(Symbols::Finally(pos)),
            "IF" => Some(Symbols::If(pos)),
            "IMAG" => Some(Symbols::Imag(pos)),
            "IN" => Some(Symbols::In(pos)),
            "IS" => Some(Symbols::Is(pos)),
            "IMPORT" => Some(Symbols::Import(pos)),
            "LOOP" => Some(Symbols::Loop(pos)),
            "MODULE" => Some(Symbols::Module(pos)),
            "MOD" => Some(Symbols::Mod(pos)),
            "NIL" => Some(Symbols::Nil(pos)),
            "OF" => Some(Symbols::Of(pos)),
            "OR" => Some(Symbols::Or(pos)),
            "OUT" => Some(Symbols::Out(pos)),
            "OPERATOR" => Some(Symbols::Operator(pos)),
            "PROCEDURE" => Some(Symbols::Procedure(pos)),
            "PORT" => Some(Symbols::Port(pos)),
            "REPEAT" => Some(Symbols::Repeat(pos)),
            "RETURN" => Some(Symbols::Return(pos)),
            "SELF" => Some(Symbols::_Self(pos)),
            "RESULT" => Some(Symbols::Result(pos)),
            "THEN" => Some(Symbols::Then(pos)),
            "TRUE" => Some(Symbols::True(pos)),
            "TO" => Some(Symbols::To(pos)),
            "TYPE" => Some(Symbols::Type(pos)),
            "UNTIL" => Some(Symbols::Until(pos)),
            "VAR" => Some(Symbols::Var(pos)),
            "WHILE" => Some(Symbols::While(pos)),
            "WITH" => Some(Symbols::With(pos)),
            "ARRAY" => Some(Symbols::Array(pos)),
            "OBJECT" => Some(Symbols::Object(pos)),
            "POINTER" => Some(Symbols::Pointer(pos)),
            "RECORD" => Some(Symbols::Record(pos)),
            "ADDRESS" => Some(Symbols::Address(pos)),
            "SIZE" => Some(Symbols::Size(pos)),
            "ALIAS" => Some(Symbols::Alias(pos)),
            _ => None
        }
    }

    fn is_reserved_keyword(&self, text: &str, pos: usize) -> Option<Symbols> {
        match text {
            "cell" => Some(Symbols::Cell(pos)),
            "cellnet" => Some(Symbols::CellNet(pos)),
            "await" => Some(Symbols::Await(pos)),
            "begin" => Some(Symbols::Begin(pos)),
            "by" => Some(Symbols::By(pos)),
            "const" => Some(Symbols::Const(pos)),
            "case" => Some(Symbols::Case(pos)),
            "code" => Some(Symbols::Code(pos)),
            "definition" => Some(Symbols::Definition(pos)),
            "do" => Some(Symbols::Do(pos)),
            "div" => Some(Symbols::Div(pos)),
            "end" => Some(Symbols::End(pos)),
            "enum" => Some(Symbols::Enum(pos)),
            "else" => Some(Symbols::Else(pos)),
            "elsif" => Some(Symbols::Elsif(pos)),
            "exit" => Some(Symbols::Exit(pos)),
            "extern" => Some(Symbols::Extern(pos)),
            "false" => Some(Symbols::False(pos)),
            "for" => Some(Symbols::For(pos)),
            "finally" => Some(Symbols::Finally(pos)),
            "if" => Some(Symbols::If(pos)),
            "imag" => Some(Symbols::Imag(pos)),
            "in" => Some(Symbols::In(pos)),
            "is" => Some(Symbols::Is(pos)),
            "import" => Some(Symbols::Import(pos)),
            "loop" => Some(Symbols::Loop(pos)),
            "module" => Some(Symbols::Module(pos)),
            "mod" => Some(Symbols::Mod(pos)),
            "nil" => Some(Symbols::Nil(pos)),
            "of" => Some(Symbols::Of(pos)),
            "or" => Some(Symbols::Or(pos)),
            "out" => Some(Symbols::Out(pos)),
            "operator" => Some(Symbols::Operator(pos)),
            "procedure" => Some(Symbols::Procedure(pos)),
            "port" => Some(Symbols::Port(pos)),
            "repeat" => Some(Symbols::Repeat(pos)),
            "return" => Some(Symbols::Return(pos)),
            "self" => Some(Symbols::_Self(pos)),
            "result" => Some(Symbols::Result(pos)),
            "then" => Some(Symbols::Then(pos)),
            "true" => Some(Symbols::True(pos)),
            "to" => Some(Symbols::To(pos)),
            "type" => Some(Symbols::Type(pos)),
            "until" => Some(Symbols::Until(pos)),
            "var" => Some(Symbols::Var(pos)),
            "while" => Some(Symbols::While(pos)),
            "with" => Some(Symbols::With(pos)),
            "array" => Some(Symbols::Array(pos)),
            "object" => Some(Symbols::Object(pos)),
            "pointer" => Some(Symbols::Pointer(pos)),
            "record" => Some(Symbols::Record(pos)),
            "address" => Some(Symbols::Address(pos)),
            "size" => Some(Symbols::Size(pos)),
            "alias" => Some(Symbols::Alias(pos)),
            _ => None
        }
    }

    fn get_next_symbol(&mut self) -> Result<Symbols, (Box<std::string::String>, usize, usize)> {
 
        /* Remove whitespace */
        while self.get_char() == ' ' || self.get_char() == '\t' { self.next_char(); }

        let pos_symb = self.position; /* Set start of symbol */

        match self.get_char() {
            '\0' => { return Ok(Symbols::EndOfFile); }
            '#' => { self.next_char(); return Ok(Symbols::UnEqual(pos_symb)); }
            '|' => { self.next_char(); return Ok(Symbols::Bar(pos_symb)); }
            ',' => { self.next_char(); return Ok(Symbols::Comma(pos_symb)); }
            '-' => { self.next_char(); return Ok(Symbols::Minus(pos_symb)); } 
            '~' => { self.next_char(); return Ok(Symbols::Not(pos_symb)); }
            '/' => { self.next_char(); return Ok(Symbols::Slash(pos_symb)); }
            '=' => { self.next_char(); return Ok(Symbols::Equal(pos_symb)); }
            ')' => { self.next_char(); return Ok(Symbols::RightParen(pos_symb)); }
            '[' => { self.next_char(); return Ok(Symbols::LeftBracket(pos_symb)); }
            ']' => { self.next_char(); return Ok(Symbols::RightBracket(pos_symb)); }
            '{' => { self.next_char(); return Ok(Symbols::LeftCurly(pos_symb)); }
            '}' => { self.next_char(); return Ok(Symbols::RightCurly(pos_symb)); }
            '^' => { self.next_char(); return Ok(Symbols::Arrow(pos_symb)); }
            '`' => { self.next_char(); return Ok(Symbols::Transpose(pos_symb)); }
            ';' => { self.next_char(); return Ok(Symbols::SemiColon(pos_symb)); }
            '&' => { self.next_char(); return Ok(Symbols::And(pos_symb)); }
            '+' => { 
                self.next_char(); 
                if self.get_char() == '*' {
                    self.next_char(); 
                    return Ok(Symbols::PlusMul(pos_symb)); }
                
                return Ok(Symbols::Plus(pos_symb)); 
            }
            '(' => {
                self.next_char();
                if self.get_char() == '*' {
                    self.next_char();
                    let mut level = 1;
                    loop {
                        match self.get_char() {
                            '*' => {
                                self.next_char();
                                if self.get_char() == ')' {
                                    self.next_char();
                                    level = level - 1;
                                }
                            },
                            '(' => {
                                self.next_char();
                                if self.get_char() == '*' {
                                    self.next_char();
                                    level = level + 1;
                                }
                            },
                            '\0' => {
                                return Err((Box::new(String::from("Unterminated comment in source code. Missing '*)'")), self.position, self.lineno));
                            }, _ => {
                                self.next_char();
                            }
                        }
                        if level == 0 { return self.get_next_symbol(); }
                    }
                }
                return Ok(Symbols::LeftParen(pos_symb));
            },
            ':' => {
                self.next_char();
                if self.get_char() == '=' {
                    self.next_char();
                    return Ok(Symbols::Becomes(pos_symb));
                }
                return Ok(Symbols::Colon(pos_symb));
            },
            '*' => {
                self.next_char();
                if self.get_char() == '*' {
                    self.next_char();
                    return Ok(Symbols::Power(pos_symb));
                }
                return Ok(Symbols::Mul(pos_symb));
            },
            '<' => {
                self.next_char();
                if self.get_char() == '=' {
                    self.next_char();
                    return Ok(Symbols::LessEqual(pos_symb));
                }
                else if self.get_char() == '<' {
                    self.next_char();
                    if self.get_char() == '?' {
                        self.next_char();
                        return Ok(Symbols::LessLessQ(pos_symb));
                    }
                    return Ok(Symbols::LessLess(pos_symb));
                }
                return Ok(Symbols::Less(pos_symb));
            },
            '>' => {
                self.next_char();
                if self.get_char() == '=' {
                    self.next_char();
                    return Ok(Symbols::GreaterEqual(pos_symb));
                }
                else if self.get_char() == '>' {
                    self.next_char();
                    if self.get_char() == '?' {
                        self.next_char();
                        return Ok(Symbols::GreaterGreaterQ(pos_symb));
                    }
                    return Ok(Symbols::GreaterGreater(pos_symb));
                }
                return Ok(Symbols::Greater(pos_symb));
            },
            '?' => {
                self.next_char();
                if self.get_char() == '?' {
                    self.next_char();
                    return Ok(Symbols::QuestionMarkQuestionMark(pos_symb));
                }
                return Ok(Symbols::QuestionMark(pos_symb));
            },
            '!' => {
                self.next_char();
                if self.get_char() == '!' {
                    self.next_char();
                    return Ok(Symbols::ExclamationMarkExclamationMark(pos_symb));
                }
                return Ok(Symbols::ExclamationMark(pos_symb));
            },
            '\\' => {
                self.next_char();
                if self.get_char() == '"' {
                    todo!()
                } // else if ch > ' ' and peek = '"'
                else {
                    return Ok(Symbols::BackSlash(pos_symb));
                }
            },
            '.' => {
                self.next_char();
                match self.get_char() {
                    '.' => { self.next_char(); return Ok(Symbols::UpTo(pos_symb)); },
                    '+' => { self.next_char(); return Ok(Symbols::DotPlus(pos_symb)); },
                    '*' => { self.next_char(); return Ok(Symbols::DotMul(pos_symb)); },
                    '/' => { self.next_char(); return Ok(Symbols::DotSlash(pos_symb)); },
                    '=' => { self.next_char(); return Ok(Symbols::DotEqual(pos_symb)); },
                    '#' => { self.next_char(); return Ok(Symbols::DotUnEqual(pos_symb)); },
                    '>' => {
                        self.next_char();
                        if self.get_char() == '=' {
                            self.next_char();
                            return Ok(Symbols::DotGreaterEqual(pos_symb));
                        }
                        return Ok(Symbols::DotGreater(pos_symb));
                    },
                    '<' => {
                        self.next_char();
                        if self.get_char() == '=' {
                            self.next_char();
                            return Ok(Symbols::DotLessEqual(pos_symb));
                        }
                        return Ok(Symbols::DotLess(pos_symb));
                    },
                    _ => { return Ok(Symbols::Period(pos_symb)); }
                }
            },
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                todo!()
            } ,
            '"' => {
                todo!()
            },
            '\'' => {
                todo!()
            },
            _ => {

                /* Handling identifier or reserved keyword */
				if self.get_char().is_alphabetic() {
					
                    let mut buffer = std::string::String::new();
                    
                    loop {
                        if self.get_char().is_alphanumeric() || self.get_char() == '_' {
                            buffer.push(self.get_char());
                            self.next_char();
                            continue;
                        }
                        break;
                    }

                    if self.strict {
                        let symb = self.is_capitalize_reserved_keyword(buffer.as_str(), pos_symb);

                        match symb {
                            Some(s) => { 
                                match s {
                                    Symbols::Code(_) => { /* Collect all until 'END' is found */
                                        let mut asm_text = std::string::String::new();

                                        loop {
                                            if self.get_char() == 'E' {
                                                self.next_char();
                                                if self.get_char() == 'N' {
                                                    self.next_char();
                                                    if self.get_char() == 'D' {
                                                        self.next_char();
                                                        self.position = self.position - 3;

                                                        return Ok(Symbols::CodeText(pos_symb, Box::new(asm_text)));
                                                    }
                                                }
                                            }
                                            if self.get_char() == '\0' { return Err((Box::new(String::from("Unterminated code block in source code. Missing 'END'")), self.position, self.lineno)); }
                                            asm_text.push(self.get_char());
                                            self.next_char();
                                        }
                                    }, 
                                    _ => ()
                                }
                                
                                return Ok(s); 
                            },
                            _ => {
                                return Ok(Symbols::Ident(pos_symb, Box::new(buffer)))
                            } 
                        }
                    }

                    let symb1 = self.is_reserved_keyword(buffer.as_str(), pos_symb);
                    match symb1 {
                        Some(s) => {
                            match s {
                                Symbols::Code(_) => { /* Collect all until 'END' or 'end' is found */
                                    let mut asm_text = std::string::String::new();

                                    loop {
                                        if self.get_char() == 'e' {
                                            self.next_char();
                                            if self.get_char() == 'n' {
                                                self.next_char();
                                                if self.get_char() == 'd' {
                                                    self.next_char();
                                                    self.position = self.position - 3;

                                                    return Ok(Symbols::CodeText(pos_symb, Box::new(asm_text)));
                                                }
                                            }
                                        }
                                        if self.get_char() == '\0' { return Err((Box::new(String::from("Unterminated code block in source code. Missing 'END'")), self.position, self.lineno)); }
                                        asm_text.push(self.get_char());
                                        self.next_char();
                                    }
                                },
                                _ => ()
                            }

                            return Ok(s);

                        },
                        _ => {
                           
                            let symb2 = self.is_capitalize_reserved_keyword(buffer.as_str(), pos_symb);

                            match symb2 {
                                Some(s) => { 
                                    match s {
                                        Symbols::Code(_) => { /* Collect all until 'END' or 'end' is found */
                                            let mut asm_text = std::string::String::new();

                                            loop {
                                                if self.get_char() == 'E' {
                                                    self.next_char();
                                                    if self.get_char() == 'N' {
                                                        self.next_char();
                                                        if self.get_char() == 'D' {
                                                            self.next_char();
                                                            self.position = self.position - 3;
    
                                                            return Ok(Symbols::CodeText(pos_symb, Box::new(asm_text)));
                                                        }
                                                    }
                                                }
                                                if self.get_char() == '\0' { return Err((Box::new(String::from("Unterminated code block in source code. Missing 'END'")), self.position, self.lineno)); }
                                                asm_text.push(self.get_char());
                                                self.next_char();
                                            }
                                        }, 
                                        _ => ()
                                    }
                                    
                                    return Ok(s); 
                                },
                                _ => {
                                    return Ok(Symbols::Ident(pos_symb, Box::new(buffer)))
                                } 
                            }
                        } 
                    }	
				}




                todo!();
            }
        }
    }

    fn get_location(&self) -> (usize, usize) {
        return (self.position.clone(), self.lineno.clone())
    }
}


#[cfg(test)]
mod tests {

    use crate::scanner::{Scanner, ScannerMethods, Symbols};

    #[test]
    fn operator_unequal() {
        let mut lexer = Scanner::new(" #", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::UnEqual(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }


    #[test]
    fn operator_equal() {
        let mut lexer = Scanner::new(" =", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Equal(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_greater() {
        let mut lexer = Scanner::new(" >", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Greater(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_greater_equal() {
        let mut lexer = Scanner::new(" >=", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::GreaterEqual(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_less() {
        let mut lexer = Scanner::new(" <", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Less(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_less_equal() {
        let mut lexer = Scanner::new(" <=", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::LessEqual(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_period() {
        let mut lexer = Scanner::new(" .", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Period(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_upto() {
        let mut lexer = Scanner::new(" ..", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::UpTo(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_dot_less_equal() {
        let mut lexer = Scanner::new(" .<=", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::DotLessEqual(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_dot_less() {
        let mut lexer = Scanner::new(" .<", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::DotLess(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_dot_greater_equal() {
        let mut lexer = Scanner::new(" .>=", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::DotGreaterEqual(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_dot_greater() {
        let mut lexer = Scanner::new(" .>", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::DotGreater(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_dot_equal() {
        let mut lexer = Scanner::new(" .=", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::DotEqual(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_dot_unequal() {
        let mut lexer = Scanner::new(" .#", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::DotUnEqual(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_dot_mul() {
        let mut lexer = Scanner::new(" .*", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::DotMul(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_dot_plus() {
        let mut lexer = Scanner::new(" .+", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::DotPlus(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_becomes() {
        let mut lexer = Scanner::new(" :=", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Becomes(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_colon() {
        let mut lexer = Scanner::new(" :", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Colon(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_semicolon() {
        let mut lexer = Scanner::new(" ;", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::SemiColon(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_power() {
        let mut lexer = Scanner::new(" **", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Power(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_mul() {
        let mut lexer = Scanner::new(" *", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Mul(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_plus() {
        let mut lexer = Scanner::new(" +", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Plus(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_plus_mul() {
        let mut lexer = Scanner::new(" +*", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::PlusMul(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_exclamatuinmark() {
        let mut lexer = Scanner::new(" !", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::ExclamationMark(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_exlamationmark_explarationmark() {
        let mut lexer = Scanner::new(" !!", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::ExclamationMarkExclamationMark(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_questionmark() {
        let mut lexer = Scanner::new(" ?", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::QuestionMark(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_questionmark_questionmark() {
        let mut lexer = Scanner::new(" ??", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::QuestionMarkQuestionMark(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_arrow() {
        let mut lexer = Scanner::new(" ^", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Arrow(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_bar() {
        let mut lexer = Scanner::new(" |", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Bar(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_not() {
        let mut lexer = Scanner::new(" ~", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Not(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_minus() {
        let mut lexer = Scanner::new(" -", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Minus(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_backquote() {
        let mut lexer = Scanner::new(" `", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Transpose(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_and() {
        let mut lexer = Scanner::new(" &", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::And(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_less_less() {
        let mut lexer = Scanner::new(" <<", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::LessLess(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_less_less_q() {
        let mut lexer = Scanner::new(" <<?", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::LessLessQ(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_greater_greater() {
        let mut lexer = Scanner::new(" >>", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::GreaterGreater(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_greater_greater_q() {
        let mut lexer = Scanner::new(" >>?", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::GreaterGreaterQ(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_left_paren() {
        let mut lexer = Scanner::new(" (", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::LeftParen(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_right_paren() {
        let mut lexer = Scanner::new(" )", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::RightParen(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_left_bracket() {
        let mut lexer = Scanner::new(" [", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::LeftBracket(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_right_bracket() {
        let mut lexer = Scanner::new(" ]", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::RightBracket(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_left_curly() {
        let mut lexer = Scanner::new(" {", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::LeftCurly(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn operator_right_curly() {
        let mut lexer = Scanner::new(" }", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::RightCurly(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }


    

    

    








    #[test]
    fn strict_keyword_await() {
        let mut lexer = Scanner::new(" AWAIT", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Await(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_cell() {
        let mut lexer = Scanner::new(" CELL", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Cell(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_cellnet() {
        let mut lexer = Scanner::new(" CELLNET", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::CellNet(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_begin() {
        let mut lexer = Scanner::new(" BEGIN", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Begin(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_by() {
        let mut lexer = Scanner::new(" BY", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::By(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_const() {
        let mut lexer = Scanner::new(" CONST", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Const(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_case() {
        let mut lexer = Scanner::new(" CASE", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Case(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_code() {
        let mut lexer = Scanner::new(" CODE END", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::CodeText(p, _) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_definition() {
        let mut lexer = Scanner::new(" DEFINITION", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Definition(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_do() {
        let mut lexer = Scanner::new(" DO", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Do(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_div() {
        let mut lexer = Scanner::new(" DIV", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Div(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_end() {
        let mut lexer = Scanner::new(" END", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::End(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_enum() {
        let mut lexer = Scanner::new(" ENUM", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Enum(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_else() {
        let mut lexer = Scanner::new(" ELSE", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Else(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_elsif() {
        let mut lexer = Scanner::new(" ELSIF", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Elsif(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_exit() {
        let mut lexer = Scanner::new(" EXIT", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Exit(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_extern() {
        let mut lexer = Scanner::new(" EXTERN", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Extern(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_false() {
        let mut lexer = Scanner::new(" FALSE", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::False(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_for() {
        let mut lexer = Scanner::new(" FOR", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::For(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_finally() {
        let mut lexer = Scanner::new(" FINALLY", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Finally(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_if() {
        let mut lexer = Scanner::new(" IF", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::If(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_imag() {
        let mut lexer = Scanner::new(" IMAG", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Imag(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_in() {
        let mut lexer = Scanner::new(" IN", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::In(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_is() {
        let mut lexer = Scanner::new(" IS", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Is(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_import() {
        let mut lexer = Scanner::new(" IMPORT", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Import(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_loop() {
        let mut lexer = Scanner::new(" LOOP", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Loop(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_module() {
        let mut lexer = Scanner::new(" MODULE", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Module(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_mod() {
        let mut lexer = Scanner::new(" MOD", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Mod(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_nil() {
        let mut lexer = Scanner::new(" NIL", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Nil(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_of() {
        let mut lexer = Scanner::new(" OF", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Of(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_or() {
        let mut lexer = Scanner::new(" OR", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Or(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_put() {
        let mut lexer = Scanner::new(" OUT", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Out(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_operator() {
        let mut lexer = Scanner::new(" OPERATOR", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Operator(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_procedure() {
        let mut lexer = Scanner::new(" PROCEDURE", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Procedure(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_port() {
        let mut lexer = Scanner::new(" PORT", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Port(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_repeat() {
        let mut lexer = Scanner::new(" REPEAT", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Repeat(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_return() {
        let mut lexer = Scanner::new(" RETURN", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Return(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_self() {
        let mut lexer = Scanner::new(" SELF", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::_Self(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_result() {
        let mut lexer = Scanner::new(" RESULT", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Result(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_then() {
        let mut lexer = Scanner::new(" THEN", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Then(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_true() {
        let mut lexer = Scanner::new(" TRUE", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::True(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_to() {
        let mut lexer = Scanner::new(" TO", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::To(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_type() {
        let mut lexer = Scanner::new(" TYPE", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Type(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_until() {
        let mut lexer = Scanner::new(" UNTIL", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Until(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_var() {
        let mut lexer = Scanner::new(" VAR", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Var(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_while() {
        let mut lexer = Scanner::new(" WHILE", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::While(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_with() {
        let mut lexer = Scanner::new(" WITH", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::With(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_array() {
        let mut lexer = Scanner::new(" ARRAY", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Array(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_object() {
        let mut lexer = Scanner::new(" OBJECT", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Object(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_pointer() {
        let mut lexer = Scanner::new(" POINTER", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Pointer(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_record() {
        let mut lexer = Scanner::new(" RECORD", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Record(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_address() {
        let mut lexer = Scanner::new(" ADDRESS", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Address(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_size() {
        let mut lexer = Scanner::new(" SIZE", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Size(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_keyword_alias() {
        let mut lexer = Scanner::new(" ALIAS", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Alias(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_await() {
        let mut lexer = Scanner::new(" await", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Await(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_cell() {
        let mut lexer = Scanner::new(" cell", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Cell(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_cellnet() {
        let mut lexer = Scanner::new(" cellnet", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::CellNet(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_begin() {
        let mut lexer = Scanner::new(" begin", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Begin(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_by() {
        let mut lexer = Scanner::new(" by", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::By(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_const() {
        let mut lexer = Scanner::new(" const", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Const(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_case() {
        let mut lexer = Scanner::new(" case", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Case(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_code() {
        let mut lexer = Scanner::new(" code end", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::CodeText(p, _) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_definition() {
        let mut lexer = Scanner::new(" definition", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Definition(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_do() {
        let mut lexer = Scanner::new(" do", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Do(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_div() {
        let mut lexer = Scanner::new(" div", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Div(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_end() {
        let mut lexer = Scanner::new(" end", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::End(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_enum() {
        let mut lexer = Scanner::new(" enum", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Enum(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_else() {
        let mut lexer = Scanner::new(" else", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Else(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_elsif() {
        let mut lexer = Scanner::new(" elsif", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Elsif(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_exit() {
        let mut lexer = Scanner::new(" exit", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Exit(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_extern() {
        let mut lexer = Scanner::new(" extern", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Extern(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_false() {
        let mut lexer = Scanner::new(" false", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::False(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_for() {
        let mut lexer = Scanner::new(" for", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::For(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_finally() {
        let mut lexer = Scanner::new(" finally", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Finally(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_if() {
        let mut lexer = Scanner::new(" if", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::If(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_imag() {
        let mut lexer = Scanner::new(" imag", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Imag(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_in() {
        let mut lexer = Scanner::new(" in", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::In(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_is() {
        let mut lexer = Scanner::new(" is", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Is(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_import() {
        let mut lexer = Scanner::new(" import", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Import(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_loop() {
        let mut lexer = Scanner::new(" loop", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Loop(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_module() {
        let mut lexer = Scanner::new(" module", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Module(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_mod() {
        let mut lexer = Scanner::new(" mod", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Mod(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_nil() {
        let mut lexer = Scanner::new(" nil", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Nil(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_of() {
        let mut lexer = Scanner::new(" of", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Of(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_or() {
        let mut lexer = Scanner::new(" or", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Or(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_put() {
        let mut lexer = Scanner::new(" out", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Out(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_operator() {
        let mut lexer = Scanner::new(" operator", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Operator(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_procedure() {
        let mut lexer = Scanner::new(" procedure", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Procedure(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_port() {
        let mut lexer = Scanner::new(" port", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Port(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_repeat() {
        let mut lexer = Scanner::new(" repeat", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Repeat(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_return() {
        let mut lexer = Scanner::new(" return", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Return(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_self() {
        let mut lexer = Scanner::new(" self", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::_Self(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_result() {
        let mut lexer = Scanner::new(" result", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Result(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_then() {
        let mut lexer = Scanner::new(" then", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Then(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_true() {
        let mut lexer = Scanner::new(" true", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::True(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_to() {
        let mut lexer = Scanner::new(" to", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::To(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_type() {
        let mut lexer = Scanner::new(" type", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Type(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_until() {
        let mut lexer = Scanner::new(" until", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Until(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_var() {
        let mut lexer = Scanner::new(" var", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Var(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_while() {
        let mut lexer = Scanner::new(" while", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::While(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_with() {
        let mut lexer = Scanner::new(" with", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::With(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_array() {
        let mut lexer = Scanner::new(" array", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Array(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_object() {
        let mut lexer = Scanner::new(" object", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Object(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_pointer() {
        let mut lexer = Scanner::new(" pointer", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Pointer(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_record() {
        let mut lexer = Scanner::new(" record", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Record(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_address() {
        let mut lexer = Scanner::new(" address", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Address(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_size() {
        let mut lexer = Scanner::new(" size", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Size(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_alias() {
        let mut lexer = Scanner::new(" alias", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Alias(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn keyword_await_more() {
        let mut lexer = Scanner::new(" AWAIT", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Await(p) => {
                        assert_eq!(1, p);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn literal_ident() {
        let mut lexer = Scanner::new(" Test11_1", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::Ident(p, s) => {
                        assert_eq!(1, p);
                        assert_eq!(Box::new(String::from("Test11_1")), s);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn end_of_file() {
        let mut lexer = Scanner::new("", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::EndOfFile => {
                        assert!(true);    
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn comment_only() {
        let mut lexer = Scanner::new("(* This is a (* test *) in Active Oberon with two levels *)", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::EndOfFile => {
                        assert!(true);    
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn strict_code_block() {
        let mut lexer = Scanner::new("CODE asembler code is here! END", true);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::CodeText(_, _) => {
                        assert!(true);    
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }

        let symb2 = lexer.get_next_symbol();

        match symb2 {
            Ok(x) => {
                match x {
                    Symbols::End(_) => {
                        assert!(true);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }

    #[test]
    fn code_block() {
        let mut lexer = Scanner::new("code asembler code is here! end", false);
        let symb = lexer.get_next_symbol();

        match symb {
            Ok(x) => {
                match x {
                    Symbols::CodeText(_, _) => {
                        assert!(true);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }

        let symb2 = lexer.get_next_symbol();

        match symb2 {
            Ok(x) => {
                match x {
                    Symbols::End(_) => {
                        assert!(true);
                    },
                    _ => { assert!(false); }
                }
            },
            _ => { assert!(false); }
        }
    }
}