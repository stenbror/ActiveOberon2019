
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
enum Symbols {

    /* Reserved keywords */
    Await(usize),
    Begin(usize),
    By(usize),
    Const(usize),
    Case(usize),
    Cell(usize),
    CellNet(usize),
    Code(usize),
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
    NotEqual(usize), // #
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
    GreaterGreater(usize), // >>
    GreatterGreaterQ(usize), // >>?

    /* Literals */
    Ident(usize, Box<str>),
    Number(usize),
    String(usize, Box<str>),
    Char(usize),

    /* System */
    EndOfFile
}

pub trait ScannerMethods {
    fn new(text: &'static str) -> Self;
    fn get_char(&mut self) -> char;
    fn next_char(&mut self) -> ();
    fn get_next_symbol(&mut self) -> Result<Symbols, (Box<std::string::String>, usize, usize)>;
}



pub struct Scanner {
    source: Vec<char>,
    position: usize,
    lineno: usize,
}

impl ScannerMethods for Scanner {


    fn new(text: &'static str) -> Scanner {
        Scanner {
        source: text.chars().collect(),
        position: 0,
        lineno: 1
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

    fn get_next_symbol(&mut self) -> Result<Symbols, (Box<std::string::String>, usize, usize)> {

        todo!();
    }
}
