use std::fmt::{Debug, Formatter};
use std::io::{BufRead, BufReader, Read};

#[derive(PartialEq)]
pub enum Token {
    Def,
    Extern,
    LParen,
    RParen,
    Plus,
    Minus,
    Times,
    Slash,
    DoubleTimes,
    And,
    Or,
    Xor,
    Leq,
    Geq,
    Lt,
    Gt,
    Eq,
    Comma,
    Semicolon,
    Assign,
    AssignPlus,
    AssignMinus,
    AssignTimes,
    AssignSlash,
    If,
    Then,
    Else,
    For,
    While,
    Var,
    In,
    To,
    End,
    Do,
    Iden(String),
    Number(f64),
    Eof
}

impl Token {
    pub fn is_terminator(&self) -> bool {
        match self {
            | Token::Def
            | Token::Extern
            | Token::Comma
            | Token::Then
            | Token::Else
            | Token::For
            | Token::In
            | Token::To
            | Token::End
            | Token::Do
            | Token::Eof => true,
            _ => false
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
       match self {
           Token::Def => write!(f, "{}", "`def`"),
           Token::Extern => write!(f, "{}", "`extern`"),
           Token::To => write!(f, "{}", "`to`"),
           Token::In => write!(f, "{}", "`in`"),
           Token::If => write!(f, "{}", "`if`"),
           Token::Then => write!(f, "{}", "`then`"),
           Token::Else => write!(f, "{}", "`else`"),
           Token::For => write!(f, "{}", "`for`"),
           Token::While => write!(f, "{}", "`while`"),
           Token::Var => write!(f, "{}", "`var`"),
           Token::End => write!(f, "{}", "`end`"),
           Token::Do => write!(f, "{}", "`do`"),
           Token::LParen => write!(f, "{}", "'('"),
           Token::RParen => write!(f, "{}", "')'"),
           Token::Plus => write!(f, "{}", "'+'"),
           Token::Minus => write!(f, "{}", "'-'"),
           Token::Times => write!(f, "{}", "'*'"),
           Token::Slash => write!(f, "{}", "'/'"),
           Token::DoubleTimes => write!(f, "{}", "'**'"),
           Token::And => write!(f, "{}", "`and`"),
           Token::Or => write!(f, "{}", "`or`"),
           Token::Xor => write!(f, "{}", "`xor`"),
           Token::Leq => write!(f, "{}", "'<='"),
           Token::Geq => write!(f, "{}", "'>='"),
           Token::Lt => write!(f, "{}", "'<'"),
           Token::Gt => write!(f, "{}", "'>'"),
           Token::Eq => write!(f, "{}", "'=='"),
           Token::Assign => write!(f, "{}", "'='"),
           Token::AssignPlus => write!(f, "{}", "'+='"),
           Token::AssignMinus => write!(f, "{}", "'-='"),
           Token::AssignTimes => write!(f, "{}", "'*='"),
           Token::AssignSlash => write!(f, "{}", "'/='"),
           Token::Comma => write!(f, "{}", "','"),
           Token::Semicolon => write!(f, "{}", "';'"),
           Token::Iden(_) => write!(f, "{}", "<iden>"),
           Token::Number(num) => write!(f, "{}", num),
           Token::Eof => write!(f, "{}", "<eof>")
       }
    }
}

pub struct Lexer<R: BufRead> {
    reader: BufReader<R>,
    strbuf: String, // reusable buffer for temporary strings
}

impl<R: BufRead> Lexer<R> {
    pub fn new(reader: BufReader<R>) -> Lexer<R> {
        Lexer {
            reader,
            strbuf: String::new()
        }
    }

    fn read_char(&mut self) -> char {
        let mut buf = [0; 1];
        let count = self.reader.read(&mut buf).unwrap();
        if count > 0 {
            buf[0] as char
        } else {
            '\0'
        }
    }

    fn peek_char(&mut self) -> char {
        let buf = self.reader.fill_buf().unwrap();
        if !buf.is_empty() {
            buf[0] as char
        } else {
            '\0'
        }
    }

    fn consume_char(&mut self) {
        self.reader.consume(1)
    }

    fn read_text(&mut self) -> Token {
        self.strbuf.clear();
        loop {
            let ch = self.peek_char();
            if ch == '\0' || (!ch.is_alphanumeric() && ch != '_') {
                break
            }
            self.strbuf.push(ch);
            self.consume_char();
        }
        match self.strbuf.as_str() {
            "def" => Token::Def,
            "extern" => Token::Extern,
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "for" => Token::For,
            "to" => Token::To,
            "in" => Token::In,
            "while" => Token::While,
            "var" => Token::Var,
            "end" => Token::End,
            "do" => Token::Do,
            "and" => Token::And,
            "or" => Token::Or,
            "xor" => Token::Xor,
            _ => Token::Iden(self.strbuf.clone())
        }
    }

    fn read_number(&mut self) -> Token {
        self.strbuf.clear();
        loop {
            let ch = self.peek_char();
            if ch == '\0' || (!ch.is_digit(10) && ch != '.') {
                break
            }
            self.strbuf.push(ch);
            self.consume_char();
        }
        let num: f64 = self.strbuf.parse().expect(&format!("Fatal: error parsing invalid number {}", self.strbuf));
        Token::Number(num)
    }

    fn skip_comment(&mut self) -> Token {
        loop {
            let ch = self.read_char();
            if ch == '\0' {
                return Token::Eof
            }
            if ch == '\n' || ch == '\r' {
                break;
            }
        }
        // ok, we skipped the comment now recursively ask for another token
        self.read_token()
    }

    fn read_misc_token(&mut self) -> Token {
        // try to handle single length characters first
        match self.read_char() {
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '\0' => Token::Eof,
            // otherwise, consider if the character is multi-length
            ch => {
                self.strbuf.clear();
                self.strbuf.push(ch);
                loop {
                    let ch = self.peek_char();
                    if ch == '\0' || ch.is_alphanumeric() || ch.is_whitespace() {
                        break
                    }
                    self.strbuf.push(ch);
                    self.consume_char();
                }
                match self.strbuf.as_str() {
                    "+" => Token::Plus,
                    "-" => Token::Minus,
                    "*" => Token::Times,
                    "/" => Token::Slash,
                    "**" => Token::DoubleTimes,
                    "<=" => Token::Leq,
                    "<" => Token::Lt,
                    ">=" => Token::Geq,
                    ">" => Token::Gt,
                    "==" => Token::Eq,
                    "=" => Token::Assign,
                    "+=" => Token::AssignPlus,
                    "-=" => Token::AssignMinus,
                    "*=" => Token::AssignTimes,
                    "/=" => Token::AssignTimes,
                    // the character isn't even multi-length - it is invalid
                    other => panic!("Invalid or unknown character: '{}'", other),
                }
            }
        }

    }

    pub fn read_token(&mut self) -> Token {
        loop {
            let ch = self.peek_char();
            if ch == '\0' {
                return Token::Eof
            } else if !ch.is_whitespace() {
                break
            }
            self.consume_char();
        }

        let ch = self.peek_char();
        if ch.is_alphabetic() {
            self.read_text()
        } else if ch.is_digit(10) || ch == '.' {
            self.read_number()
        } else if ch == '#' {
            // a comment, so skip until end of line
            self.skip_comment()
        } else {
            // otherwise, handle any misc characters
            self.read_misc_token()
        }
    }

    pub fn read_tokens(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        for _ in 0..100000 {
            match self.read_token() {
                Token::Eof => return tokens,
                token => {
                    // println!("{:?}", token);
                    tokens.push(token)
                },
            }
        }
        panic!("Fatal: Exceeded maximum token limit. Does the lexer have an infinite loop?")
    }
}

mod test {
    use std::io::{BufReader, Cursor};
    use crate::lexer::Lexer;
    use crate::lexer::Token::*;

    #[test]
    fn test_basic() {
        let input = r"
            extern atan()
            extern printf(str)

            # Compute the x'th fibonacci number.
            def fib(x) do
              if 3 <= x then
                1
              else
                fib(x-1)+fib(x-2)
            end

            def printLoop(x) do
              for i in 0 to 10 do
                printf(str)
              end
            end

            # This expression will compute the 40th number.
            fib(40)
        ";
        let reader = BufReader::new(Cursor::new(input));
        let tokens = Lexer::new(reader).read_tokens();
        println!("{:?}", tokens);

        let expected_tokens = [
            Extern, Iden("atan".to_string()), LParen, RParen,

            Extern, Iden("printf".to_string()), LParen, Iden("str".to_string()), RParen,

            Def, Iden("fib".to_string()), LParen, Iden("x".to_string()), RParen, Do, If,
            Number(3.0), Leq, Iden("x".to_string()), Then, Number(1.0), Else,
            Iden("fib".to_string()), LParen, Iden("x".to_string()), Minus, Number(1.0), RParen, Plus, Iden("fib".to_string()),
            LParen, Iden("x".to_string()), Minus, Number(2.0), RParen, End,

            Def, Iden("printLoop".to_string()), LParen, Iden("x".to_string()), RParen, Do,
            For, Iden("i".to_string()), In, Number(0.0), To, Number(10.0), Do,
            Iden("printf".to_string()), LParen, Iden("str".to_string()), RParen, End, End,

            Iden("fib".to_string()), LParen, Number(40.0), RParen
        ];
        assert_eq!(tokens, expected_tokens);
    }
}