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
    Leq,
    Geq,
    Lt,
    Gt,
    Eq,
    Comma,
    If,
    Then,
    Else,
    For,
    Iden(String),
    Number(f64),
    Eof
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
       match self {
           Token::Def => write!(f, "{}", "def"),
           Token::Extern => write!(f, "{}", "extern"),
           Token::LParen => write!(f, "{}", "'('"),
           Token::RParen => write!(f, "{}", "')'"),
           Token::Plus => write!(f, "{}", "'+'"),
           Token::Minus => write!(f, "{}", "'-'"),
           Token::Times => write!(f, "{}", "'*'"),
           Token::Slash => write!(f, "{}", "'/'"),
           Token::Leq => write!(f, "{}", "'<='"),
           Token::Geq => write!(f, "{}", "'>='"),
           Token::Lt => write!(f, "{}", "'<'"),
           Token::Gt => write!(f, "{}", "'>'"),
           Token::Eq => write!(f, "{}", "'='"),
           Token::Comma => write!(f, "{}", "','"),
           Token::If => write!(f, "{}", "'if"),
           Token::Then => write!(f, "{}", "then"),
           Token::Else => write!(f, "{}", "else"),
           Token::For => write!(f, "{}", "for"),
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

    fn read_iden(&mut self) -> Token {
        let mut iden_str = String::new();
        loop {
            let ch = self.peek_char();
            if ch == '\0' || (!ch.is_alphanumeric() && ch != '_') {
                break
            }
            iden_str.push(ch);
            self.consume_char();
        }
        match iden_str.as_str() {
            "def" => Token::Def,
            "extern" => Token::Extern,
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "for" => Token::For,
            _ => Token::Iden(iden_str)
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
        let num: f64 = self.strbuf.parse().unwrap();
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
        match self.read_char() {
            '(' => Token::LParen,
            ')' => Token::RParen,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Times,
            '/' => Token::Slash,
            '<' => {
                let ch = self.peek_char();
                if ch == '\0' {
                    return Token::Eof
                }
                if ch == '=' {
                    self.consume_char();
                    Token::Leq
                } else {
                    Token::Lt
                }
            },
            '>' => {
                let ch = self.peek_char();
                if ch == '\0' {
                    return Token::Eof
                }
                if ch == '=' {
                    self.consume_char();
                    Token::Geq
                } else {
                    Token::Gt
                }
            },
            '=' => Token::Eq,
            ',' => Token::Comma,
            '\0' => Token::Eof,
            ch => panic!("Invalid or unknown character: '{}'", ch),
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
            self.read_iden()
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

            # Compute the x'th fibonacci number.
            def fib(x)
              if 3 <= x then
                1
              else
                fib(x-1)+fib(x-2)

            # This expression will compute the 40th number.
            fib(40)
        ";
        let reader = BufReader::new(Cursor::new(input));
        let tokens = Lexer::new(reader).read_tokens();
        println!("{:?}", tokens);

        let expected_tokens = [
            Extern, Iden("atan".to_string()), LParen, RParen,
            Def, Iden("fib".to_string()), LParen, Iden("x".to_string()), RParen, If,
            Number(3.0), Leq, Iden("x".to_string()), Then, Number(1.0), Else,
            Iden("fib".to_string()), LParen, Iden("x".to_string()), Minus, Number(1.0), RParen, Plus, Iden("fib".to_string()),
            LParen, Iden("x".to_string()), Minus, Number(2.0), RParen, Iden("fib".to_string()), LParen, Number(40.0), RParen
        ];
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn test_single_line() {
        let input = r"
            def foo(a b) a*a + 2*a*b + b*b
        ";

        let reader = BufReader::new(Cursor::new(input));
        let tokens = Lexer::new(reader).read_tokens();
        println!("{:?}", tokens);
    }
}