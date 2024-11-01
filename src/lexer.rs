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
           Token::Iden(_) => write!(f, "{}", "<iden>"),
           Token::Number(num) => write!(f, "{}", num),
           Token::Eof => write!(f, "{}", "<eof>")
       }
    }
}

pub struct Lexer<R: BufRead> {
    reader: BufReader<R>,
    last_char: char,
    strbuf: String, // reusable buffer for temporary strings
}

impl<R: BufRead> Lexer<R> {
    pub fn new(reader: BufReader<R>) -> Lexer<R> {
        Lexer {
            reader,
            last_char: ' ',
            strbuf: String::new()
        }
    }

    pub fn read_char(&mut self) -> Option<char> {
        let mut buffer = [0; 1];
        match self.reader.read(&mut buffer) {
            Ok(count) => {
                if count > 0 {
                    Some(buffer[0] as char)
                } else {
                    None
                }
            },
            Err(err) => panic!("Oops! Failed to read a token with error: {}", err)
        }
    }

    pub fn read_iden(&mut self) -> Token {
        let mut iden_str = String::new();
        while self.last_char.is_alphanumeric() { // read until we get a char that cant be in an iden
            iden_str.push(self.last_char);
            self.last_char = match self.read_char() {
                None => break, // stop reading the iden if we reach eof
                Some(ch) => ch
            }
        }
        match iden_str.as_str() {
            "def" => Token::Def,
            "extern" => Token::Extern,
            _ => Token::Iden(iden_str)
        }
    }

    pub fn read_number(&mut self) -> Token {
        self.strbuf.clear();
        loop {
            self.strbuf.push(self.last_char);
            self.last_char = match self.read_char() {
                None => break, // stop reading the num if we reach eof
                Some(ch) => ch
            };
            if !self.last_char.is_digit(10) && self.last_char != '.' {
                break;
            }
        }
        let num: f64 = self.strbuf.parse().unwrap();
        Token::Number(num)
    }

    pub fn skip_comment(&mut self) -> Token {
        loop {
            self.last_char = match self.read_char() {
                None => return Token::Eof, // eof in comment means there cannot be anymore tokens
                Some(ch) => ch
            };
            if self.last_char == '\n' || self.last_char == '\r' {
                break;
            }
        }
        // ok, we skipped the comment now recursively ask for another token
        self.read_token()
    }

    pub fn read_misc_token(&self) -> Token {
        let curr_char = self.last_char;
        match curr_char {
            '(' => Token::LParen,
            ')' => Token::RParen,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Times,
            '/' => Token::Slash,
            '<' => Token::Lt,
            '>' => Token::Gt,
            '=' => Token::Eq,
            ',' => Token::Comma,
            // Token::Geq,
            // Token::Leq,
            _ => panic!("Unknown token has been reached {}", self.last_char),
        }
    }

    pub fn read_token(&mut self) -> Token {
        while self.last_char.is_whitespace() { // skip all whitespace until, last_char will be the a non-whitespace at the end of the loop
            self.last_char = match self.read_char() {
                None => return Token::Eof,
                Some(ch) => ch
            };
        }

        if self.last_char.is_alphabetic() {
            self.read_iden()
        } else if self.last_char.is_digit(10) || self.last_char == '.' {
            self.read_number()
        } else if self.last_char == '#' {
            // a comment, so skip until end of line
            self.skip_comment()
        } else {
            // otherwise, handle any misc characters
            let token = self.read_misc_token();
            self.last_char = match self.read_char() {
                None => return Token::Eof, // eof in comment means there cannot be anymore tokens
                Some(ch) => ch, // consume the token since we just used it
            };
            token
        }
    }

    pub fn read_tokens(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        loop {
            match self.read_token() {
                Token::Eof => return tokens,
                token => tokens.push(token),
            }
        }
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
              if x < 3 then
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
            Def, Iden("fib".to_string()), LParen, Iden("x".to_string()), RParen, Iden("if".to_string()),
            Iden("x".to_string()), Lt, Number(3.0), Iden("then".to_string()), Number(1.0), Iden("else".to_string()),
            Iden("fib".to_string()), LParen, Iden("x".to_string()), Minus, Number(1.0), RParen, Plus, Iden("fib".to_string()),
            LParen, Iden("x".to_string()), Minus, Number(2.0), RParen, Iden("fib".to_string()), LParen, Number(40.0), RParen
        ];
        assert_eq!(tokens, expected_tokens);
    }
}