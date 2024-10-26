use std::io::{BufRead, BufReader, Read, Stdin};

#[derive(Debug, PartialEq)]
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
    Gt,
    Lt,
    Eq,
    Iden(String),
    Number(f64)
}

struct Lexer<R: BufRead> {
    reader: BufReader<R>,
    line: u32,
    col: u32,
    last_char: char
}

impl<R: BufRead> Lexer<R> {
    pub fn new(reader: BufReader<R>) -> Lexer<R> {
        Lexer { reader, last_char: ' ', line: 0, col: 0 }
    }

    pub fn get_char(&mut self) -> Option<char> {
        let mut buffer = [0; 1];
        match self.reader.read(&mut buffer) {
            Ok(count) => {
                if count > 0 {
                    let c = buffer[0] as char;
                    if c == '\n' {
                        self.line += 1
                    } else {
                        self.col += 1
                    }
                    Some(c)
                } else {
                    None
                }
            },
            Err(err) => panic!("Oops! Failed to read a token with error: {}", err)
        }
    }

    pub fn get_token(&mut self) -> Option<Token> {
        while self.last_char.is_whitespace() { // skip all whitespace until, last_char will be the a non-whitespace at the end of the loop
            self.last_char = match self.get_char() {
                None => return None,
                Some(ch) => ch
            };
        }

        if self.last_char.is_alphabetic() {
            let mut iden_str = String::new();
            while self.last_char.is_alphanumeric() { // read until we get a char that cant be in an iden
                iden_str.push(self.last_char);
                self.last_char = match self.get_char() {
                    None => break, // stop reading the iden if we reach eof
                    Some(ch) => ch
                }
            }
            let token = match iden_str.as_str() {
                "def" => Token::Def,
                "extern" => Token::Extern,
                _ => Token::Iden(iden_str)
            };
            Some(token)
        } else if self.last_char.is_digit(10) || self.last_char == '.' {
            let mut num_str = String::new();
            loop {
                num_str.push(self.last_char);
                self.last_char = match self.get_char() {
                    None => break, // stop reading the num if we reach eof
                    Some(ch) => ch
                };
                if !self.last_char.is_digit(10) && self.last_char != '.' {
                    break;
                }
            }
            let num: f64 = num_str.parse().unwrap();
            Some(Token::Number(num))
        } else if self.last_char == '#' { // a comment, so skip until end of line
            loop {
                self.last_char = match self.get_char() {
                    None => return None, // eof in comment means there cannot be anymore tokens
                    Some(ch) => ch
                };
                if self.last_char == '\n' || self.last_char == '\r' {
                    break;
                }
            }
            // ok, we skipped the comment now recursively ask for another token
            self.get_token()
        } else {
            // otherwise, handle any misc characters
            let curr_char = self.last_char;
            let token = match curr_char {
                '(' => Token::LParen,
                ')' => Token::RParen,
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Times,
                '/' => Token::Slash,
                '<' => Token::Lt,
                '>' => Token::Gt,
                '=' => Token::Eq,
                // Token::Geq,
                // Token::Leq
                _ => panic!("Unknown token has been reached {}", self.last_char)
            };
            self.last_char = match self.get_char() {
                None => return None, // eof in comment means there cannot be anymore tokens
                Some(ch) => ch // consume the token since we just used it
            };
            Some(token)
        }
    }

    pub fn get_tokens(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        loop {
            match self.get_token() {
                None => return tokens,
                Some(token) => tokens.push(token)
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
        let tokens = Lexer::new(reader).get_tokens();
        println!("{:?}", tokens);

        let expected_tokens = [Def, Iden(String::from("fib")), LParen, Iden(String::from("x")), RParen, Iden(String::from("if")),
            Iden(String::from("x")), Lt, Number(3.0), Iden(String::from("then")), Number(1.0), Iden(String::from("else")), Iden(String::from("fib")), LParen,
            Iden(String::from("x")), Minus, Number(1.0), RParen, Plus, Iden(String::from("fib")), LParen, Iden(String::from("x")), Minus,
            Number(2.0), RParen, Iden(String::from("fib")), LParen, Number(40.0), RParen];
        assert_eq!(tokens, expected_tokens);
    }
}