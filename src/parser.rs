use std::io::BufRead;
use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Multiply,
    Divide,
    Subtract,
    Lt,
    Gt,
    Eq
}

#[derive(Debug, PartialEq)]
pub enum ExprAst {
    Number(f64),
    Variable(String),
    Binary {
        op: Operator,
        lhs: Box<ExprAst>,
        rhs: Box<ExprAst>,
    },
    Call {
        callee: String,
        args: Vec<Box<ExprAst>>,
    },
    Err(String)
}

#[derive(Debug, PartialEq)]
pub struct PrototypeAst {
    name: String,
    args: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionAst {
    prototype: PrototypeAst,
    body: ExprAst,
}

#[derive(Debug, PartialEq)]
pub enum DeclAst {
    Extern(PrototypeAst),
    Function(FunctionAst),
    Err(String)
}

pub struct Parser<R: BufRead> {
    lexer: Lexer<R>,
    last_token: Option<Token>,
}

impl<R: BufRead> Parser<R> {
    pub fn new(lexer: Lexer<R>) -> Parser<R> {
        Parser{ lexer, last_token: None }
    }

    pub fn peek_token(&mut self) -> Option<&Token> {
        match self.last_token {
            Some(ref last_token) => Some(last_token),
            None => match self.lexer.read_token() {
                None => None,
                Some(tok) => {
                    self.last_token = Some(tok);
                    self.last_token.as_ref()
                }
            },
        }
    }

    pub fn read_token(&mut self) -> Option<Token> {
        match self.last_token {
            None => self.lexer.read_token(),
            Some(_) => self.last_token.take(),
        }
    }

    pub fn parse_iden_expr(&mut self, iden: String) -> ExprAst {
        let opt_tok = self.peek_token();
        match opt_tok {
            Some(Token::LParen) => {
                self.read_token(); // consume the lparen
                let mut args = vec![];
                loop {
                    let arg_expr = self.parse_expr();
                    args.push(Box::new(arg_expr));

                    let opt_tok = self.read_token();
                    match opt_tok {
                        Some(Token::Comma) => (), // consume the comma and keep parsing args
                        Some(Token::RParen) => break, // stop parsing args
                        Some(token) => panic!("Expected an rparen token to be at the end of a call expression got {:?}", token),
                        None => panic!("Reached the end of the token stream while parsing an identifier"),
                    }
                }
                ExprAst::Call{ callee: iden, args, }
            },
            _ => ExprAst::Variable(iden),
        }
    }

    pub fn parse_binop_rhs() {

    }

    pub fn parse_expr(&mut self) -> ExprAst {
        let opt_tok = self.read_token();
        match opt_tok {
            Some(Token::Number(num)) => ExprAst::Number(num),
            Some(Token::Iden(iden)) => self.parse_iden_expr(iden),
            Some(token) => panic!("Expected an iden, numeric, or lparen token while parsing the start of an expression got {:?}", token),
            None => panic!("Reached the end of the token stream while parsing an expression"),
        }
    }

    pub fn parse(&mut self) -> Vec<DeclAst> {
        let mut nodes = vec![];

        while let Some(token) = self.read_token() {
            let node = match token {
                Token::Extern => DeclAst::Extern(self.parse_prototype()),
                Token::Def => self.parse_function(),
                _ => panic!("Expected a toplevel node declaration to be either an extern or a def"),
            };
            nodes.push(node);
        }

        nodes
    }

    pub fn parse_extern(&mut self) -> DeclAst {
        DeclAst::Extern(self.parse_prototype())
    }

    pub fn parse_prototype(&mut self) -> PrototypeAst {
        let opt_tok = self.read_token();
        let name = match opt_tok {
            Some(Token::Iden(iden)) => iden,
            Some(token) => panic!("Expected an iden token while parsing prototype, got {:?}", token),
            None => panic!("Reached the end of the token stream while parsing a prototype"),
        };
        let opt_tok = self.read_token();
        match opt_tok {
            Some(Token::LParen) => (), // discard
            Some(token) => panic!("Expected an lparen token while parsing prototype got {:?}", token),
            None => panic!("Reached the end of the token stream while parsing a prototype"),
        };
        let mut args = vec![];
        loop {
            let opt_tok = self.read_token();
            let arg = match opt_tok {
                Some(Token::Iden(iden)) => iden,
                Some(Token::RParen) => break,
                Some(token) => panic!("Expected an iden token while parsing prototype got {:?}", token),
                None => panic!("Reached the end of the token stream while parsing a prototype argument list"),
            };
            args.push(arg);
        }
        PrototypeAst { name, args }
    }

    pub fn parse_function(&mut self) -> DeclAst {
        let prototype = self.parse_prototype();
        let body = self.parse_expr();
        DeclAst::Function(FunctionAst { prototype, body, })
    }
}


mod test {
    use std::io::{BufReader, Cursor};
    use crate::lexer::Lexer;
    use crate::parser::ExprAst::{Call, Variable};
    use crate::parser::{FunctionAst, Parser, PrototypeAst};
    use crate::parser::DeclAst::{Extern, Function};

    #[test]
    pub fn test_nested_functions() {
        let input = r"
            extern println()

            # Pick the left
            def pickLeft(x y) x

            # Pick the middle one
            def pickMiddle(x y z) pickLeft(pickLeft(y, z), z)
        ";

        let reader = BufReader::new(Cursor::new(input));
        let tokens = Parser::new(Lexer::new(reader)).parse();
        println!("{:?}", tokens);

        let expected_ast = [
            Extern(PrototypeAst { name: "println".to_string(), args: vec![] }),
            Function(FunctionAst {
                prototype: PrototypeAst { name: "pickLeft".to_string(), args: vec!["x".to_string(), "y".to_string()] },
                body: Variable("x".to_string())
            }),
            Function(FunctionAst {
                prototype: PrototypeAst { name: "pickMiddle".to_string(), args: vec!["x".to_string(), "y".to_string(), "z".to_string()] },
                body: Call {
                    callee: "pickLeft".to_string(),
                    args: vec![
                        Box::new(Call {
                            callee: "pickLeft".to_string(),
                            args: vec![
                                Box::new(Variable("y".to_string())),
                                Box::new(Variable("z".to_string()))
                            ]
                        }),
                        Box::new(Variable("z".to_string()))
                    ]
                }
            })
        ];
        assert_eq!(tokens, expected_ast);
    }
}