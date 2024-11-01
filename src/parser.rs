use std::fmt::Debug;
use std::io::BufRead;
use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Operator {
    Add,
    Multiply,
    Divide,
    Subtract,
    Lt,
    Gt,
    Eq,
    None,
}

impl Operator {
    fn precedence(&self) -> u32 {
        match self {
            Operator::Add => 10,
            Operator::Subtract => 10,
            Operator::Multiply => 20,
            Operator::Divide => 20,
            Operator::Lt => 30,
            Operator::Gt => 30,
            Operator::Eq => 40,
            Operator::None => u32::MAX,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BinaryAst {
    op: Operator,
    lhs: ExprAst,
    rhs: ExprAst,
}

impl BinaryAst {
    pub fn new(op: Operator, lhs: ExprAst, rhs: ExprAst) -> ExprAst {
        ExprAst::Binary(Box::new(BinaryAst { op, lhs, rhs, }))
    }
}

#[derive(Debug, PartialEq)]
pub enum ExprAst {
    Number(f64),
    Variable(String),
    Binary(Box<BinaryAst>),
    Call {
        callee: String,
        args: Vec<ExprAst>,
    },
    Err(String)
}

impl ExprAst {
    pub fn variable(var: &str) -> ExprAst {
        ExprAst::Variable(var.to_string())
    }
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

    pub fn peek_token(&mut self) -> &Token {
        match self.last_token {
            Some(ref last_token) => last_token,
            None => self.last_token.insert(self.lexer.read_token()),
        }
    }

    pub fn read_token(&mut self) -> Token {
        match self.last_token {
            None => self.lexer.read_token(),
            Some(_) => self.last_token.take().unwrap(),
        }
    }

    pub fn consume_token(&mut self) {
        match self.last_token {
            None => (),
            Some(_) => self.last_token = None,
        };
    }

    pub fn parse_iden_expr(&mut self, iden: String) -> ExprAst {
        let token = self.peek_token();
        match token {
            Token::LParen => {
                self.consume_token(); // consume the lparen
                let mut args = vec![];
                loop {
                    let mut parens = 0;
                    let arg_expr = self.parse_expr(&mut parens);
                    args.push(arg_expr);

                    let token = self.read_token();
                    match token {
                        Token::Comma => (), // consume the comma and keep parsing args
                        Token::RParen => break, // stop parsing args
                        token => panic!("Expected an rparen token, got {:?}", token),
                    }
                }
                ExprAst::Call{ callee: iden, args, }
            },
            _ => ExprAst::Variable(iden),
        }
    }

    pub fn parse_operator(&mut self) -> Operator {
        match self.read_token() {
            Token::Plus => Operator::Add,
            Token::Minus => Operator::Subtract,
            Token::Times => Operator::Multiply,
            Token::Slash => Operator::Divide,
            Token::Gt => Operator::Gt,
            Token::Lt => Operator::Lt,
            Token::Eq => Operator::Eq,
            token => panic!("Expected an operator, got {:?}", token),
        }
    }

    pub fn parse_binop_rhs(&mut self, lhs: ExprAst, curr_op: Operator, parens: &mut i32) -> ExprAst {
        let token = self.read_token();
        let expr = match token {
            Token::Number(num) => ExprAst::Number(num),
            Token::Iden(iden) => self.parse_iden_expr(iden),
            token => panic!("Expected an variable, call, or numeric token, got {:?}", token),
        };

        if self.peek_token().is_sentinel() {
            // if we get a signal that we have the last token, just return the expr as the new rhs and be done
            BinaryAst::new(curr_op, lhs, expr)
        } else {
            let next_op = self.parse_operator();

            if next_op.precedence() > curr_op.precedence() {
                let rhs = self.parse_binop_rhs(expr, next_op, parens);
                BinaryAst::new(curr_op, lhs, rhs)
            } else {
                let lhs = BinaryAst::new(curr_op, lhs, expr);
                self.parse_binop_rhs(lhs, next_op, parens)
            }
        }
    }

    pub fn parse_expr(&mut self, parens: &mut i32) -> ExprAst {
        let token = self.read_token();
        let expr = match token {
            Token::Number(num) => ExprAst::Number(num),
            Token::Iden(iden) => self.parse_iden_expr(iden),
            token => panic!("Expected an iden, numeric, or lparen token at the start of an expression got {:?}", token),
        };

        // check try to parse the rhs if it exists
        if self.peek_token().is_sentinel() {
            expr
        } else {
            let op = self.parse_operator();
            self.parse_binop_rhs(expr, op, parens)
        }
    }

    pub fn parse(&mut self) -> Vec<DeclAst> {
        let mut nodes = vec![];

        loop {
            let token = self.read_token();
            let node = match token {
                Token::Extern => DeclAst::Extern(self.parse_prototype()),
                Token::Def => {
                    let prototype = self.parse_prototype();
                    let mut parens = 0;
                    let body = self.parse_expr(&mut parens);
                    DeclAst::Function(FunctionAst { prototype, body, })
                },
                Token::Eof => return nodes,
                _ => panic!("Expected a toplevel node declaration to be either an extern or a def"),
            };
            nodes.push(node);
        }
    }

    pub fn parse_prototype(&mut self) -> PrototypeAst {
        let token = self.read_token();
        let name = match token {
            Token::Iden(iden) => iden,
            token => panic!("Expected an iden token while parsing prototype, got {:?}", token),
        };
        let token = self.read_token();
        match token {
            Token::LParen => (), // discard
            token => panic!("Expected an lparen token while parsing prototype got {:?}", token),
        };
        let mut args = vec![];
        loop {
            let token = self.read_token();
            let arg = match token {
                Token::Iden(iden) => iden,
                Token::RParen => break,
                token => panic!("Expected an iden token while parsing prototype got {:?}", token),
            };
            args.push(arg);
        }
        PrototypeAst { name, args }
    }
}

mod test {
    use std::io::{BufReader, Cursor};
    use crate::lexer::Lexer;
    use crate::parser::ExprAst::{Call, Variable};
    use crate::parser::{BinaryAst, ExprAst, FunctionAst, Parser, PrototypeAst};
    use crate::parser::DeclAst::{Extern, Function};
    use crate::parser::Operator::{Add, Multiply, Subtract};

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
                        Call {
                            callee: "pickLeft".to_string(),
                            args: vec![
                                Variable("y".to_string()),
                                Variable("z".to_string())
                            ]
                        },
                        Variable("z".to_string())
                    ]
                }
            })
        ];
        assert_eq!(tokens, expected_ast);
    }

    #[test]
    pub fn test_binop_exprs() {
        let input = r"
            def calculate1(a b c d) a * b * c + d

            def calculate2(a b c d) a + b + c * d

            def calculate3(a b c d) a + b * c + d

            def calculate4(a b c d) a * b - c * d
        ";

        let reader = BufReader::new(Cursor::new(input));
        let tokens = Parser::new(Lexer::new(reader)).parse();
        println!("{:?}", tokens);

        let expected_ast = [
            Function(FunctionAst {
                prototype: PrototypeAst { name: "calculate1".to_string(), args: vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string()] },
                body: BinaryAst::new(
                    Add,
                    BinaryAst::new(
                        Multiply,
                        BinaryAst::new(Multiply, ExprAst::variable("a"), ExprAst::variable("b")),
                        ExprAst::variable("c")
                    ),
                    ExprAst::variable("d")
                )
            }),
            Function(FunctionAst {
                prototype: PrototypeAst { name: "calculate2".to_string(), args: vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string()] },
                body: BinaryAst::new(
                    Add,
                    BinaryAst::new(Add, ExprAst::variable("a"), ExprAst::variable("b")),
                    BinaryAst::new(Multiply, ExprAst::variable("c"), ExprAst::variable("d"))
                )
            }),
            Function(FunctionAst {
                prototype: PrototypeAst { name: "calculate3".to_string(), args: vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string()] },
                body: BinaryAst::new(
                    Add,
                    ExprAst::variable("a"),
                    BinaryAst::new(
                        Add,
                        BinaryAst::new(Multiply, ExprAst::variable("b"), ExprAst::variable("c")),
                        ExprAst::variable("d")
                    )
                )
            }),
            Function(FunctionAst {
                prototype: PrototypeAst { name: "calculate4".to_string(), args: vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string()] },
                body: BinaryAst::new(
                    Subtract,
                    BinaryAst::new(Multiply, ExprAst::variable("a"), ExprAst::variable("b")),
                    BinaryAst::new(Multiply, ExprAst::variable("c"), ExprAst::variable("d"))
                )
            }),
        ];
        assert_eq!(tokens, expected_ast);
    }

    #[test]
    pub fn test_binop_subexprs() {
        let input = r"
            def calculate1(a b c d) (a - b) * (c + d) + a + b

            def calculate2(a b c d) (a + (b * (c / d)))
        ";

        let reader = BufReader::new(Cursor::new(input));
        let tokens = Parser::new(Lexer::new(reader)).parse();
        println!("{:?}", tokens);

        let expected_ast = [];
        assert_eq!(tokens, expected_ast);
    }

    #[test]
    pub fn test_binop_unmatched() {
        let input = r"
            def calculate1(a b c d) ((a - b) * ((c + d)
        ";

        let reader = BufReader::new(Cursor::new(input));
        let tokens = Parser::new(Lexer::new(reader)).parse();
        println!("{:?}", tokens);

        let expected_ast = [];
        assert_eq!(tokens, expected_ast);
    }
}