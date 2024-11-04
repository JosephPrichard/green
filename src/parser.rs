use std::fmt::Debug;
use std::io::BufRead;
use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Lt,
    Gt,
    Eq,
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
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BinaryAst {
    pub op: Operator,
    pub lhs: ExprAst,
    pub rhs: ExprAst,
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
        args: Box<[ExprAst]>,
    },
    Err(String)
}

impl ExprAst {
    pub fn variable(var: &str) -> ExprAst {
        ExprAst::Variable(var.to_string())
    }

    pub fn call(callee: &str, args: Vec<ExprAst>) -> ExprAst {
        ExprAst::Call { callee: String::from(callee), args: args.into_boxed_slice() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrototypeAst {
    pub name: String,
    pub args: Box<[String]>,
}

impl PrototypeAst {
    pub fn of(name: &str, args: &[&str]) -> PrototypeAst {
        let mut vargs = Vec::with_capacity(args.len());
        for arg in args {
            vargs.push(String::from(*arg))
        }
        PrototypeAst { name: name.to_string(), args: vargs.into_boxed_slice() }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionAst {
    pub prototype: PrototypeAst,
    pub body: ExprAst,
}

#[derive(Debug, PartialEq)]
pub enum Ast {
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
        let token = match self.last_token {
            None => self.lexer.read_token(),
            Some(_) => self.last_token.take().unwrap(),
        };
        // println!("{:?}", token);
        token
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
                    let arg_expr = self.parse_expr(0);
                    args.push(arg_expr);

                    let token = self.read_token();
                    match token {
                        Token::Comma => (), // consume the comma and keep parsing args
                        Token::RParen => break, // stop parsing args
                        token => panic!("Expected an rparen token, got {:?}", token),
                    }
                }
                ExprAst::Call{ callee: iden, args: args.into_boxed_slice(), }
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

    pub fn parse_binop_rhs(&mut self, lhs: ExprAst, curr_op: Operator, depth: u32) -> ExprAst {
        let token = self.read_token();
        let expr = match token {
            Token::LParen => self.parse_expr(depth + 1),
            Token::Number(num) => ExprAst::Number(num),
            Token::Iden(iden) => self.parse_iden_expr(iden),
            token => panic!("Expected an iden, numeric, or lparen token at the start of an expression, got {:?}", token),
        };

        let token = self.peek_token();
        match token {
            Token::Def | Token::Extern | Token::Comma | Token::Eof =>
                if depth == 0 {
                    BinaryAst::new(curr_op, lhs, expr)
                } else {
                    panic!("Reached the end of an expression with unclosed parenthesis")
                },
            Token::RParen => {
                if depth > 0 { // only consume if we're inside a nested subexpression
                    self.consume_token()
                }
                BinaryAst::new(curr_op, lhs, expr)
            },
            _ => {
                let next_op = self.parse_operator();

                if next_op.precedence() > curr_op.precedence() {
                    let rhs = self.parse_binop_rhs(expr, next_op, depth);
                    BinaryAst::new(curr_op, lhs, rhs)
                } else {
                    let lhs = BinaryAst::new(curr_op, lhs, expr);
                    self.parse_binop_rhs(lhs, next_op, depth)
                }
            }
        }
    }

    pub fn parse_expr(&mut self, depth: u32) -> ExprAst {
        let token = self.read_token();
        let expr = match token {
            Token::LParen => self.parse_expr(depth + 1),
            Token::Number(num) => ExprAst::Number(num),
            Token::Iden(iden) => self.parse_iden_expr(iden),
            token => panic!("Expected an iden, numeric, or lparen token at the start of an expression, got {:?}", token),
        };

        let token = self.peek_token();
        match token {
            Token::Def | Token::Extern | Token::Comma | Token::RParen | Token::Eof => expr,
            _ => {
                let op = self.parse_operator();
                self.parse_binop_rhs(expr, op, depth)
            }
        }
    }

    pub fn parse(&mut self) -> Vec<Ast> {
        let mut nodes = vec![];

        loop {
            let token = self.read_token();
            let node = match token {
                Token::Extern => Ast::Extern(self.parse_prototype()),
                Token::Def => {
                    let prototype = self.parse_prototype();
                    let body = self.parse_expr(0);
                    Ast::Function(FunctionAst { prototype, body, })
                },
                Token::Eof => return nodes,
                token => panic!("Expected a declaration to be either an extern or a def, got {:?}", token),
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
        PrototypeAst { name, args: args.into_boxed_slice() }
    }
}

mod test {
    use std::io::{BufReader, Cursor};
    use crate::lexer::Lexer;
    use crate::parser::ExprAst::{Call, Variable};
    use crate::parser::{BinaryAst, ExprAst, FunctionAst, Parser, PrototypeAst};
    use crate::parser::Ast::{Extern, Function};
    use crate::parser::Operator::{Add, Divide, Multiply, Subtract};

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
        let asts = Parser::new(Lexer::new(reader)).parse();
        println!("{:?}", asts);

        let expected_ast = [
            Extern(PrototypeAst::of("println", &[])),
            Function(FunctionAst {
                prototype: PrototypeAst::of("pickLeft", &["x", "y"]),
                body: ExprAst::variable("x")
            }),
            Function(FunctionAst {
                prototype: PrototypeAst::of("pickMiddle", &["x", "y", "z"]),
                body: ExprAst::call(
                    "pickLeft",
                    vec![
                        ExprAst::call(
                            "pickLeft",
                            vec![
                                ExprAst::variable("y"),
                                ExprAst::variable("z")
                            ]
                        ),
                        ExprAst::variable("z")
                    ]
                )
            })
        ];
        assert_eq!(asts, expected_ast);
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
        let asts = Parser::new(Lexer::new(reader)).parse();
        println!("{:?}", asts);

        let expected_ast = [
            Function(FunctionAst {
                prototype: PrototypeAst::of("calculate1", &["a", "b", "c", "d"]),
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
                prototype: PrototypeAst::of("calculate2", &["a", "b", "c", "d"]),
                body: BinaryAst::new(
                    Add,
                    BinaryAst::new(Add, ExprAst::variable("a"), ExprAst::variable("b")),
                    BinaryAst::new(Multiply, ExprAst::variable("c"), ExprAst::variable("d"))
                )
            }),
            Function(FunctionAst {
                prototype: PrototypeAst::of("calculate3", &["a", "b", "c", "d"]),
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
                prototype: PrototypeAst::of("calculate4", &["a", "b", "c", "d"]),
                body: BinaryAst::new(
                    Subtract,
                    BinaryAst::new(Multiply, ExprAst::variable("a"), ExprAst::variable("b")),
                    BinaryAst::new(Multiply, ExprAst::variable("c"), ExprAst::variable("d"))
                )
            }),
        ];
        assert_eq!(asts, expected_ast);
    }

    #[test]
    pub fn test_binop_subexprs() {
        let input = r"
            def calculate1(a b) (a - b)

            def calculate2(a b c d) (a - b) * (c + d) + a + b

            def calculate3(a b c d) (a + (b * (c - d + a)))
        ";

        let reader = BufReader::new(Cursor::new(input));
        let asts = Parser::new(Lexer::new(reader)).parse();
        println!("{:?}", asts);

        let expected_ast = [
            Function(FunctionAst {
                prototype: PrototypeAst::of("calculate1", &["a", "b"]),
                body: BinaryAst::new(Subtract, Variable("a".to_string()), Variable("b".to_string()))
            }),
            Function(FunctionAst {
                prototype: PrototypeAst::of("calculate2", &["a", "b", "c", "d"]),
                body: BinaryAst::new(
                    Add,
                    BinaryAst::new(
                        Add,
                        BinaryAst::new(
                            Multiply,
                            BinaryAst::new(Subtract, ExprAst::variable("a"),  ExprAst::variable("b")),
                            BinaryAst::new(Add, ExprAst::variable("c"),  ExprAst::variable("d"))
                        ),
                        ExprAst::variable("a")
                    ),
                    ExprAst::variable("b")
                )
            }),
            Function(FunctionAst {
                prototype: PrototypeAst::of("calculate3", &["a", "b", "c", "d"]),
                body: BinaryAst::new(
                    Add,
                    ExprAst::variable("a"),
                    BinaryAst::new(
                        Multiply,
                        ExprAst::variable("b"),
                        BinaryAst::new(
                            Add,
                            BinaryAst::new(Subtract, ExprAst::variable("c"), ExprAst::variable("d")),
                            ExprAst::variable("a")
                        )
                    )
                )
            })
        ];
        assert_eq!(asts, expected_ast);
    }

    #[test]
    #[should_panic]
    pub fn test_binop_unmatched() {
        let input = r"
            def calculate1(a b c d) ((a - b) * ((c + d)
        ";

        let reader = BufReader::new(Cursor::new(input));
        Parser::new(Lexer::new(reader)).parse();
    }
}