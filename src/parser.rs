use std::io::BufRead;
use crate::lexer::{Lexer, Token};

pub enum Operator {
    Add,
    Multiply,
    Divide,
    Subtract,
    Lt,
    Gt,
    Eq
}

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
    }
}

pub struct PrototypeAst {
    name: String,
    args: Vec<String>,
}

pub struct FunctionAst {
    prototype: PrototypeAst,
    body: ExprAst,
}

pub enum DeclAst {
    Extern(PrototypeAst),
    Function(FunctionAst)
}

pub struct Parser<R: BufRead> {
    lexer: Lexer<R>,
}

impl<R: BufRead> Parser<R> {
    pub fn new(lexer: Lexer<R>) -> Parser<R> {
        return Parser{ lexer };
    }

    pub fn read_token(&mut self) -> Option<Token> {
        return self.lexer.read_token();
    }

    pub fn parse_number_expr(&mut self) -> ExprAst {
        let opt_tok = self.read_token();
        match opt_tok {
            Some(Token::Number(num)) => ExprAst::Number(num),
            Some(token) => panic!("Expected a numeric token, got a {:?} ", token),
            None => panic!("Reached the end of the token stream while parsing a numeric expression"),
        }
    }

    pub fn parse_iden_expr() {

    }

    pub fn parse_primary_expr(&mut self) {
        match opt_tok {

        }
    }

    pub fn parse_binop_rhs() {

    }

    pub fn parse_expr(&mut self) -> ExprAst {

    }

    pub fn parse(&mut self) -> Vec<DeclAst> {
        let mut nodes = vec![];

        while let Some(token) = self.read_token() {
            let node = match token {
                Token::Extern => DeclAst::Extern(self.parse_prototype()),
                Token::Def => DeclAst::Function(self.parse_function()),
                _ => panic!("Expected a toplevel node declaration to be either an extern or a def"),
            };
            nodes.push(node);
        }

        nodes
    }

    pub fn parse_prototype(&mut self) -> PrototypeAst {
        let opt_tok = self.read_token();
        let name = match opt_tok {
            Some(Token::Iden(iden)) => iden,
            Some(token) => panic!("Expected an iden token while parsing prototype, got a {:?}", token),
            None => panic!("Reached the end of the token stream while parsing a prototype"),
        };
        let opt_tok = self.read_token();
        match opt_tok {
            Some(Token::LParen) => (), // discard
            Some(token) => panic!("Expected an lparen token while parsing prototype got a {:?}", token),
            None => panic!("Reached the end of the token stream while parsing a prototype"),
        };
        let mut args = vec![];
        loop {
            let opt_tok = self.read_token();
            let arg = match opt_tok {
                Some(Token::Iden(iden)) => iden,
                Some(Token::RParen) => break,
                Some(token) => panic!("Expected an iden token while parsing prototype got a {:?}", token),
                None => panic!("Reached the end of the token stream while parsing a prototype argument list"),
            };
            args.push(arg);
        }
        PrototypeAst { name, args }
    }

    pub fn parse_function(&mut self) -> FunctionAst {
        let prototype = self.parse_prototype();
        let body = self.parse_expr();
        FunctionAst { prototype, body, }
    }
}