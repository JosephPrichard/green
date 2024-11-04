use std::collections::HashMap;
use std::fmt::Write;
use crate::parser::{Ast, BinaryAst, ExprAst, FunctionAst, Operator, PrototypeAst};

pub struct CodegenCtx<'ast, W: Write> {
    w: &'ast mut W,
    index: u32,
    func_table: HashMap<&'ast str, &'ast PrototypeAst>, // maps function names to prototypes
    var_table: HashMap<&'ast str, String> // maps variable names to values
}

impl<'ast, W: Write> CodegenCtx<'ast, W> {
    pub fn new(writer: &'ast mut W) -> CodegenCtx<'ast, W> {
        CodegenCtx {
            w: writer,
            index: 0,
            func_table: HashMap::new(),
            var_table: HashMap::new()
        }
    }

    pub fn codegen(&mut self, program: &'ast [Ast]) -> Result<(), String> {
        for decl in program {
            self.codegen_decl(decl)?;
        }
        Ok(())
    }

    fn codegen_prototype(&mut self, proto: &'ast PrototypeAst, alias: bool) {
        self.func_table.insert(proto.name.as_str(), &proto);
        self.var_table.clear();

        write!(self.w, "define double @{}(", if proto.name == "main" { "0" } else { &proto.name }).unwrap();

        for (i, arg) in proto.args.iter().enumerate() {
            let reg = self.create_reg();
            write!(self.w, "double").unwrap();
            if alias {
                write!(self.w, " {}", reg).unwrap();
            }
            if i < proto.args.len() - 1 {
                write!(self.w, ", ").unwrap();
            }
            self.var_table.insert(arg, reg);
        }

        write!(self.w, ")").unwrap();
    }

    fn codegen_function(&mut self, func: &'ast FunctionAst) -> Result<(), String> {
        self.codegen_prototype(&func.prototype, true);

        writeln!(self.w, " {{").unwrap();

        let mut bb = "entry:\n".to_string();

        let reg = self.codegen_expr(&mut bb, &func.body)?;
        writeln!(bb, "\tret double {}", reg).unwrap();

        write!(self.w, "{}", bb).unwrap();
        writeln!(self.w, "}}\n").unwrap();

        Ok(())
    }

    pub fn codegen_decl(&mut self, ast: &'ast Ast) -> Result<(), String> {
        match ast {
            Ast::Extern(proto) => {
                self.codegen_prototype(proto, false);
                writeln!(self.w, "\n").unwrap();
                Ok(())
            },
            Ast::Function(func) => self.codegen_function(func),
            Ast::Err(err) => panic!("Fatal: encountered error node in codegen phase {}", err),
        }
    }

    pub fn create_reg(&mut self) -> String {
        self.index += 1;
        format!("%tmp{}", self.index)
    }

    fn codegen_call(&mut self, bb: &mut String, callee: &str, args: &'ast [ExprAst]) -> Result<String, String> {
        match self.func_table.get(callee) {
            None => panic!("Unbound function reference: '{}'", callee),
            Some(proto) => {
                if proto.args.len() != args.len() {
                    return Err(format!("Function {} is defined with {} args, but called with {}", proto.name, proto.args.len(), args.len()))
                }
            }
        };

        let mut values = vec![];
        for expr in args {
            values.push(self.codegen_expr(bb, expr)?);
        }

        let reg = self.create_reg();
        write!(bb, "\t{} = call double @{}(", reg, callee).unwrap();
        for (i, value) in values.iter().enumerate() {
            write!(bb, "double {}", value).unwrap();
            if i < values.len() - 1 {
                write!(bb, ", ").unwrap();
            }
        }
        writeln!(bb, ")").unwrap();

        Ok(reg)
    }

    fn codegen_binary(&mut self, bb: &mut String, ast: &'ast BinaryAst) -> Result<String, String> {
        let lhs = self.codegen_expr(bb, &ast.lhs)?;
        let rhs = self.codegen_expr(bb, &ast.rhs)?;

        let reg = match ast.op {
            Operator::Add => {
                let reg = self.create_reg();
                writeln!(bb, "\t{} = fadd float {}, {}", reg, lhs, rhs).unwrap();
                reg
            },
            Operator::Subtract => {
                let reg = self.create_reg();
                writeln!(bb, "\t{} = fsub float {}, {}", reg, lhs, rhs).unwrap();
                reg
            },
            Operator::Multiply => {
                let reg = self.create_reg();
                writeln!(bb, "\t{} = fmul float {}, {}", reg, lhs, rhs).unwrap();
                reg
            },
            Operator::Divide => {
                let reg = self.create_reg();
                writeln!(bb, "\t{} = fdiv float {}, {}", reg, lhs, rhs).unwrap();
                reg
            },
            Operator::Lt => {
                let reg1 = self.create_reg();
                writeln!(bb, "\t{} = fcmp ult float {}, {}\n", reg1, lhs, rhs).unwrap();
                let reg2 = self.create_reg();
                writeln!(bb, "\t{} = uitofp <1 x i1> {} to <1 x float>\n", reg2, reg1).unwrap();
                reg2
            }
            Operator::Gt => {
                let reg1 = self.create_reg();
                writeln!(bb, "\t{} = fcmp ugt float {}, {}\n", reg1, lhs, rhs).unwrap();
                let reg2 = self.create_reg();
                writeln!(bb, "\t{} = uitofp <1 x i1> {} to <1 x float>n", reg2, reg1).unwrap();
                reg2
            }
            Operator::Eq => {
                let reg1 = self.create_reg();
                writeln!(bb, "\t{} = fcmp ueq float {}, {}\n", reg1, lhs, rhs).unwrap();
                let reg2 = self.create_reg();
                writeln!(bb, "\t{} = uitofp <1 x i1> {} to <1 x float>\n", reg2, reg1).unwrap();
                reg2
            }
        };
        Ok(reg)
    }

    // an expression evaluates to a string that is either a literal or a register
    pub fn codegen_expr(&mut self, bb: &mut String, expr: &'ast ExprAst) -> Result<String, String> {
        match expr {
            ExprAst::Number(num) => Ok(num.to_string()),
            ExprAst::Variable(var) => {
                match self.var_table.get(var.as_str()) {
                    None => panic!("Unbound variable reference: '{}'", var),
                    Some(value) => Ok(value.clone()),
                }
            }
            ExprAst::Binary(ast) => self.codegen_binary(bb, ast.as_ref()),
            ExprAst::Call { callee, args } => self.codegen_call(bb, callee, args),
            ExprAst::Err(err) => panic!("Fatal: encountered error node in codegen phase {}", err),
        }
    }
}

mod test {
    use std::io::{BufReader, BufWriter, Cursor};
    use crate::codegen::CodegenCtx;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    pub fn test_compile_formula() {
        let input = r"
            extern sqrt(x)

            def foo(a b) a*a + 2*a*b + b*b

            def bar(a b) foo(a * 2, b + 1) + sqrt(4)

            def main() bar(1, 2)
        ";

        let reader = BufReader::new(Cursor::new(input));
        let asts = Parser::new(Lexer::new(reader)).parse();

        let mut output = String::new();

        let mut codegen = CodegenCtx::new(&mut output);
        codegen.codegen(&asts).unwrap();

        println!("{}", output);
    }
}