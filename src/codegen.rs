use std::collections::HashMap;
use std::fmt::Write;
use crate::builder::IRBuilder;
use crate::parser::{Ast, BinaryAst, ExprAst, ForAst, FunctionAst, IfAst, Operator, PrototypeAst};

pub struct CodegenCtx<'ast, W: Write> {
    builder: IRBuilder<'ast, W>,
    prev_lbl: Option<String>,
    func_table: HashMap<&'ast str, &'ast PrototypeAst>, // maps function names to prototypes
    var_table: HashMap<&'ast str, String> // maps variable names to values
}

impl<'ast, W: Write> CodegenCtx<'ast, W> {
    pub fn new(builder: IRBuilder<'ast, W>) -> CodegenCtx<'ast, W> {
        CodegenCtx {
            builder,
            prev_lbl: None,
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
        self.func_table.insert(&proto.name, &proto);
        self.var_table.clear();

        let name = if proto.name == "main" { "0" } else { &proto.name };

        let regs: Vec<String> = proto.args.iter().map(|_| self.builder.create_reg()).collect();

        self.builder.def_func(name, &regs, alias);

        assert_eq!(proto.args.len(), regs.len());
        for (arg, reg) in proto.args.iter().zip(regs) {
            self.var_table.insert(arg, reg);
        }
    }

    fn codegen_function(&mut self, func: &'ast FunctionAst) -> Result<(), String> {
        self.codegen_prototype(&func.prototype, true);

        self.builder.begin_func_body();

        self.builder.begin_basic_block("entry");
        self.prev_lbl = Some("entry".to_string());

        let reg = self.codegen_expr(&func.body)?;
        self.builder.ret(&reg);

        self.builder.end_func_body();
        Ok(())
    }

    fn codegen_decl(&mut self, ast: &'ast Ast) -> Result<(), String> {
        match ast {
            Ast::Extern(proto) => {
                self.codegen_prototype(proto, false);
                self.builder.end_prototype();
                Ok(())
            },
            Ast::Function(func) => self.codegen_function(func),
            Ast::Err(err) => panic!("Fatal: encountered error node in codegen phase {}", err),
        }
    }

    fn codegen_var(&mut self, var: &str) -> Result<String, String> {
        match self.var_table.get(var) {
            None => panic!("Unbound variable reference: '{}'", var),
            Some(value) => Ok(value.clone()),
        }
    }

    fn codegen_call(&mut self, callee: &str, args: &'ast [ExprAst]) -> Result<String, String> {
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
            values.push(self.codegen_expr(expr)?);
        }

        let reg = self.builder.create_reg();
        self.builder.call(reg.as_str(), callee, &values);
        Ok(reg)
    }

    fn codegen_bin_expr(&mut self, ast: &'ast BinaryAst) -> Result<String, String> {
        let lhs = self.codegen_expr(&ast.lhs)?;
        let rhs = self.codegen_expr(&ast.rhs)?;

        let reg = match ast.op {
            Operator::Add => {
                let ret = self.builder.create_reg();
                self.builder.fadd(&ret, &lhs, &rhs);
                ret
            },
            Operator::Subtract => {
                let ret = self.builder.create_reg();
                self.builder.fsub(&ret, &lhs, &rhs);
                ret
            },
            Operator::Multiply => {
                let ret = self.builder.create_reg();
                self.builder.fmul(&ret, &lhs, &rhs);
                ret
            },
            Operator::Divide => {
                let ret = self.builder.create_reg();
                self.builder.fdiv(&ret, &lhs, &rhs);
                ret
            },
            Operator::Lt => {
                let ret1 = self.builder.create_reg();
                self.builder.fcmp_ult(&ret1, &lhs, &rhs);
                let ret2 = self.builder.create_reg();
                self.builder.uitofp(&ret2, &ret1);
                ret2
            },
            Operator::Gt => {
                let ret1 = self.builder.create_reg();
                self.builder.fcmp_ugt(&ret1, &lhs, &rhs);
                let ret2 = self.builder.create_reg();
                self.builder.uitofp(&ret2, &ret1);
                ret2
            },
            Operator::Eq => {
                let ret1 = self.builder.create_reg();
                self.builder.fcmp_ueq(&ret1, &lhs, &rhs);
                let ret2 = self.builder.create_reg();
                self.builder.uitofp(&ret2, &ret1);
                ret2
            }
        };
        Ok(reg)
    }

    fn codegen_if_expr(&mut self, expr: &'ast IfAst) -> Result<String, String> {
        let cond = self.codegen_expr(&expr.cond)?;

        let then_lbl = self.builder.create_label("then");
        let other_lbl = self.builder.create_label("else");
        let end_lbl = self.builder.create_label("end");

        self.builder.cond_branch(&cond, &then_lbl, &other_lbl);

        self.builder.begin_basic_block(&then_lbl);
        let then = self.codegen_expr(&expr.then)?;
        self.builder.branch(&end_lbl);

        self.builder.begin_basic_block(&other_lbl);
        let otherwise = self.codegen_expr(&expr.otherwise)?;
        self.builder.branch(&end_lbl);

        self.builder.begin_basic_block(&end_lbl);
        let reg = self.builder.create_reg();
        self.builder.phi(&reg, &then, &then_lbl, &otherwise, &other_lbl);

        self.prev_lbl = Some(end_lbl);

        Ok(reg)
    }

    fn codegen_for_expr(&mut self, ast: &'ast ForAst) -> Result<String, String> {
        let beg = self.codegen_expr(&ast.beg)?;
        let end = self.codegen_expr(&ast.end)?;

        let count_reg = self.builder.create_reg();
        let next_reg = self.builder.create_reg();

        let loop_lbl = self.builder.create_label("loop");
        let end_lbl = self.builder.create_label("end");

        self.builder.branch(&loop_lbl);

        let begin_lbl = self.prev_lbl.as_ref().unwrap(); // we SHOULD be inside a basic block, so this should not be None

        self.builder.begin_basic_block(&loop_lbl);
        self.builder.phi(&count_reg, &beg, &begin_lbl, &next_reg, &loop_lbl);

        self.var_table.insert(&ast.var, count_reg.clone()); // we need to capture the counter variable as a loop variable
        self.codegen_expr(&ast.body)?; // discord the return value for non errors.

        self.builder.fadd(&next_reg, &count_reg, &1.to_string());

        let cmp_reg = self.builder.create_reg();
        let cast_reg = self.builder.create_reg();
        self.builder.fcmp_ueq(&cmp_reg, &next_reg, &end);
        self.builder.uitofp(&cast_reg, &cmp_reg);

        self.builder.cond_branch(&cast_reg, &loop_lbl, &end_lbl);

        self.builder.begin_basic_block(&end_lbl);

        self.var_table.remove(ast.var.as_str()); // ok, we can remove the counter variable at the end of the loop

        self.prev_lbl = Some(end_lbl);

        Ok(0.to_string())
    }

    // an expression evaluates to a string that is either a literal or a register
    fn codegen_expr(&mut self, expr: &'ast ExprAst) -> Result<String, String> {
        match expr {
            ExprAst::Number(num) => Ok(num.to_string()),
            ExprAst::Variable(var) => self.codegen_var(&var),
            ExprAst::Binary(ast) => self.codegen_bin_expr(ast.as_ref()),
            ExprAst::Call{ callee, args } => self.codegen_call(callee, args),
            ExprAst::If(ast) => self.codegen_if_expr(ast.as_ref()),
            ExprAst::For(ast) => self.codegen_for_expr(ast.as_ref()),
            ExprAst::Err(err) => panic!("Fatal: encountered error node in codegen phase {}", err),
        }
    }
}

mod test {
    use std::io::{BufReader, Cursor};
    use crate::builder::IRBuilder;
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

        let mut codegen = CodegenCtx::new(IRBuilder::new(&mut output));
        codegen.codegen(&asts).unwrap();

        println!("{}", output);
    }

    #[test]
    pub fn test_compile_cond() {
        let input = r"
            extern puts(str)

            def cond(a b) if (a = b) then (if (b < 10) then (a + b + 10) else 10) else (b + 50 * 2)

            def loop(x)
                for i in 0 to x:
                    puts(i)

            def main() cond(10, 50)
        ";

        let reader = BufReader::new(Cursor::new(input));
        let asts = Parser::new(Lexer::new(reader)).parse();

        let mut output = String::new();

        let mut codegen = CodegenCtx::new(IRBuilder::new(&mut output));
        codegen.codegen(&asts).unwrap();

        println!("{}", output);
    }
}