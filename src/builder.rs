use std::fmt::Write;

pub struct IRBuilder<'w, W> {
    w: &'w mut W,
    index: u32,
}

impl <'w, W: Write> IRBuilder<'w, W> {
    pub fn new(w: &'w mut W) -> IRBuilder<'w, W> {
        IRBuilder { w, index: 0, }
    }

    pub fn create_reg(&mut self) -> String {
        self.index += 1;
        format!("%tmp{}", self.index)
    }

    pub fn create_label(&mut self, prefix: &str) -> String {
        self.index += 1;
        format!("{}{}", prefix, self.index)
    }

    pub fn fadd(&mut self, ret: &str, arg1: &str, arg2: &str) {
        writeln!(self.w, "\t{} = fadd float {}, {}", ret, arg1, arg2).unwrap();
    }

    pub fn fsub(&mut self, ret: &str, arg1: &str, arg2: &str) {
        writeln!(self.w, "\t{} = fsub float {}, {}", ret, arg1, arg2).unwrap();
    }

    pub fn fmul(&mut self, ret: &str, arg1: &str, arg2: &str) {
        writeln!(self.w, "\t{} = fmul float {}, {}", ret, arg1, arg2).unwrap();
    }

    pub fn fdiv(&mut self, ret: &str, arg1: &str, arg2: &str) {
        writeln!(self.w, "\t{} = fdiv float {}, {}", ret, arg1, arg2).unwrap();
    }

    pub fn fcmp_ult(&mut self, ret: &str, arg1: &str, arg2: &str) {
        writeln!(self.w, "\t{} = fcmp ult float {}, {}", ret, arg1, arg2).unwrap();
    }

    pub fn fcmp_ugt(&mut self, ret: &str, arg1: &str, arg2: &str) {
        writeln!(self.w, "\t{} = fcmp ugt float {}, {}", ret, arg1, arg2).unwrap();
    }

    pub fn fcmp_ueq(&mut self, ret: &str, arg1: &str, arg2: &str) {
        writeln!(self.w, "\t{} = fcmp ueq float {}, {}", ret, arg1, arg2).unwrap();
    }

    pub fn uitofp(&mut self, ret: &str, arg: &str) {
        writeln!(self.w, "\t{} = uitofp <1 x i1> {} to <1 x float>", ret, arg).unwrap();
    }

    pub fn begin_basic_block(&mut self, bb_name: &str) {
        writeln!(self.w, "{}:", bb_name).unwrap();
    }

    pub fn ret(&mut self, reg: &str) {
        writeln!(self.w, "\tret double {}", reg).unwrap();
    }

    pub fn begin_func_body(&mut self) {
        writeln!(self.w, " {{").unwrap();
    }

    pub fn end_func_body(&mut self) {
        writeln!(self.w, "}}\n").unwrap();
    }

    pub fn end_prototype(&mut self) {
        writeln!(self.w, "\n").unwrap();
    }

    pub fn def_func(&mut self, name: &str, regs: &[String], alias: bool) {
        write!(self.w, "define double @{}(", name).unwrap();

        for (i, reg) in regs.iter().enumerate() {

            write!(self.w, "double").unwrap();
            if alias {
                write!(self.w, " {}", reg).unwrap();
            }
            if i < regs.len() - 1 {
                write!(self.w, ", ").unwrap();
            }
        }

        write!(self.w, ")").unwrap();
    }

    pub fn call(&mut self, ret: &str, callee: &str, args: &[String]) {
        write!(self.w, "\t{} = call double @{}(", ret, callee).unwrap();

        for (i, value) in args.iter().enumerate() {
            write!(self.w, "double {}", value).unwrap();
            if i < args.len() - 1 {
                write!(self.w, ", ").unwrap();
            }
        }
        writeln!(self.w, ")").unwrap();
    }

    pub fn branch(&mut self, bb: &str) {
        writeln!(self.w, "\tbr label %{}", bb).unwrap();
    }

    pub fn cond_branch(&mut self, cond: &str, bb1: &str, bb2: &str) {
        writeln!(self.w, "\tbr i1 {}, label %{}, label %{}", cond, bb1, bb2).unwrap();
    }

    pub fn phi(&mut self, ret: &str, arg1: &str, bb1: &str, arg2: &str, bb2: &str) {
        writeln!(self.w, "\t{} = phi float [{}, %{}], [{}, %{}]", ret, arg1, bb1, arg2, bb2).unwrap();
    }
}