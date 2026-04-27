use crate::parser::parser::{
    BinaryOperator,
    Expression,
    ExpressionKind,
    FunctionDef,
    PostfixOperator,
    Program,
    Statement,
    StatementKind,
    UnaryOperator,
};

#[derive(Debug, Clone)]
pub enum TACInstruction {
    // x = y op z
    BinaryOp {
        result: String,
        left: String,
        op: String,
        right: String,
    },
    // x = op y
    UnaryOp {
        result: String,
        op: String,
        operand: String,
    },
    // x = y
    Copy {
        result: String,
        value: String,
    },
    // param x
    Param {
        value: String,
    },
    // x = call f, n
    Call {
        result: Option<String>,
        func: String,
        num_args: String,
    },
    // return x
    Return {
        value: Option<String>,
    },
    // label;
    Label {
        name: String,
    },
    // Goto label;
    Goto {
        label: String,
    },
    // if x goto label;
    IfGoto{
        condition: String,
        label: String,
    },
    // iffalse x goto label;
    IfFalseGoto{
        condition: String,
        label: String,
    },
    // print x
    Print {
        value: String,
    },
    // input x
    Input {
        var: String,
    },
    // 
    FuncBegin {
        name: String,
        params: Vec<String>,
    },
    // 
    FuncEnd {
        name: String,
    },
}

impl std::fmt::Display for TACInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TACInstruction::BinaryOp {
                result,
                left,
                op,
                right,
            } => write!(f, "    {} = {} {} {}", result, left, op, right),
 
            TACInstruction::UnaryOp {
                result,
                op,
                operand,
            } => write!(f, "    {} = {} {}", result, op, operand),
 
            TACInstruction::Copy { result, value } => {
                write!(f, "    {} = {}", result, value)
            }
 
            TACInstruction::Param { value } => write!(f, "    param {}", value),
 
            TACInstruction::Call {
                result,
                func,
                num_args,
            } => match result {
                Some(r) => write!(f, "    {} = call {}, {}", r, func, num_args),
                None => write!(f, "    call {}, {}", func, num_args),
            },
 
            TACInstruction::Return { value } => match value {
                Some(v) => write!(f, "    return {}", v),
                None => write!(f, "    return"),
            },
 
            TACInstruction::Label { name } => write!(f, "{}:", name),
 
            TACInstruction::Goto { label } => write!(f, "    goto {}", label),
 
            TACInstruction::IfGoto { condition, label } => {
                write!(f, "    if {} goto {}", condition, label)
            }
 
            TACInstruction::IfFalseGoto { condition, label } => {
                write!(f, "    iffalse {} goto {}", condition, label)
            }
 
            TACInstruction::Print { value } => write!(f, "    print {}", value),
 
            TACInstruction::Input { var } => write!(f, "    input {}", var),
 
            TACInstruction::FuncBegin { name, params } => {
                if params.is_empty() {
                    write!(f, "func_begin {}", name)
                } else {
                    write!(f, "func_begin {} ({})", name, params.join(", "))
                }
            }
 
            TACInstruction::FuncEnd { name } => write!(f, "func_end {}", name),
 
        }
    }
}

pub struct TACGenerator {
    pub instructions: Vec<TACInstruction>,
    temp_counter: usize,
    label_counter: usize,
    /// Stack of (loop_start_label, loop_end_label) for break / continue
    loop_stack: Vec<(String, String)>,
}

impl TACGenerator {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            temp_counter: 0,
            label_counter: 0,
            loop_stack: Vec::new(),
        }
    }
 
    // ── helpers ──────────────────────────────────────────────────────────
 
    fn new_temp(&mut self) -> String {
        let t = format!("t{}", self.temp_counter);
        self.temp_counter += 1;
        t
    }
 
    fn new_label(&mut self) -> String {
        let l = format!("L{}", self.label_counter);
        self.label_counter += 1;
        l
    }
 
    fn emit(&mut self, instr: TACInstruction) {
        self.instructions.push(instr);
    }
 
    // ── public entry point ──────────────────────────────────────────────
 
    pub fn generate(program: &Program) -> Vec<TACInstruction> {
        let mut generator = TACGenerator::new();
        for func in &program.functions {
            generator.gen_function(func);
        }
        generator.instructions
    }
 
    // ── functions ───────────────────────────────────────────────────────
 
    fn gen_function(&mut self, func: &FunctionDef) {
        let param_names: Vec<String> = func.params.iter().map(|p| p.name.clone()).collect();
 
        self.emit(TACInstruction::FuncBegin {
            name: func.name.clone(),
            params: param_names,
        });
 
        for stmt in &func.body {
            self.gen_statement(stmt);
        }
 
        self.emit(TACInstruction::FuncEnd {
            name: func.name.clone(),
        });
    }
 
    // ── statements ──────────────────────────────────────────────────────
 
    fn gen_statement(&mut self, stmt: &Statement) {
        match &stmt.kind {
            // ── declarations / assignments ───────────────────────────────
            StatementKind::VariableDeclaration { name, value, .. }
            | StatementKind::ConstDeclaration { name, value, .. } => {
                let val = self.gen_expression(value);
                self.emit(TACInstruction::Copy {
                    result: name.clone(),
                    value: val,
                });
            }
 
            StatementKind::Assignment { name, value } => {
                let val = self.gen_expression(value);
                self.emit(TACInstruction::Copy {
                    result: name.clone(),
                    value: val,
                });
            }
 
            // ── control flow ─────────────────────────────────────────────
            StatementKind::If {
                condition,
                then_block,
                elif_blocks,
                else_block,
            } => {
                let end_label = self.new_label();
 
                // first condition
                let mut next_label = self.new_label();
                let cond = self.gen_expression(condition);
                self.emit(TACInstruction::IfFalseGoto {
                    condition: cond,
                    label: next_label.clone(),
                });
                for s in then_block {
                    self.gen_statement(s);
                }
                self.emit(TACInstruction::Goto {
                    label: end_label.clone(),
                });
 
                // elif chains
                for (elif_cond, elif_body) in elif_blocks {
                    self.emit(TACInstruction::Label {
                        name: next_label.clone(),
                    });
                    next_label = self.new_label();
                    let c = self.gen_expression(elif_cond);
                    self.emit(TACInstruction::IfFalseGoto {
                        condition: c,
                        label: next_label.clone(),
                    });
                    for s in elif_body {
                        self.gen_statement(s);
                    }
                    self.emit(TACInstruction::Goto {
                        label: end_label.clone(),
                    });
                }
 
                // else
                self.emit(TACInstruction::Label { name: next_label });
                if let Some(else_stmts) = else_block {
                    for s in else_stmts {
                        self.gen_statement(s);
                    }
                }
 
                self.emit(TACInstruction::Label { name: end_label });
            }
 
            StatementKind::Switch { value, cases } => {
                let switch_val = self.gen_expression(value);
                let end_label = self.new_label();
 
                let mut case_labels: Vec<String> = Vec::new();
                for _ in cases {
                    case_labels.push(self.new_label());
                }
                let default_label = end_label.clone();
 
                // emit comparisons
                for (i, case) in cases.iter().enumerate() {
                    let case_val = self.gen_expression(&case.value);
                    let t = self.new_temp();
                    self.emit(TACInstruction::BinaryOp {
                        result: t.clone(),
                        left: switch_val.clone(),
                        op: "==".to_string(),
                        right: case_val,
                    });
                    self.emit(TACInstruction::IfGoto {
                        condition: t,
                        label: case_labels[i].clone(),
                    });
                }
                self.emit(TACInstruction::Goto {
                    label: default_label,
                });
 
                // emit case bodies
                for (i, case) in cases.iter().enumerate() {
                    self.emit(TACInstruction::Label {
                        name: case_labels[i].clone(),
                    });
                    for s in &case.body {
                        self.gen_statement(s);
                    }
                    self.emit(TACInstruction::Goto {
                        label: end_label.clone(),
                    });
                }
 
                self.emit(TACInstruction::Label { name: end_label });
            }
 
            StatementKind::For {
                init,
                condition,
                update,
                body,
            } => {
                let start_label = self.new_label();
                let update_label = self.new_label();
                let end_label = self.new_label();
 
                // init
                self.gen_statement(init);
 
                // condition check
                self.emit(TACInstruction::Label {
                    name: start_label.clone(),
                });
                let cond = self.gen_expression(condition);
                self.emit(TACInstruction::IfFalseGoto {
                    condition: cond,
                    label: end_label.clone(),
                });
 
                // push loop context for break/continue
                self.loop_stack
                    .push((update_label.clone(), end_label.clone()));
 
                // body
                for s in body {
                    self.gen_statement(s);
                }
 
                // update
                self.emit(TACInstruction::Label {
                    name: update_label.clone(),
                });
                self.gen_expression(update);
                self.emit(TACInstruction::Goto { label: start_label });
 
                self.emit(TACInstruction::Label { name: end_label });
 
                self.loop_stack.pop();
            }
 
            // ── break / continue ─────────────────────────────────────────
            StatementKind::Break => {
                if let Some((_, end_label)) = self.loop_stack.last() {
                    self.emit(TACInstruction::Goto {
                        label: end_label.clone(),
                    });
                }
            }
 
            StatementKind::Continue => {
                if let Some((update_label, _)) = self.loop_stack.last() {
                    self.emit(TACInstruction::Goto {
                        label: update_label.clone(),
                    });
                }
            }
 
            // ── function calls ───────────────────────────────────────────
            StatementKind::FunctionCall { name, args } => {
                let arg_temps: Vec<String> =
                    args.iter().map(|a| self.gen_expression(a)).collect();
                for a in &arg_temps {
                    self.emit(TACInstruction::Param {
                        value: a.clone(),
                    });
                }
                self.emit(TACInstruction::Call {
                    result: None,
                    func: name.clone(),
                    num_args: arg_temps.len().to_string(),
                });
            }
 
            // ── I/O ──────────────────────────────────────────────────────
            StatementKind::Print { value } => {
                let val = self.gen_expression(value);
                self.emit(TACInstruction::Print { value: val });
            }
 
            StatementKind::Input { var_name } => {
                self.emit(TACInstruction::Input {
                    var: var_name.clone(),
                });
            }
 
            // ── return ───────────────────────────────────────────────────
            StatementKind::Return { value } => {
                let val = value.as_ref().map(|e| self.gen_expression(e));
                self.emit(TACInstruction::Return { value: val });
            }
 
            // ── expression statement ─────────────────────────────────────
            StatementKind::Expression { expr } => {
                self.gen_expression(expr);
            }
        }
    }
 
    // ── expressions (returns the temp / name holding the result) ────────
 
    fn gen_expression(&mut self, expr: &Expression) -> String {
        match &expr.kind {
            // ── literals ─────────────────────────────────────────────────
            ExpressionKind::IntegerLiteral(n) => n.to_string(),
            ExpressionKind::FloatLiteral(f) => f.to_string(),
            ExpressionKind::BoolLiteral(b) => {
                if *b {
                    "1".to_string()
                } else {
                    "0".to_string()
                }
            }
            ExpressionKind::CharLiteral(c) => format!("'{}'", c),
            ExpressionKind::StringLiteral(s) => format!("\"{}\"", s),
 
            // ── identifier ───────────────────────────────────────────────
            ExpressionKind::Identifier(id) => id.clone(),
 
            // ── binary ───────────────────────────────────────────────────
            ExpressionKind::BinaryOp { op, left, right } => {
                let l = self.gen_expression(left);
                let r = self.gen_expression(right);
                let t = self.new_temp();
                let op_str = match op {
                    BinaryOperator::Add => "+",
                    BinaryOperator::Subtract => "-",
                    BinaryOperator::Multiply => "*",
                    BinaryOperator::Divide => "/",
                    BinaryOperator::Modulo => "%",
                    BinaryOperator::Expo => "**",
                    BinaryOperator::Equal => "==",
                    BinaryOperator::NotEqual => "!=",
                    BinaryOperator::GreaterThan => ">",
                    BinaryOperator::LessThan => "<",
                    BinaryOperator::And => "&&",
                    BinaryOperator::Or => "||",
                    BinaryOperator::Join => "join",
                };
                self.emit(TACInstruction::BinaryOp {
                    result: t.clone(),
                    left: l,
                    op: op_str.to_string(),
                    right: r,
                });
                t
            }
 
            // ── unary ────────────────────────────────────────────────────
            ExpressionKind::UnaryOp { op, operand } => {
                let o = self.gen_expression(operand);
                let t = self.new_temp();
                let op_str = match op {
                    UnaryOperator::Not => "!",
                    UnaryOperator::Negate => "-",
                };
                self.emit(TACInstruction::UnaryOp {
                    result: t.clone(),
                    op: op_str.to_string(),
                    operand: o,
                });
                t
            }
 
            // ── postfix (i++, i--) ───────────────────────────────────────
            ExpressionKind::PostfixOp { op, operand } => {
                let o = self.gen_expression(operand);
                let t = self.new_temp();
                // save original value
                self.emit(TACInstruction::Copy {
                    result: t.clone(),
                    value: o.clone(),
                });
                let op_str = match op {
                    PostfixOperator::Increment => "+",
                    PostfixOperator::Decrement => "-",
                };
                // mutate the variable
                self.emit(TACInstruction::BinaryOp {
                    result: o.clone(),
                    left: o,
                    op: op_str.to_string(),
                    right: "1".to_string(),
                });
                t
            }
 
            // ── function call ────────────────────────────────────────────
            ExpressionKind::FunctionCall { name, args } => {
                let arg_temps: Vec<String> =
                    args.iter().map(|a| self.gen_expression(a)).collect();
                for a in &arg_temps {
                    self.emit(TACInstruction::Param {
                        value: a.clone(),
                    });
                }
                let t = self.new_temp();
                self.emit(TACInstruction::Call {
                    result: Some(t.clone()),
                    func: name.clone(),
                    num_args: arg_temps.len().to_string(),
                });
                t
            }
 
        }
    }
}
 
// ── Dump all instructions to a .tac string ──────────────────────────────────
 
pub fn instructions_to_string(instructions: &[TACInstruction]) -> String {
    instructions
        .iter()
        .map(|i| i.to_string())
        .collect::<Vec<_>>()
        .join("\n")
}
 
