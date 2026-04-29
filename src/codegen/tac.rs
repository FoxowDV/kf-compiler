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
                write!(f, "    if ({}) goto {}", condition, label)
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

    fn emit_cond_jump(&mut self, cond: &str, true_label: &str, false_label: &str) {
        self.emit(TACInstruction::IfGoto { 
            condition: cond.to_string(), 
            label: true_label.to_string() 
        });
        self.emit(TACInstruction::Goto { 
            label: false_label.to_string() 
        });
    }
 
 
    pub fn generate(program: &Program) -> Vec<TACInstruction> {
        let mut generator = TACGenerator::new();
        for func in &program.functions {
            generator.gen_function(func);
        }
        generator.instructions
    }
 
 
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
 
 
    fn gen_statement(&mut self, stmt: &Statement) {
        match &stmt.kind {
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
 
            StatementKind::If {
                condition,
                then_block,
                elif_blocks,
                else_block,
            } => {
                let end_label = self.new_label();
 
                let true_label = self.new_label();
                let mut next_label = self.new_label();
                let cond = self.gen_expression(condition);
                self.emit_cond_jump(&cond, &true_label, &next_label);

                self.emit(TACInstruction::Label { name: true_label });
                for s in then_block {
                    self.gen_statement(s);
                }
                self.emit(TACInstruction::Goto {
                    label: end_label.clone(),
                });
 
                for (elif_cond, elif_body) in elif_blocks {
                    self.emit(TACInstruction::Label {
                        name: next_label.clone(),
                    });
                    
                    let elif_true = self.new_label();
                    next_label = self.new_label();
                    let c = self.gen_expression(elif_cond);
                    self.emit_cond_jump(&c, &elif_true, &next_label);

                    self.emit(TACInstruction::Label { name: elif_true });

                    for s in elif_body {
                        self.gen_statement(s);
                    }
                    self.emit(TACInstruction::Goto {
                        label: end_label.clone(),
                    });
                }
 
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
                let body_label = self.new_label();
                let update_label = self.new_label();
                let end_label = self.new_label();
 
                self.gen_statement(init);
 
                self.emit(TACInstruction::Label {
                    name: start_label.clone(),
                });

                let cond = self.gen_expression(condition);
                self.emit_cond_jump(&cond, &body_label, &end_label);

                self.emit(TACInstruction::Label { name: body_label });
 
                self.loop_stack
                    .push((update_label.clone(), end_label.clone()));
 
                for s in body {
                    self.gen_statement(s);
                }
 
                self.emit(TACInstruction::Label {
                    name: update_label.clone(),
                });
                self.gen_expression(update);
                self.emit(TACInstruction::Goto { label: start_label });
 
                self.emit(TACInstruction::Label { name: end_label });
 
                self.loop_stack.pop();
            }

            StatementKind::While {
                condition,
                body,
            } => {
                let start_label = self.new_label();
                let body_label = self.new_label();
                let end_label = self.new_label();
 
                self.emit(TACInstruction::Label {
                    name: start_label.clone(),
                });

                let cond = self.gen_expression(condition);
                self.emit_cond_jump(&cond, &body_label, &end_label);

                self.emit(TACInstruction::Label { name: body_label });
 
                self.loop_stack
                    .push((start_label.clone(), end_label.clone()));
 
                for s in body {
                    self.gen_statement(s);
                }
 
                self.emit(TACInstruction::Goto { label: start_label });
                self.emit(TACInstruction::Label { name: end_label });
 
                self.loop_stack.pop();
            }
 
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
 
            StatementKind::Print { value } => {
                let val = self.gen_expression(value);
                self.emit(TACInstruction::Print { value: val });
            }
 
            StatementKind::Input { var_name } => {
                self.emit(TACInstruction::Input {
                    var: var_name.clone(),
                });
            }
 
            StatementKind::Return { value } => {
                let val = value.as_ref().map(|e| self.gen_expression(e));
                self.emit(TACInstruction::Return { value: val });
            }
 
            StatementKind::Expression { expr } => {
                self.gen_expression(expr);
            }
        }
    }
 
 
    fn gen_expression(&mut self, expr: &Expression) -> String {
        match &expr.kind {
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
 
            ExpressionKind::Identifier(id) => id.clone(),
 
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
 
            ExpressionKind::PostfixOp { op, operand } => {
                let o = self.gen_expression(operand);
                let t = self.new_temp();
                self.emit(TACInstruction::Copy {
                    result: t.clone(),
                    value: o.clone(),
                });
                let op_str = match op {
                    PostfixOperator::Increment => "+",
                    PostfixOperator::Decrement => "-",
                };
                self.emit(TACInstruction::BinaryOp {
                    result: o.clone(),
                    left: o,
                    op: op_str.to_string(),
                    right: "1".to_string(),
                });
                t
            }
 
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
 
 
pub fn instructions_to_string(instructions: &[TACInstruction]) -> String {
    instructions
        .iter()
        .map(|i| i.to_string())
        .collect::<Vec<_>>()
        .join("\n")
}
 
