use crate::parser::parser::{
    Program, 
    Statement, 
    StatementKind, 
    Type, 
    ExpressionKind, 
    PostfixOperator,
    UnaryOperator,
    BinaryOperator,
    Expression

};


#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,          
    pub symbol_type: SymbolType, 
    pub data_type: String,       
    pub value: Option<String>,   
    pub scope: String,           
    pub line: usize,            
    pub col: usize,            
}

#[derive(Debug, Clone)]
pub enum SymbolType {
    Variable,
    Constant,
    Function,
    Parameter,
}

pub fn extract_symbols(program: &Program) -> Vec<Symbol>{
    let mut symbols = Vec::new();

    for function in &program.functions {
        let param_types: Vec<String> = function.params
            .iter()
            .map(|p| type_to_string(&p.param_type))
            .collect();
        symbols.push(Symbol {
            name: function.name.clone(),
            symbol_type: SymbolType::Function,
            data_type: format!(
                "-> {}",
                type_to_string(&function.return_type)
                ),
            value: None,
            scope: "global".to_string(),
            line: function.span.start.line,
            col: function.span.start.col
        });

        for param in &function.params {
            symbols.push(Symbol {
                name: param.name.clone(),
                symbol_type: SymbolType::Parameter,
                data_type: type_to_string(&param.param_type),
                value: None,
                scope: function.name.clone(),
                line: param.span.start.line,
                col: param.span.start.col
            });
        }

        extract_symbols_from_statements(&function.body, &function.name, &mut symbols);
    }
    symbols
}


fn extract_symbols_from_statements(statements: &[Statement], scope: &str, symbols: &mut Vec<Symbol>) {
    for stmt in statements {
        match &stmt.kind {
            StatementKind::VariableDeclaration { name, var_type, value } => {
                symbols.push(Symbol {
                    name: name.clone(),
                    symbol_type: SymbolType::Variable,
                    data_type: type_to_string(var_type),
                    value: Some(expression_to_string(value)),
                    scope: scope.to_string(),
                    line: stmt.span.start.line,
                    col: stmt.span.start.col,
                });
            }
            
            StatementKind::ConstDeclaration { name, var_type, value } => {
                symbols.push(Symbol {
                    name: name.clone(),
                    symbol_type: SymbolType::Constant,
                    data_type: type_to_string(var_type),  
                    value: Some(expression_to_string(value)),  
                    scope: scope.to_string(),  
                    line: stmt.span.start.line,  
                    col: stmt.span.start.col,  
                });
            }
            
            StatementKind::If { then_block, elif_blocks, else_block, .. } => {
                extract_symbols_from_statements(then_block, scope, symbols);
                for (_, block) in elif_blocks {
                    extract_symbols_from_statements(block, scope, symbols);
                }
                if let Some(else_stmts) = else_block {
                    extract_symbols_from_statements(else_stmts, scope, symbols);
                }
            }
            
            StatementKind::Switch { cases, .. } => {
                for case in cases {
                    extract_symbols_from_statements(&case.body, scope, symbols);
                }
            }

            StatementKind::For { init, body, .. } => {
                // Extraer la variable del init del for
                if let StatementKind::VariableDeclaration { name, var_type, value } = &init.kind {
                    symbols.push(Symbol {
                        name: name.clone(),
                        symbol_type: SymbolType::Variable,
                        data_type: type_to_string(var_type),
                        value: Some(expression_to_string(value)),
                        scope: scope.to_string(),
                        line: init.span.start.line,
                        col: init.span.start.col,
                    });
                }
 
                extract_symbols_from_statements(body, scope, symbols);
            }
            
            _ => {}
        }
    }
}


fn type_to_string(t: &Type) -> String {
    match t {
        Type::Ont => "ont".to_string(),
        Type::Uont => "uont".to_string(),
        Type::Michi => "michi".to_string(),
        Type::Ntr => "ntr".to_string(),
        Type::Chip => "chip".to_string(),
        Type::Yesorno => "yesorno".to_string(),
    }
}

fn expression_to_string(expr: &Expression) -> String {
    
    match &expr.kind {
        ExpressionKind::IntegerLiteral(n) => n.to_string(),
        ExpressionKind::FloatLiteral(f) => f.to_string(),
        ExpressionKind::CharLiteral(c) => format!("'{}'", c),
        ExpressionKind::StringLiteral(s) => format!("\"{}\"", s),
        ExpressionKind::BoolLiteral(b) => if *b { "yes" } else { "no" }.to_string(),
        ExpressionKind::Identifier(id) => id.clone(),
        
        ExpressionKind::BinaryOp { op, left, right } => {
            let op_str = match op {
                BinaryOperator::Add => "plus",
                BinaryOperator::Subtract => "minus",
                BinaryOperator::Multiply => "mult",
                BinaryOperator::Divide => "by",
                BinaryOperator::Modulo => "mod",
                BinaryOperator::Equal => "eq",
                BinaryOperator::NotEqual => "noteq",
                BinaryOperator::GreaterThan => "great",
                BinaryOperator::LessThan => "lesst",
                BinaryOperator::And => "and",
                BinaryOperator::Or => "or",
                BinaryOperator::Join => "join",
            };
            format!("{} {} {}", expression_to_string(left), op_str, expression_to_string(right))
        }
        
        ExpressionKind::UnaryOp { op, operand } => {
            let op_str = match op {
                UnaryOperator::Not => "nah",
                UnaryOperator::Negate => "minus",
            };
            format!("{} {}", op_str, expression_to_string(operand))
        }
        
        ExpressionKind::FunctionCall { name, args } => {
            let args_str = args.iter()
                .map(expression_to_string)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", name, args_str)
        }
        
        ExpressionKind::ArrayAccess { array, index } => {
            format!("{}[{}]", array, expression_to_string(index))
        }
        
        ExpressionKind::PostfixOp { op, operand } => {
            let op_str = match op {
                PostfixOperator::Increment => "plusplus",
                PostfixOperator::Decrement => "minusminus",
            };
            format!("{} {}", expression_to_string(operand), op_str)
        }
    }
}

