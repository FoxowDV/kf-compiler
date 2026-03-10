use crate::parser::parser::{
    BinaryOperator, 
    Expression, 
    ExpressionKind, 
    PostfixOperator, 
    Program, 
    SourceSpan, 
    Statement, 
    StatementKind, 
    Type, 
    UnaryOperator
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

#[derive(Debug, Clone)]
pub struct SemanticError {
    pub message: String,
    pub span: Option<SourceSpan>,
}


// Infer type
fn infer_type(expr: &Expression, symbols: &[Symbol], scope: &str) -> Option<String> {
    match &expr.kind {
        ExpressionKind::IntegerLiteral(_) => Some("ont".to_string()),
        ExpressionKind::FloatLiteral(_) => Some("michi".to_string()),
        ExpressionKind::CharLiteral(_) => Some("chip".to_string()),
        ExpressionKind::StringLiteral(_) => Some("ntr".to_string()),
        ExpressionKind::BoolLiteral(_) => Some("yesorno".to_string()),
        ExpressionKind::Identifier(id) => {
            symbols.iter()
                .find(|s| s.name == *id && (s.scope == scope || s.scope == "global"))
                .map(|s| s.data_type.clone())
        },
        ExpressionKind::UnaryOp{ op, operand } => {
            match op {
                UnaryOperator::Negate => infer_type(operand, symbols, scope),
                UnaryOperator::Not => Some("yesorno".to_string()),
            }
        },
        ExpressionKind::FunctionCall{ name, .. } => {
            symbols.iter()
                .find(|s| s.name == *name && matches!(s.symbol_type, SymbolType::Function))
                .and_then(|s| s.data_type.strip_prefix("-> ").map(|t| t.to_string()))
        },
        _ => None,
    }
}

fn check_type_mismatch(
    var_name: &str,
    declared: &Type,
    value: &Expression,
    symbols: &[Symbol],
    scope: &str,
    span: &SourceSpan,
) -> Result<(), SemanticError> {
    let declared_str = type_to_string(declared);
    if let Some(inferred) = infer_type(value, symbols, scope) {
        let compatible = declared_str == inferred
            || (declared_str == "ont" && inferred == "uont")
            || (declared_str == "uont" && inferred == "ont")
            || (declared_str == "michi" && (inferred == "ont" || inferred == "uont"));

        if !compatible {
            return Err(SemanticError { 
                message: format!(
                     "'{}' es de tipo '{}' pero se le asigna valor de tipo '{}'",
                     var_name, declared_str, inferred
                     ),
                span: Some(span.clone())
            });
        }
    }

    Ok(())
}

// Check dup
fn check_duplicate(symbols: &[Symbol], name: &str, scope: &str, span: &SourceSpan) -> Result<(), SemanticError> {
    if let Some(existing) = symbols.iter().find(|s| s.name == name && s.scope == scope) {
        return Err(SemanticError { 
            message: format!(
                         "'{}' ya fue declarado en el scope '{}' (linea {}, columna {})",
                         name, scope, existing.line, existing.col
                     ),
            span: Some(span.clone())}
        )
    }
    Ok(())
}

pub fn extract_symbols(program: &Program) -> Result<Vec<Symbol>, SemanticError>{
    let mut symbols = Vec::new();

    for function in &program.functions {
        // Check duplicate function
        check_duplicate(&symbols, &function.name, "global", &function.span)?;

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
            // Check duplicate param name 
            check_duplicate(&symbols, &param.name, &function.name, &param.span)?;
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

        extract_symbols_from_statements(&function.body, &function.name, &mut symbols)?;
    }
    Ok(symbols)
}


fn extract_symbols_from_statements(
    statements: &[Statement], 
    scope: &str, 
    symbols: &mut Vec<Symbol>
    ) -> Result<(), SemanticError> {
        for stmt in statements {
            match &stmt.kind {
                StatementKind::VariableDeclaration { name, var_type, value } => {
                    check_duplicate(symbols, name, scope, &stmt.span)?;
                    check_type_mismatch(name, var_type, value, symbols, scope, &stmt.span)?;

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
                    check_duplicate(symbols, name, scope, &stmt.span)?;
                    check_type_mismatch(name, var_type, value, symbols, scope, &stmt.span)?;
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
                    extract_symbols_from_statements(then_block, scope, symbols)?;
                    for (_, block) in elif_blocks {
                        extract_symbols_from_statements(block, scope, symbols)?;
                    }
                    if let Some(else_stmts) = else_block {
                        extract_symbols_from_statements(else_stmts, scope, symbols)?;
                    }
                }
                
                StatementKind::Switch { cases, .. } => {
                    for case in cases {
                        extract_symbols_from_statements(&case.body, scope, symbols)?;
                    }
                }

                StatementKind::For { init, body, .. } => {
                    // Extraer la variable del init del for
                    if let StatementKind::VariableDeclaration { name, var_type, value } = &init.kind {
                        check_duplicate(symbols, name, scope, &init.span)?;
                        check_type_mismatch(name, var_type, value, symbols, scope, &init.span)?;
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
     
                    extract_symbols_from_statements(body, scope, symbols)?;
                }
                
                _ => {}
            }
        }
        Ok(())
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

