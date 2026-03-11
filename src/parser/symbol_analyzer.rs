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


// Find symbol, in scope first and then global
fn find_symbol<'a>(symbols: &'a [Symbol], name: &str, scope: &str) -> Option<&'a Symbol> {
    symbols.iter()
        .find(|s| s.name == name && s.scope == scope)
        .or_else(|| symbols.iter().find(|s| s.name == name && s.scope == "global"))
}

// check argument count an d types
fn check_function_args(
    func_name: &str,
    args: &[Expression],
    symbols: &[Symbol],
    scope: &str,
    span: &SourceSpan,
) -> Result<(), SemanticError> {
    let params: Vec<&Symbol> = symbols.iter()
        .filter(|s| s.scope == func_name && matches!(s.symbol_type, SymbolType::Parameter))
        .collect();

    if args.len() != params.len() {
        return Err(SemanticError {
            message: format!(
                "'{}' expects {} arguments but {} were given",
                func_name, params.len(), args.len()
            ),
            span: Some(span.clone()),
        });
    }

    for (arg, param) in args.iter().zip(params.iter()) {
        if let Some(inferred) = infer_type(arg, symbols, scope) {
            if !types_compatible(&param.data_type, &inferred) {
                return Err(SemanticError {
                    message: format!(
                        "argument'{}' of '{}' is of type '{}' but a value of type '{}' was given",
                        param.name, func_name, param.data_type, inferred
                    ),
                    span: Some(arg.span.clone()),
                });
            }
        }
    }

    Ok(())
}

const BUILTIN_FUNCTIONS: &[&str] = &["tupni", "tnirp"];

const RESERVED_KEYWORDS: &[&str] = &[
    // Keywords
    "utl", "off", "onoff", "on", "wii", "mote", "dec", "kf",
    "next", "ash", "brokie", "send", "tnirp", "tupni",
    // Types
    "ont", "uont", "michi", "ntr", "chip", "yesorno",
    // Literals
    "yes", "no",
    // Operators
    "is", "plus", "plusplus", "mult", "minus", "minusminus",
    "by", "mod", "join",
    // Logical
    "and", "or", "nah", "great", "lesst", "eq", "noteq",
];

fn check_reserved_keyword(name: &str, span: &SourceSpan) -> Result<(), SemanticError> {
    if RESERVED_KEYWORDS.contains(&name) {
        return Err(SemanticError {
            message: format!("'{}' is a reserved keyword and cannot be used as a identifier", name),
            span: Some(span.clone()),
        });
    }
    Ok(())
}

fn check_expression(expr: &Expression, symbols: &[Symbol], scope: &str) -> Result<(), SemanticError> {
    match &expr.kind {
        ExpressionKind::Identifier(id) => {
            if find_symbol(symbols, id, scope).is_none() {
                return Err(SemanticError { 
                    message: format!( "'{}' is not declared in the scope '{}'", id, scope),
                    span: Some(expr.span.clone())
                });
            }
        }
        ExpressionKind::FunctionCall{ name, args } => {
            if !BUILTIN_FUNCTIONS.contains(&name.as_str()) {
                match find_symbol(symbols, name, scope) {
                    None => {
                        return Err(SemanticError { 
                            message: format!( "funcion '{}' is not declared", name),
                            span: Some(expr.span.clone())
                        });
                    }
                    Some(sym) if !matches!(sym.symbol_type, SymbolType::Function) => {
                        return Err(SemanticError { 
                            message: format!( "'{}' is not a function", name),
                            span: Some(expr.span.clone())
                        });
                    }
                    _ => {}
                }
                check_function_args(name, args, symbols, scope, &expr.span)?;
            }
            for arg in args {
                check_expression(arg, symbols, scope)?;
            }
        }
        ExpressionKind::BinaryOp { op, left, right } => {
            check_expression(left, symbols, scope)?;
            check_expression(right, symbols, scope)?;

            let left_type = infer_type(left, symbols, scope);
            let right_type = infer_type(right, symbols, scope);

            if let (Some(lt), Some(rt)) = (&left_type, &right_type) {
                match op {
                    BinaryOperator::Add | BinaryOperator::Subtract |
                    BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Modulo => {
                        if !is_numeric(lt) || !is_numeric(rt) {
                            return Err(SemanticError {
                                message: format!( "Arithmetic operation between incompatible types '{}' and '{}'", lt, rt),
                                span: Some(expr.span.clone()),
                            });
                        }
                    }
                    BinaryOperator::GreaterThan | BinaryOperator::LessThan => {
                        if !is_numeric(lt) || !is_numeric(rt) {
                            return Err(SemanticError {
                                message: format!( "Operation between incompatible types '{}' and '{}'", lt, rt),
                                span: Some(expr.span.clone()),
                            });
                        }
                    }
                    BinaryOperator::Equal | BinaryOperator::NotEqual => {
                        if !types_compatible(lt, rt) && !types_compatible(rt, lt) {
                            return Err(SemanticError {
                                message: format!( "Operation between incompatible types '{}' and '{}'", lt, rt),
                                span: Some(expr.span.clone()),
                            });
                        }
                    }
                    BinaryOperator::And | BinaryOperator::Or => {
                        if lt != "yesorno" || rt != "yesorno" {
                            return Err(SemanticError {
                                message: format!("Logical peration between incompatible types '{}' and '{}'", lt, rt),
                                span: Some(expr.span.clone()),
                            });
                        }
                    }
                    BinaryOperator::Join => {
                        if lt != "ntr" && rt != "ntr" {
                            return Err(SemanticError {
                                message: format!("Join operation requires at least an 'ntr', '{}' and '{}' was found", lt, rt),
                                span: Some(expr.span.clone()),
                            });
                        }
                    }
                }
            }
        }
        ExpressionKind::UnaryOp { op, operand } => {
            check_expression(operand, symbols, scope)?;

            if let Some(t) = infer_type(operand, symbols, scope) {
                match op {
                    UnaryOperator::Negate => {
                        if !is_numeric(&t) {
                            return Err(SemanticError {
                                message: format!("Cannot negate a value of type '{}'", t),
                                span: Some(expr.span.clone()),
                            });
                        }
                    }
                    UnaryOperator::Not => {
                        if t != "yesorno" {
                            return Err(SemanticError {
                                message: format!("'nah' requires a value of type 'yesorno', '{}' was found", t),
                                span: Some(expr.span.clone()),
                            });
                        }
                    }
                }
            }
        }
        ExpressionKind::PostfixOp { operand, .. } => {
            check_expression(operand, symbols, scope)?;

            if let Some(t) = infer_type(operand, symbols, scope) {
                if !is_numeric(&t) {
                    return Err(SemanticError {
                        message: format!("plusplus/minusminus required a numeric value, '{}' was found", t),
                        span: Some(expr.span.clone()),
                    });
                }
            }
        }

        _ => {}
    }
    Ok(())
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
                ExpressionKind::PostfixOp { operand, .. } => {
            infer_type(operand, symbols, scope)
        }
        ExpressionKind::BinaryOp { op, left, right } => {
            let left_type = infer_type(left, symbols, scope)?;
            let right_type = infer_type(right, symbols, scope)?;

            match op {
                // result is michi if either side is michi, otherwise ont
                BinaryOperator::Add | BinaryOperator::Subtract | 
                BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Modulo => {
                    if left_type == "michi" || right_type == "michi" {
                        Some("michi".to_string())
                    } else {
                        Some("ont".to_string())
                    }
                }
                // result is yesorno
                BinaryOperator::GreaterThan | BinaryOperator::LessThan |
                BinaryOperator::Equal | BinaryOperator::NotEqual => {
                    Some("yesorno".to_string())
                }
                // result is yesorno
                BinaryOperator::And | BinaryOperator::Or => {
                    Some("yesorno".to_string())
                }
                // result is ntr
                BinaryOperator::Join => {
                    Some("ntr".to_string())
                }
            }
        }
        ExpressionKind::FunctionCall{ name, .. } => {
            symbols.iter()
                .find(|s| s.name == *name && matches!(s.symbol_type, SymbolType::Function))
                .and_then(|s| s.data_type.strip_prefix("-> ").map(|t| t.to_string()))
        },
        _ => None,
    }
}

fn types_compatible(declared: &str, inferred: &str) -> bool {
    declared == inferred
        || (declared == "ont" && inferred == "uont")
        || (declared == "uont" && inferred == "ont")
        || (declared == "michi" && (inferred == "ont" || inferred == "uont"))
}

fn is_numeric(t: &str) -> bool {
    matches!(t, "ont" | "uont" | "michi")
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
        if !types_compatible(&declared_str, &inferred) {
            return Err(SemanticError { 
                message: format!( "'{}' is of type '{}' but type '{}' was assigned", var_name, declared_str, inferred),
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
            message: format!("'{}' is already declared on the scope '{}' (line {}, col {})", name, scope, existing.line, existing.col),
            span: Some(span.clone())}
        )
    }
    Ok(())
}



pub fn extract_symbols(program: &Program) -> Result<Vec<Symbol>, SemanticError>{
    let mut symbols = Vec::new();

    for function in &program.functions {
        check_reserved_keyword(&function.name, &function.span)?;
        // Check duplicate function
        check_duplicate(&symbols, &function.name, "global", &function.span)?;

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
            check_reserved_keyword(&param.name, &param.span)?;
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
                    check_reserved_keyword(name, &stmt.span)?;
                    check_duplicate(symbols, name, scope, &stmt.span)?;
                    check_expression(value, symbols, scope)?;
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
                    check_reserved_keyword(name, &stmt.span)?;
                    check_duplicate(symbols, name, scope, &stmt.span)?;
                    check_expression(value, symbols, scope)?;
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
                StatementKind::Assignment { name, value } => {
                    // check variable declared
                    let sym = find_symbol(symbols, name, scope)
                        .ok_or_else(|| SemanticError {
                            message: format!( "'{}' is not declared in the scope '{}'", name, scope),
                            span: Some(stmt.span.clone())
                        })?;
                    // check constant assignment
                    if matches!(sym.symbol_type, SymbolType::Constant) {
                        return Err(SemanticError { 
                            message: format!( "'{}' is a constant", name),
                            span: Some(stmt.span.clone())
                        });
                    }
                    // check function assignment
                    if matches!(sym.symbol_type, SymbolType::Function) {
                        return Err(SemanticError { 
                            message: format!( "'{}' is a function", name),
                            span: Some(stmt.span.clone())
                        });
                    }

                    check_expression(value, symbols, scope)?;

                    // check type of assignment
                    let declared_type = sym.data_type.clone();
                    if let Some(inferred) = infer_type(value, symbols, scope) {
                        if !types_compatible(&declared_type, &inferred) {
                            return Err(SemanticError { 
                                message: format!( "'{}' is of type '{}' but type '{}' was assigned", name, declared_type, inferred),
                                span: Some(stmt.span.clone())
                            });
                        }
                    }

                }
                StatementKind::FunctionCall { name, args } => {
                    if !BUILTIN_FUNCTIONS.contains(&name.as_str()) {
                        //check declared
                        let sym = find_symbol(symbols, name, scope)
                            .ok_or_else(|| SemanticError {
                                message: format!( "function '{}' is not declared", name),
                                span: Some(stmt.span.clone())
                            })?;

                        // check is function
                        if !matches!(sym.symbol_type, SymbolType::Function) {
                            return Err(SemanticError { 
                                message: format!( "'{}' is not a function", name),
                                span: Some(stmt.span.clone())
                            });
                        }

                        check_function_args(name, args, symbols, scope, &stmt.span)?;

                    }

                    for arg in args {
                        check_expression(arg, symbols, scope)?;
                    }
                }

                StatementKind::Input { var_name } => {
                    let sym = find_symbol(symbols, var_name, scope)
                        .ok_or_else(|| SemanticError {
                            message: format!("'{}' was not declared in the scope '{}'", var_name, scope),
                            span: Some(stmt.span.clone()),
                        })?;

                    if matches!(sym.symbol_type, SymbolType::Constant) {
                        return Err(SemanticError {
                            message: format!("'{}' is a constant and cannot be used with tupni", var_name),
                            span: Some(stmt.span.clone()),
                        });
                    }
                }

                StatementKind::Print { value } => {
                    check_expression(value, symbols, scope)?;
                }

                StatementKind::Return { value } => {
                    if let Some(expr) = value {
                        check_expression(expr, symbols, scope)?;

                        // chekc the type from the table
                        if let Some(func_sym) = symbols.iter().find(|s| s.name == scope && matches!(s.symbol_type, SymbolType::Function)) {
                            if let Some(ret_str) = func_sym.data_type.strip_prefix("-> ") {
                                if let Some(inferred) = infer_type(expr, symbols, scope) {
                                    if !types_compatible(ret_str, &inferred) {
                                        return Err(SemanticError {
                                            message: format!(
                                                "the function '{}' returns '{}' but value of type '{}' was found",
                                                scope, ret_str, inferred
                                            ),
                                            span: Some(stmt.span.clone()),
                                        });
                                    }
                                }
                            }
                        }
                    }
                }

                StatementKind::Expression { expr } => { 
                    check_expression(expr, symbols, scope)?;
                }
                
                StatementKind::If { condition, then_block, elif_blocks, else_block } => {
                    check_expression(condition, symbols, scope)?;
                    extract_symbols_from_statements(then_block, scope, symbols)?;
                    for (elif_cond, block) in elif_blocks {
                        check_expression(elif_cond, symbols, scope)?;
                        extract_symbols_from_statements(block, scope, symbols)?;
                    }
                    if let Some(else_stmts) = else_block {
                        extract_symbols_from_statements(else_stmts, scope, symbols)?;
                    }
                }
                
                StatementKind::Switch { value, cases } => {
                    check_expression(value, symbols, scope)?;
                    for case in cases {
                        check_expression(&case.value, symbols, scope)?;
                        extract_symbols_from_statements(&case.body, scope, symbols)?;
                    }
                }

                StatementKind::For { init, condition, update, body } => {
                    if let StatementKind::VariableDeclaration { name, var_type, value } = &init.kind {
                        check_reserved_keyword(name, &init.span)?;
                        check_duplicate(symbols, name, scope, &init.span)?;
                        check_expression(value, symbols, scope)?;
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

                    check_expression(condition, symbols, scope)?;
                    check_expression(update, symbols, scope)?;
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

