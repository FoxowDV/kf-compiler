use pest::Parser;
use pest::Span;
use pest_derive::Parser;

use crate::lexing::token::Position;
use crate::lexing::lexer::calculate_position;

#[derive(Parser)]
#[grammar = "kf.pest"]
pub struct KFParser;


// Info de los tokens
#[derive(Debug, Clone, PartialEq)]
pub struct SourceSpan {
    pub start: Position,
    pub end: Position,
    pub start_offset: usize,
    pub end_offset: usize,
}

impl SourceSpan {
    // Del pest al position source nuestro
    pub fn from_pest_span(span: Span, source: &str) -> Self {
        let start_pos = span.start_pos();
        let end_pos = span.end_pos();
        
        Self {
            start: calculate_position(source, start_pos.pos()),
            end: calculate_position(source, end_pos.pos()),
            start_offset: start_pos.pos(),
            end_offset: end_pos.pos(),
        }
    }
}


// Declaraciones de grammar

// Base 
#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<FunctionDef>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub body: Vec<Statement>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Ont,  
    Uont,  
    Michi,  
    Ntr,     
    Chip,     
    Yesorno,  
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    ConstDeclaration { name: String, var_type: Type, value: Expression },
    VariableDeclaration { name: String, var_type: Type, value: Expression },
    Assignment { name: String, value: Expression },
    If { 
        condition: Expression, 
        then_block: Vec<Statement>, 
        elif_blocks: Vec<(Expression, Vec<Statement>)>,
        else_block: Option<Vec<Statement>> 
    },
    Switch { 
        value: Expression, 
        cases: Vec<CaseClause> 
    },
    For { 
        init: Box<Statement>, 
        condition: Expression, 
        update: Expression, 
        body: Vec<Statement> 
    },
    FunctionCall { name: String, args: Vec<Expression> },
    Print { value: Expression },
    Input { var_name: String },
    Return { value: Option<Expression> },
    Break,
    Continue,
    Expression { expr: Expression },
}

#[derive(Debug, Clone)]
pub struct CaseClause {
    pub value: Expression,
    pub body: Vec<Statement>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    IntegerLiteral(i32),
    FloatLiteral(f64),
    CharLiteral(char),
    StringLiteral(String),
    BoolLiteral(bool),
    Identifier(String),
    FunctionCall { name: String, args: Vec<Expression> },
    BinaryOp { 
        op: BinaryOperator, 
        left: Box<Expression>, 
        right: Box<Expression> 
    },
    UnaryOp { 
        op: UnaryOperator, 
        operand: Box<Expression> 
    },
    PostfixOp { 
        op: PostfixOperator, 
        operand: Box<Expression> 
    },
    ArrayAccess { 
        array: String, 
        index: Box<Expression> 
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add, Subtract, Multiply, Divide, Modulo,
    Equal, NotEqual, GreaterThan, LessThan,
    And, Or, Join,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Not, Negate,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PostfixOperator {
    Increment, Decrement,
}

// Error
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Option<SourceSpan>,
}

pub fn parse_program(source: &str) -> Result<Program, ParseError> {
    let pairs = KFParser::parse(Rule::program, source)
        .map_err(|e| ParseError {
            message: format!("Parse error: {}", e),
            span: None,
        })?;

    let mut functions = Vec::new();
    let mut program_span = None;

    for pair in pairs {
        match pair.as_rule() {
            Rule::program => {
                if program_span.is_none() {
                    program_span = Some(SourceSpan::from_pest_span(pair.as_span(), source));
                }

                for inner_pair in pair.into_inner() {
                    if let Rule::function_def = inner_pair.as_rule() {
                        functions.push(parse_function_def(inner_pair, source)?);
                    }
                }
            }
            _ => {}
        }
    }

    Ok(Program {
        functions,
        span: program_span.unwrap_or(SourceSpan {
            start: Position {line: 1, col: 1},
            end: Position { line: 1, col: 1},
            start_offset: 0,
            end_offset: 0,
        }),
    })
}

fn parse_function_def(pair: pest::iterators::Pair<Rule>, source: &str) -> Result<FunctionDef, ParseError> {
    let span = SourceSpan::from_pest_span(pair.as_span(), source);
    let mut parts = pair.into_inner();

    expect_rule(&mut parts, Rule::utl)?;
    let name = parts.next()
        .ok_or_else(|| ParseError {message: "Expected identifier".to_string(), span: Some(span.clone()) })?
        .as_str()
        .to_string();

    expect_rule(&mut parts, Rule::left_paren)?;
    
    // parameters
    let mut params = Vec::new();
    let next = parts.next().unwrap();
    
    if let Rule::param_list = next.as_rule() {
        for param_pair in next.into_inner() {
            if let Rule::param = param_pair.as_rule() {
                params.push(parse_parameter(param_pair, source)?);
            }
        }
        expect_rule(&mut parts, Rule::right_paren)?;
        expect_rule(&mut parts, Rule::colon)?;
    } else if let Rule::right_paren = next.as_rule() {
        expect_rule(&mut parts, Rule::colon)?;
    } else {
    }
    
    let return_type = parse_type(parts.next().unwrap())?;
    
    //  body
    let body = parse_block(parts.next().unwrap(), source)?;
    
    Ok(FunctionDef {
        name,
        params,
        return_type,
        body,
        span,
    })
    
}

fn parse_parameter(pair: pest::iterators::Pair<Rule>, source: &str) -> Result<Parameter, ParseError> {
    let span = SourceSpan::from_pest_span(pair.as_span(), source);
    let mut parts = pair.into_inner();
    
    let name = parts.next().unwrap().as_str().to_string();
    expect_rule(&mut parts, Rule::colon)?;
    let param_type = parse_type(parts.next().unwrap())?;
    
    Ok(Parameter {
        name,
        param_type,
        span,
    })
}

fn parse_type(pair: pest::iterators::Pair<Rule>) -> Result<Type, ParseError> {
    let inner = pair.into_inner().next().unwrap();
    
    Ok(match inner.as_rule() {
        Rule::ont => Type::Ont,
        Rule::uont => Type::Uont,
        Rule::michi => Type::Michi,
        Rule::ntr => Type::Ntr,
        Rule::chip => Type::Chip,
        Rule::yesorno => Type::Yesorno,
        _ => return Err(ParseError {
            message: format!("Unknown type: {:?}", inner.as_rule()),
            span: None,
        }),
    })
}

fn parse_case_clause(pair: pest::iterators::Pair<Rule>, source: &str) -> Result<CaseClause, ParseError> {
    let span = SourceSpan::from_pest_span(pair.as_span(), source);
    let mut parts = pair.into_inner();
    
    expect_rule(&mut parts, Rule::mote)?;
    let value = parse_expression(parts.next().unwrap(), source)?;
    expect_rule(&mut parts, Rule::colon)?;
    
    let mut body = Vec::new();
    for stmt_pair in parts {
        body.push(parse_statement(stmt_pair, source)?);
    }
    
    Ok(CaseClause { value, body, span })
}

fn parse_statement(pair: pest::iterators::Pair<Rule>, source: &str) -> Result<Statement, ParseError> {
    let span = SourceSpan::from_pest_span(pair.as_span(), source);
    let inner = pair.into_inner().next().unwrap();
    
    let kind = match inner.as_rule() {
        Rule::const_declaration => {
            let mut parts = inner.into_inner();
            expect_rule(&mut parts, Rule::kf)?;
            let name = parts.next().unwrap().as_str().to_string();
            expect_rule(&mut parts, Rule::colon)?;
            let var_type = parse_type(parts.next().unwrap())?;
            expect_rule(&mut parts, Rule::is)?;
            let value = parse_expression(parts.next().unwrap(), source)?;
            StatementKind::ConstDeclaration { name, var_type, value }
        }
        
        Rule::variable_declaration => {
            let mut parts = inner.into_inner();
            expect_rule(&mut parts, Rule::dec)?;
            let name = parts.next().unwrap().as_str().to_string();
            expect_rule(&mut parts, Rule::colon)?;
            let var_type = parse_type(parts.next().unwrap())?;
            expect_rule(&mut parts, Rule::is)?;
            let value = parse_expression(parts.next().unwrap(), source)?;
            StatementKind::VariableDeclaration { name, var_type, value }
        }
        
        Rule::assignment => {
            let mut parts = inner.into_inner();
            let name = parts.next().unwrap().as_str().to_string();
            expect_rule(&mut parts, Rule::is)?;
            let value = parse_expression(parts.next().unwrap(), source)?;
            StatementKind::Assignment { name, value }
        }
        
        Rule::function_call_statement => {
            let func_call = inner.into_inner().next().unwrap();
            let (name, args) = parse_function_call_inner(func_call, source)?;
            StatementKind::FunctionCall { name, args }
        }
        
        Rule::if_statement => {
            let mut parts = inner.into_inner();
            expect_rule(&mut parts, Rule::off)?;
            expect_rule(&mut parts, Rule::left_paren)?;
            let condition = parse_expression(parts.next().unwrap(), source)?;
            expect_rule(&mut parts, Rule::right_paren)?;
            let then_block = parse_block(parts.next().unwrap(), source)?;

            let mut elif_blocks = Vec::new();
            let mut else_block = None;

            while let Some(current_part) = parts.next() {
                match current_part.as_rule() {
                    Rule::onoff_statement => {
                        let mut elif_parts = current_part.into_inner();

                        expect_rule(&mut elif_parts, Rule::onoff)?;
                        expect_rule(&mut elif_parts, Rule::left_paren)?;
                        let elif_condition = parse_expression(elif_parts.next().unwrap(), source)?;
                        expect_rule(&mut elif_parts, Rule::right_paren)?;
                        let elif_body = parse_block(elif_parts.next().unwrap(), source)?;

                        elif_blocks.push((elif_condition, elif_body))
                    },
                    Rule::on => {
                        if let Some(else_body_pair) = parts.next() {
                            if else_body_pair.as_rule() == Rule::block {
                                else_block = Some(parse_block(else_body_pair, source)?);
                            }
                        }
                        break;
                    },
                    _ => {
                        return Err(ParseError {
                            message: format!("Unexpected token {:?}", current_part.as_rule()),
                            span: None,
                        })
                    }
                }
            }
            StatementKind::If { condition, then_block, elif_blocks, else_block }
        }
        
        Rule::switch_statement => {
            let mut parts = inner.into_inner();
            expect_rule(&mut parts, Rule::wii)?;
            expect_rule(&mut parts, Rule::left_paren)?;
            let value = parse_expression(parts.next().unwrap(), source)?;
            expect_rule(&mut parts, Rule::right_paren)?;
            expect_rule(&mut parts, Rule::left_brace)?;
            
            let mut cases = Vec::new();
            for case_pair in parts {
                if let Rule::case_clause = case_pair.as_rule() {
                    cases.push(parse_case_clause(case_pair, source)?);
                }
            }
            
            StatementKind::Switch { value, cases }
        }
        
        Rule::for_loop => {
            let mut parts = inner.into_inner();
            expect_rule(&mut parts, Rule::ash)?;
            expect_rule(&mut parts, Rule::left_paren)?;
            let init = Box::new(parse_statement(parts.next().unwrap(), source)?);
            let condition = parse_expression(parts.next().unwrap(), source)?;
            expect_rule(&mut parts, Rule::semicolon)?;
            let update = parse_expression(parts.next().unwrap(), source)?;
            expect_rule(&mut parts, Rule::left_paren)?;
            let body = parse_block(parts.next().unwrap(), source)?;
            StatementKind::For { init, condition, update, body }
        }
        
        Rule::print_statement => {
            let mut parts = inner.into_inner();
            expect_rule(&mut parts, Rule::tnirp)?;
            expect_rule(&mut parts, Rule::left_paren)?;
            let value = parse_expression(parts.next().unwrap(), source)?;
            StatementKind::Print { value }
        }
        
        Rule::input_statement => {
            let mut parts = inner.into_inner();
            expect_rule(&mut parts, Rule::tupni)?;
            expect_rule(&mut parts, Rule::left_paren)?;
            let var_name = parts.next().unwrap().as_str().to_string();
            StatementKind::Input { var_name }
        }
        
        Rule::return_statement => {
            let mut parts = inner.into_inner();
            expect_rule(&mut parts, Rule::send)?;
            let value = if let Some(expr_pair) = parts.next() {
                Some(parse_expression(expr_pair, source)?)
            } else {
                None
            };
            StatementKind::Return { value }
        }
        
        Rule::break_statement => StatementKind::Break,
        
        Rule::continue_statement => StatementKind::Continue,
        
        Rule::expression_statement => {
            let expr = parse_expression(inner.into_inner().next().unwrap(), source)?;
            StatementKind::Expression { expr }
        }
        
        _ => return Err(ParseError {
            message: format!("Unknown statement rule: {:?}", inner.as_rule()),
            span: Some(SourceSpan::from_pest_span(inner.as_span(), source)),
        })
    };
    
    Ok(Statement { kind, span })
}

fn parse_block(pair: pest::iterators::Pair<Rule>, source: &str) -> Result<Vec<Statement>, ParseError> {
    let mut statements = Vec::new();
    
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::statement => statements.push(parse_statement(inner, source)?),
            Rule::left_brace | Rule::right_brace => {}, // Skip braces
            _ => {}
        }
    }
    
    Ok(statements)
}

fn parse_expression(pair: pest::iterators::Pair<Rule>, source: &str) -> Result<Expression, ParseError> {
    let span = SourceSpan::from_pest_span(pair.as_span(), source);
    
    let kind = match pair.as_rule() {
        Rule::expression | Rule::logical_or | Rule::logical_and | 
        Rule::equality | Rule::comparison | Rule::term | Rule::factor => {
            return parse_binary_expression(pair, source);
        }
        
        Rule::unary => return parse_unary_expression(pair, source),
        Rule::postfix => return parse_postfix_expression(pair, source),
        Rule::primary => return parse_primary_expression(pair, source),
        
        _ => return Err(ParseError {
            message: format!("Unknown expression rule: {:?}", pair.as_rule()),
            span: Some(span),
        })
    };
    
    Ok(Expression { kind, span })
}

fn parse_binary_expression(pair: pest::iterators::Pair<Rule>, source: &str) -> Result<Expression, ParseError> {
    let span = SourceSpan::from_pest_span(pair.as_span(), source);
    let mut parts = pair.into_inner().peekable();
    
    let mut left = parse_expression(parts.next().unwrap(), source)?;
    
    while let Some(op_pair) = parts.next() {
        let op = match op_pair.as_rule() {
            Rule::plus => BinaryOperator::Add,
            Rule::minus => BinaryOperator::Subtract,
            Rule::mult => BinaryOperator::Multiply,
            Rule::by => BinaryOperator::Divide,
            Rule::mod_op => BinaryOperator::Modulo,
            Rule::eq => BinaryOperator::Equal,
            Rule::noteq => BinaryOperator::NotEqual,
            Rule::great => BinaryOperator::GreaterThan,
            Rule::lesst => BinaryOperator::LessThan,
            Rule::and => BinaryOperator::And,
            Rule::or => BinaryOperator::Or,
            Rule::join => BinaryOperator::Join,
            _ => {
                let right = parse_expression(op_pair, source)?;
                if parts.peek().is_none() {
                    return Ok(left);
                }
                left = right;
                continue;
            }
        };
        
        if let Some(right_pair) = parts.next() {
            let right = parse_expression(right_pair, source)?;
            left = Expression {
                kind: ExpressionKind::BinaryOp {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                span: span.clone(),
            };
        }
    }
    
    Ok(left)
}

fn parse_unary_expression(pair: pest::iterators::Pair<Rule>, source: &str) -> Result<Expression, ParseError> {
    let span = SourceSpan::from_pest_span(pair.as_span(), source);
    let mut parts = pair.into_inner();

    if let Some(first) = parts.next() {
        match first.as_rule() {
            Rule::nah => {
                let operand = parse_expression(parts.next().unwrap(), source)?;
                Ok(Expression {
                    kind: ExpressionKind::UnaryOp { op: UnaryOperator::Not, operand: Box::new(operand) },
                    span
                })
            }
            Rule::minus => {
                let operand = parse_expression(parts.next().unwrap(), source)?;
                Ok(Expression {
                    kind: ExpressionKind::UnaryOp { op: UnaryOperator::Negate, operand: Box::new(operand) },
                    span
                })
            }
            _ => parse_expression(first, source)
        }
    } else {
        Err(ParseError { message: "Empty unary".to_string(), span: Some(span) })
    }
}

fn parse_postfix_expression(pair: pest::iterators::Pair<Rule>, source: &str) -> Result<Expression, ParseError> {
    let span = SourceSpan::from_pest_span(pair.as_span(), source);
    let mut parts = pair.into_inner();
    let mut expr = parse_expression(parts.next().unwrap(), source)?;
    
    for op_pair in parts {
        let op = match op_pair.as_rule() {
            Rule::plusplus => PostfixOperator::Increment,
            Rule::minusminus => PostfixOperator::Decrement,
            _ => continue,
        };
        
        expr = Expression {
            kind: ExpressionKind::PostfixOp {
                op,
                operand: Box::new(expr),
            },
            span: span.clone(),
        };
    }
    
    Ok(expr)
}

fn parse_primary_expression(pair: pest::iterators::Pair<Rule>, source: &str) -> Result<Expression, ParseError> {
    let span = SourceSpan::from_pest_span(pair.as_span(), source);
    let mut inner_pairs = pair.into_inner();
    let inner = inner_pairs.next().unwrap();
    
    let kind = match inner.as_rule() {
        Rule::left_paren => {
            let expr = inner_pairs.next().unwrap();
            return parse_expression(expr, source);
        }
        Rule::integer_literal => {
            ExpressionKind::IntegerLiteral(inner.as_str().parse().unwrap())
        }
        Rule::float_literal => {
            ExpressionKind::FloatLiteral(inner.as_str().parse().unwrap())
        }
        Rule::char_literal => {
            let s = inner.as_str();
            ExpressionKind::CharLiteral(s.chars().nth(1).unwrap())
        }
        Rule::string_literal => {
            let s = inner.as_str();
            ExpressionKind::StringLiteral(s[1..s.len()-1].to_string())
        }
        Rule::yes => ExpressionKind::BoolLiteral(true),
        Rule::no => ExpressionKind::BoolLiteral(false),
        Rule::function_call => {
            let (name, args) = parse_function_call_inner(inner, source)?;
            ExpressionKind::FunctionCall { name, args }
        }
        Rule::array_access => {
            let mut parts = inner.into_inner();
            let array = parts.next().unwrap().as_str().to_string();
            expect_rule(&mut parts, Rule::left_bracket)?;
            let index = parse_expression(parts.next().unwrap(), source)?;
            ExpressionKind::ArrayAccess {
                array,
                index: Box::new(index),
            }
        }
        Rule::identifier => {
            ExpressionKind::Identifier(inner.as_str().to_string())
        }
        Rule::expression => return parse_expression(inner, source),
        _ => return Err(ParseError {
            message: format!("Unknown primary expression: {:?}", inner.as_rule()),
            span: Some(SourceSpan::from_pest_span(inner.as_span(), source)),
        })
    };
    
    Ok(Expression { kind, span })
}

fn parse_function_call_inner(pair: pest::iterators::Pair<Rule>, source: &str) -> Result<(String, Vec<Expression>), ParseError> {
    let mut parts = pair.into_inner();
    let name = parts.next().unwrap().as_str().to_string();
    expect_rule(&mut parts, Rule::left_paren)?;
    
    let mut args = Vec::new();
    if let Some(next) = parts.next() {
        match next.as_rule() {
            Rule::arg_list => {
                for arg in next.into_inner() {
                    args.push(parse_expression(arg, source)?);
                }
                expect_rule(&mut parts, Rule::right_paren)?;
            },
            Rule::right_paren => {
                //
            },
            _ => {
                return Err(ParseError {
                    message: format!("Unexpected in function {:?}", next.as_rule()),
                    span: None,
                })
            }
        }
    }
    
    Ok((name, args))
}

impl Statement {
    pub fn start_position(&self) -> &Position {
        &self.span.start
    }
    
    pub fn end_position(&self) -> &Position {
        &self.span.end
    }
    
    pub fn contains_position(&self, line: usize, col: usize) -> bool {
        let pos = Position { line, col };
        self.span.contains(&pos)
    }
}

impl Expression {
    pub fn start_position(&self) -> &Position {
        &self.span.start
    }
    
    pub fn end_position(&self) -> &Position {
        &self.span.end
    }
}

impl SourceSpan {
    pub fn contains(&self, pos: &Position) -> bool {
        if pos.line < self.start.line || pos.line > self.end.line {
            return false;
        }
        
        if pos.line == self.start.line && pos.col < self.start.col {
            return false;
        }
        
        if pos.line == self.end.line && pos.col > self.end.col {
            return false;
        }
        
        true
    }
}

fn expect_rule<'a>(
    parts: &mut pest::iterators::Pairs<'a, Rule>,
    expected: Rule,
) -> Result<pest::iterators::Pair<'a, Rule>, ParseError> {
    match parts.next() {
        Some(pair) if pair.as_rule() == expected => Ok(pair),
        Some(pair) => Err(ParseError {
            message: format!("Expected {:?}, found {:?}", expected, pair.as_rule()),
            span: Some(SourceSpan::from_pest_span(pair.as_span(), "")),
        }),
        None => Err(ParseError {
            message: format!("Expected {:?}, found end of input", expected),
            span: None,
        }),
    }
}
