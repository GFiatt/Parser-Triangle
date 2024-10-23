use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use serde::{Serialize, Deserialize};
use regex::Regex;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
enum TokenType {
    IntegerLiteral,
    CharacterLiteral,
    Identifier,
    Operator,
    Keyword,
    Punto,
    DosPuntos,
    PuntoYComa,
    Coma,
    Asignacion,
    Igual,
    Complement,
    ParenIzq,
    ParenDer,
    CorchIzq,
    CorchDer,
    LlaveIzq,
    LlaveDer,
    EOF,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Token {
    token_type: TokenType,
    lexeme: String,
    line: usize,
    column: usize,
}

#[derive(Debug, Serialize, Deserialize)]
enum ASTNode {
    Program(Box<ASTNode>),
    CommandSequence(Vec<ASTNode>),
    Assignment {
        vname: Box<ASTNode>,
        expression: Box<ASTNode>,
    },
    VName(String),
    LetCommand {
        declarations: Box<ASTNode>,
        command: Box<ASTNode>,
    },
    DeclarationSequence(Vec<ASTNode>),
    ConstDeclaration {
        identifier: String,
        expression: Box<ASTNode>,
    },
    VarDeclaration {
        identifier: String,
        type_denoter: Box<ASTNode>,
    },
    TypeDeclaration {
        identifier: String,
        type_denoter: Box<ASTNode>,
    },
    IfCommand {
        condition: Box<ASTNode>,
        then_command: Box<ASTNode>,
        else_command: Box<ASTNode>,
    },
    WhileCommand {
        condition: Box<ASTNode>,
        command: Box<ASTNode>,
    },
    CallCommand {
        identifier: String,
        actual_parameters: Vec<ASTNode>,
    },
    IntegerLiteral(i64),
    CharacterLiteral(char),
    Operator(String),
    RecordAggregate(Vec<(String, ASTNode)>),
    ArrayAggregate(Vec<ASTNode>),
    TypeDenoter(String),
    ArrayType {
        size: Box<ASTNode>,
        element_type: Box<ASTNode>,
    },
    RecordType(Vec<(String, ASTNode)>),
    DotVName {
        vname: Box<ASTNode>,
        identifier: String,
    },
    SubscriptVName {
        vname: Box<ASTNode>,
        expression: Box<ASTNode>,
    },
    BinaryExpression {
        operator: String,
        left: Box<ASTNode>,
        right: Box<ASTNode>,
    },
    Empty,
}

struct Parser {
    tokens: Vec<Token>,
    position: usize,
    in_begin_block: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, position: 0 , in_begin_block: 0}
    }

    fn parse(&mut self) -> Result<ASTNode, String> {
        let command = self.parse_command()?;
        Ok(ASTNode::Program(Box::new(command)))
    }

    fn token_context(&self, token: &Token) -> &'static str {
        match token.token_type {
            TokenType::Asignacion => "Assignment",
            
            TokenType::IntegerLiteral | TokenType::CharacterLiteral | 
            TokenType::Identifier | TokenType::Operator | 
            TokenType::ParenIzq | TokenType::ParenDer | 
            TokenType::LlaveIzq | TokenType::LlaveDer | 
            TokenType::CorchIzq | TokenType::CorchDer => "Expression",

            TokenType::DosPuntos | TokenType::Complement | 
            TokenType::Keyword => "Declaration",
            

            //TokenType::Identifier if self.is_type_context(token) => "Type-Denoter",


            _ => "Unknown"
        }
    }

    #[allow(dead_code)]
    fn is_type_context(&self, _token: &Token) -> bool {
        false
    }

    fn parse_command(&mut self) -> Result<ASTNode, String> {
        let mut commands = Vec::new();
    
        while !self.is_block_terminator() {

            if self.current_token_lexeme() == "begin" || self.current_token_lexeme() == "let" {
                self.in_begin_block += 1;
            }

            while self.current_token().token_type == TokenType::PuntoYComa {
                self.advance();
            }

            if self.is_block_terminator() || self.current_token().token_type == TokenType::EOF {
                break;
            }
            
            let command = self.parse_single_command()?;
            commands.push(command);
            
            
            if self.current_token_lexeme() == "end" && self.in_begin_block > 0 {
                self.in_begin_block -= 1;
            }
            
            if !self.is_semicolon_optional() && self.current_token().token_type != TokenType::PuntoYComa {
                return Err(format!("Syntax Error: Missing ';' at line {}, column {}", self.current_token().line, self.current_token().column));
            }

            if self.current_token().token_type == TokenType::PuntoYComa {
                self.advance();
            }
        }
    
        if commands.len() == 1 {
            Ok(commands.remove(0))
        } else {
            Ok(ASTNode::CommandSequence(commands))
        }
    }
    
    fn is_block_terminator(&self) -> bool {
        match self.current_token_lexeme().as_str() {
            "end" | "else" | "in" => true,
            _ => false,
        }
    }
    
    fn is_semicolon_optional(&self) -> bool {
        let next_pos = self.position;
        if next_pos < self.tokens.len() {
            let next_token = &self.tokens[next_pos];
            !(matches!(next_token.token_type, TokenType::Keyword) && (next_token.lexeme == "begin" || next_token.lexeme == "let"))
        } else {
            true
        }
    }

    fn parse_single_command(&mut self) -> Result<ASTNode, String> {
        match self.current_token_lexeme().as_str() {
            "let" => self.parse_let_command(),
            "if" => self.parse_if_command(),
            "putint" | "put" => self.parse_call_command(),
            "while" => self.parse_while_command(),
            "begin" => {
                self.advance();
                let command = self.parse_command()?;
                self.expect_keyword("end")?;
                Ok(command)
            }
            _ => {
                let start_position = self.position;
                if let Ok(vname) = self.parse_vname() {
                    if self.current_token().token_type == TokenType::Asignacion {
                        self.advance();
                        let expression = self.parse_expression()?;
                        return Ok(ASTNode::Assignment {
                            vname: Box::new(vname),
                            expression: Box::new(expression),
                        });
                    } else {
                        self.position = start_position;
                    }
                }
    
                if self.current_token().token_type == TokenType::Identifier && self.next_token_lexeme() == "(" {
                    self.parse_call_command()
                } else {
                    let context = self.token_context(self.current_token());
                    let error_message = match context {
                        "Expression" => "an Expression",
                        "Declaration" => "a Declaration",
                        "Type-Denoter" => "a Type-Denoter",
                        "Assignment" => "an assignment operator",
                        _ => "Unexpected token",
                    };
                    self.error("Expected a single-Command", self.current_token(), &error_message)
                }
            }
        }
    }            

    fn parse_let_command(&mut self) -> Result<ASTNode, String> {
        self.advance();
        let declarations = self.parse_declaration()?;
        self.expect_keyword("in")?;
        let command = self.parse_command()?;
        Ok(ASTNode::LetCommand {
            declarations: Box::new(declarations),
            command: Box::new(command),
        })
    }

    fn parse_if_command(&mut self) -> Result<ASTNode, String> {
        let mut outside = 0;
        self.back_lexeme();
        if self.current_token_lexeme() == "(" {
            outside = 1;
            self.advance(); 
        } else if self.current_token_lexeme() != "if" {
            self.advance(); 
        }

        
        self.advance();
        let condition = self.parse_expression()?;

        self.expect_keyword("then")?;
        
        let then_branch = if self.in_begin_block > 0 {
            if outside == 1 {
                self.parse_expression()?  
            } else {
                self.parse_command()?
            }
        } else {
            self.parse_expression()?
        };

        self.expect_keyword("else")?;

        let else_branch = if self.in_begin_block > 0 {
            if outside == 1 {
                self.parse_expression()?    
            } else {
                self.parse_command()?
            }
        } else {
            self.parse_expression()?
        };    

        Ok(ASTNode::IfCommand {
            condition: Box::new(condition),
            then_command: Box::new(then_branch),
            else_command: Box::new(else_branch),
        })
    }    

    fn parse_while_command(&mut self) -> Result<ASTNode, String> {
        self.advance();
        let condition = self.parse_expression()?;
        self.expect_keyword("do")?;
        let command = self.parse_command()?;
        Ok(ASTNode::WhileCommand {
            condition: Box::new(condition),
            command: Box::new(command),
        })
    }

    #[allow(dead_code)]
    fn parse_assignment_command(&mut self) -> Result<ASTNode, String> {
        let vname = self.parse_vname()?;
        self.expect_token(TokenType::Asignacion)?;
        let expression = self.parse_expression()?;
        Ok(ASTNode::Assignment {
            vname: Box::new(vname),
            expression: Box::new(expression),
        })
    }

    fn parse_call_command(&mut self) -> Result<ASTNode, String> {
        let identifier = self.expect_identifier()?;
        self.expect_token(TokenType::ParenIzq)?;
        let actual_parameters = self.parse_actual_parameter_sequence()?;
        self.expect_token(TokenType::ParenDer)?;
        Ok(ASTNode::CallCommand {
            identifier,
            actual_parameters,
        })
    }

    fn parse_expression(&mut self) -> Result<ASTNode, String> {
        self.parse_binary_expression(0)
    }

    fn parse_binary_expression(&mut self, min_precedence: u8) -> Result<ASTNode, String> {
        let mut left = self.parse_primary_expression()?;
        while let Some(op_info) = self.get_operator_info() {
            if op_info.precedence >= min_precedence {
                let _op = self.advance_lexeme();
                let right = self.parse_binary_expression(op_info.precedence + 1)?;
                left = ASTNode::BinaryExpression {
                    operator: op_info.operator,
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn get_operator_info(&self) -> Option<OperatorInfo> {
        if self.current_token().token_type == TokenType::Operator {
            let op = self.current_token_lexeme();
            let precedence = match op.as_str() {
                "*" | "/" => 2,
                "+" | "-" => 1,
                _ => 0,
            };
            Some(OperatorInfo {
                operator: op,
                precedence,
            })
        } else {
            None
        }
    }

    fn parse_primary_expression(&mut self) -> Result<ASTNode, String> {
        match self.current_token().token_type {
            TokenType::IntegerLiteral => {
                let value = self.current_token().lexeme.parse::<i64>().unwrap();
                self.advance();
                Ok(ASTNode::IntegerLiteral(value))
            }
            TokenType::CharacterLiteral => {
                let lexeme = self.current_token().lexeme.clone();
                let value = lexeme.chars().next().unwrap();
                self.advance();
                Ok(ASTNode::CharacterLiteral(value))
            }
            TokenType::Identifier => {
                if self.next_token_lexeme() == "(" {
                    self.parse_call_command()
                } else {
                    self.parse_vname()
                }
            }
            TokenType::Operator => {
                let operator = self.advance_lexeme();
                let _operand = self.parse_primary_expression()?;
                Ok(ASTNode::Operator(operator))
            }
            TokenType::ParenIzq => {
                self.advance();
                let expr = self.handle_parenthesized_expression()?;
                self.expect_token(TokenType::ParenDer)?;
                Ok(expr)
            }
            TokenType::LlaveIzq => {
                self.advance();
                let record_aggregate = self.parse_record_aggregate()?;
                self.expect_token(TokenType::LlaveDer)?;
                Ok(ASTNode::RecordAggregate(record_aggregate))
            }
            TokenType::CorchIzq => {
                self.advance();
                let array_aggregate = self.parse_array_aggregate()?;
                self.expect_token(TokenType::CorchDer)?;
                Ok(ASTNode::ArrayAggregate(array_aggregate))
            }
            _ => self.error("Excected a primary expression", &self.current_token(), ""),
        }
    }

    fn handle_parenthesized_expression(&mut self) -> Result<ASTNode, String> {
        if self.current_token_lexeme() == "if" {
            self.parse_if_command()
        } else {
            self.parse_expression()
        }
    }

    fn parse_vname(&mut self) -> Result<ASTNode, String> {
        let mut vname = ASTNode::VName(self.expect_identifier()?);
        loop {
            if self.current_token_lexeme() == "." {
                self.advance();
                let field = self.expect_identifier()?;
                vname = ASTNode::DotVName {
                    vname: Box::new(vname),
                    identifier: field,
                };
            } else if self.current_token().token_type == TokenType::CorchIzq {
                self.advance();
                let expression = self.parse_expression()?;
                self.expect_token(TokenType::CorchDer)?;
                vname = ASTNode::SubscriptVName {
                    vname: Box::new(vname),
                    expression: Box::new(expression),
                };
            } else {
                break;
            }
        }
        Ok(vname)
    }
    
    fn parse_actual_parameter_sequence(&mut self) -> Result<Vec<ASTNode>, String> {
        let mut parameters = Vec::new();
        if self.current_token().token_type != TokenType::ParenDer {
            parameters.push(self.parse_expression()?);
            while self.current_token().token_type == TokenType::Coma {
                self.advance();
                parameters.push(self.parse_expression()?);
            }
        }
        Ok(parameters)
    }

    fn parse_declaration(&mut self) -> Result<ASTNode, String> {
        let mut declarations = Vec::new();
        declarations.push(self.parse_single_declaration()?);
        while self.current_token().token_type == TokenType::PuntoYComa {
            self.advance();
            declarations.push(self.parse_single_declaration()?);
        }
        if declarations.len() == 1 {
            Ok(declarations.remove(0))
        } else {
            Ok(ASTNode::DeclarationSequence(declarations))
        }
    }

    fn parse_single_declaration(&mut self) -> Result<ASTNode, String> {
        match self.current_token_lexeme().as_str() {
            "const" => self.parse_const_declaration(),
            "var" => self.parse_var_declaration(),
            "type" => self.parse_type_declaration(),
            _ => self.error("a declaration", &self.current_token(), "hola5"),
        }
    }

    fn parse_const_declaration(&mut self) -> Result<ASTNode, String> {
        self.advance();
        let identifier = self.expect_identifier()?;
        self.expect_token(TokenType::Complement)?;
        let expression = self.parse_expression()?;
        Ok(ASTNode::ConstDeclaration {
            identifier,
            expression: Box::new(expression),
        })
    }

    fn parse_var_declaration(&mut self) -> Result<ASTNode, String> {
        self.advance();
        let identifier = self.expect_identifier()?;
        self.expect_token(TokenType::DosPuntos)?;
        let type_denoter = self.parse_type_denoter()?;
        Ok(ASTNode::VarDeclaration {
            identifier,
            type_denoter: Box::new(type_denoter),
        })
    }

    fn parse_type_declaration(&mut self) -> Result<ASTNode, String> {
        self.advance();
        let identifier = self.expect_identifier()?;
        self.expect_token(TokenType::Complement)?;
        let type_denoter = self.parse_type_denoter()?;
        Ok(ASTNode::TypeDeclaration {
            identifier,
            type_denoter: Box::new(type_denoter),
        })
    }

    fn parse_type_denoter(&mut self) -> Result<ASTNode, String> {
        if self.current_token_lexeme() == "array" {
            self.advance();
            let size = self.parse_expression()?;
            self.expect_keyword("of")?;
            let element_type = self.parse_type_denoter()?;
            Ok(ASTNode::ArrayType {
                size: Box::new(size),
                element_type: Box::new(element_type),
            })
        } else if self.current_token_lexeme() == "record" {
            self.advance();
            let fields = self.parse_field_type_denoter()?;
            self.expect_keyword("end")?;
            Ok(ASTNode::RecordType(fields))
        } else if self.current_token().token_type == TokenType::Identifier || self.current_token().token_type == TokenType::Keyword {
            let identifier = self.advance_lexeme();
            Ok(ASTNode::TypeDenoter(identifier))
        } else {
            self.error("a type denoter", &self.current_token(), "hola")
        }
    }

    fn parse_field_type_denoter(&mut self) -> Result<Vec<(String, ASTNode)>, String> {
        let mut fields = Vec::new();
        loop {
            let identifier = self.expect_identifier()?;
            self.expect_token(TokenType::DosPuntos)?;
            let type_denoter = self.parse_type_denoter()?;
            fields.push((identifier, type_denoter));
            if self.current_token().token_type == TokenType::Coma {
                self.advance();
            } else {
                break;
            }
        }
        Ok(fields)
    }

    fn parse_record_aggregate(&mut self) -> Result<Vec<(String, ASTNode)>, String> {
        let mut fields = Vec::new();
        loop {
            let identifier = self.expect_identifier()?;
            self.expect_token(TokenType::Complement)?;
            let expression = self.parse_expression()?;
            fields.push((identifier, expression));
            if self.current_token().token_type == TokenType::Coma {
                self.advance();
            } else {
                break;
            }
        }
        Ok(fields)
    }

    fn parse_array_aggregate(&mut self) -> Result<Vec<ASTNode>, String> {
        let mut elements = Vec::new();
        loop {
            let expression = self.parse_expression()?;
            elements.push(expression);
            if self.current_token().token_type == TokenType::Coma {
                self.advance();
            } else {
                break;
            }
        }
        Ok(elements)
    }

    fn current_token(&self) -> &Token {
        &self.tokens[self.position]
    }

    fn current_token_lexeme(&self) -> String {
        self.current_token().lexeme.clone()
    }

    fn next_token_lexeme(&self) -> String {
        if self.position + 1 < self.tokens.len() {
            self.tokens[self.position + 1].lexeme.clone()
        } else {
            "".to_string()
        }
    }

    fn advance(&mut self) {
        if self.position < self.tokens.len() {
            self.position += 1;
        }
    }

    fn back(&mut self) {
        if self.position > 0 {
            self.position -= 1;
        }
    }

    fn advance_lexeme(&mut self) -> String {
        let lexeme = self.current_token().lexeme.clone();
        self.advance();
        lexeme
    }

    fn back_lexeme(&mut self) -> String {
        let lexeme = self.current_token().lexeme.clone();
        self.back();
        lexeme
    }

    fn expect_token(&mut self, expected: TokenType) -> Result<(), String> {
        if self.current_token().token_type == expected {
            self.advance();
            Ok(())
        } else {
            self.error(
                "Expected an Expression",
                &self.current_token(),
                "a Command"
            )
        }
    }

    fn expect_keyword(&mut self, keyword: &str) -> Result<(), String> {
        if self.current_token_lexeme() == keyword {
            self.advance();
            Ok(())
        } else {
            println!("{:?}", self.current_token());
            println!("{:?}", self.current_token().token_type);
            
            self.error(
                &format!("Missing '{}'", keyword),
                &self.current_token(),
                ""
            )
        }
    }

    fn expect_identifier(&mut self) -> Result<String, String> {
        if self.current_token().token_type == TokenType::Identifier {
            Ok(self.advance_lexeme())
        } else {
            println!("{:?}", self.current_token());
            println!("{:?}", self.current_token().token_type);

            self.error("Expected an identifier", &self.current_token(), "an Expression")
        }
    }

    fn error<T>(&self, expected: &str, token: &Token, found: &str) -> Result<T, String> {
        if token.lexeme == "" {
            Err(format!(
                "Syntax Error at line {}, column {}: {} but found end of line {}",
                token.line, token.column, expected, found
            ))
        } else if token.lexeme == "if" {
            Err(format!(
                "Syntax Error at line {}, column {}: {}",
                token.line, token.column, expected
            ))
        } else {
            Err(format!(
                "Syntax Error at line {}, column {}: {} but found {} ('{}')",
                token.line, token.column, expected, found, token.lexeme
            ))
        }
        
    }
}

struct OperatorInfo {
    operator: String,
    precedence: u8,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Uso: parse <archivo_entrada>");
        std::process::exit(1);
    }

    let input_filename = &args[1];
    let output_filename = "Outputs/tree.out";

    let input_path = Path::new(&input_filename);
    let output_path = Path::new(&output_filename);

    let file = File::open(&input_path).expect("Error al abrir el archivo de entrada.");
    let reader = BufReader::new(file);

    let tokens = read_tokens(reader).expect("Error al leer los tokens.");

    let mut parser = Parser::new(tokens);

    match parser.parse() {
        Ok(ast) => {
            let json = serde_json::to_string_pretty(&ast).expect("Error al serializar el AST.");
            let mut output_file = File::create(&output_path).expect("Error al crear el archivo de salida.");
            output_file.write_all(json.as_bytes()).expect("Error al escribir en el archivo de salida.");
            println!("Análisis sintáctico completado. AST guardado en {}", output_path.display());
        }
        Err(e) => {
            eprintln!("{}", e);
        }
    }
}

fn read_tokens(reader: BufReader<File>) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    for line in reader.lines() {
        let line = line.map_err(|e| e.to_string())?;
        if line.trim().is_empty() {
            continue;
        }
        let token = parse_token_line(&line)?;
        tokens.push(token);
    }
    tokens.push(Token {
        token_type: TokenType::EOF,
        lexeme: "".to_string(),
        line: 0,
        column: 0,
    });
    Ok(tokens)
}

fn parse_token_line(line: &str) -> Result<Token, String> {
    let re = Regex::new(r#"Token:\s*(?P<lexeme>".*?"|'.*?'|[^,]+),\s*Tipo:\s*(?P<tipo>[^,]+),\s*Linea:\s*(?P<linea>\d+),\s*Columna:\s*(?P<columna>\d+)"#).unwrap();

    if let Some(caps) = re.captures(line) {
        let lexeme = caps.name("lexeme").unwrap().as_str();
        let tipo_part = caps.name("tipo").unwrap().as_str();
        let line_number = caps.name("linea").unwrap().as_str().parse::<usize>().unwrap_or(0);
        let column_number = caps.name("columna").unwrap().as_str().parse::<usize>().unwrap_or(0);

        let lexeme = lexeme.trim().trim_matches(|c| c == '"' || c == '\'').to_string();

        let token_type = match tipo_part.trim() {
            "IntegerLiteral" => TokenType::IntegerLiteral,
            "CharLiteral" => TokenType::CharacterLiteral,
            "CharacterLiteral" => TokenType::CharacterLiteral,
            "Identifier" => TokenType::Identifier,
            "Operator" => TokenType::Operator,
            "Keyword" => TokenType::Keyword,
            "Punto" => TokenType::Punto,
            "DosPuntos" => TokenType::DosPuntos,
            "PuntoYComa" => TokenType::PuntoYComa,
            "Coma" => TokenType::Coma,
            "Asignacion" => TokenType::Asignacion,
            "Igual" => TokenType::Igual,
            "Complement" => TokenType::Complement,
            "ParenIzq" => TokenType::ParenIzq,
            "ParenDer" => TokenType::ParenDer,
            "CorchIzq" => TokenType::CorchIzq,
            "CorchDer" => TokenType::CorchDer,
            "LlaveIzq" => TokenType::LlaveIzq,
            "LlaveDer" => TokenType::LlaveDer,
            _ => {
                return Err(format!(
                    "Tipo de token desconocido: {} en la línea {}",
                    tipo_part, line_number
                ))
            }
        };

        Ok(Token {
            token_type,
            lexeme,
            line: line_number,
            column: column_number,
        })
    } else {
        Err(format!("Línea inválida en el archivo de tokens: {}", line))
    }
}
