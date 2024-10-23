use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::path::Path;

#[derive(Debug, Clone)]
#[allow(dead_code)]
enum Token {
    IntegerLiteral(String, usize, usize),
    CharLiteral(String, usize, usize),
    Identifier(String, usize, usize),
    Operator(String, usize, usize),
    Keyword(String, usize, usize),
    Punto(String, usize, usize),        
    DosPuntos(String, usize, usize),    
    PuntoYComa(String, usize, usize),   
    Coma(String, usize, usize),         
    Asignacion(String, usize, usize),   
    Complement(String, usize, usize),   
    ParenIzq(String, usize, usize),     
    ParenDer(String, usize, usize),     
    CorchIzq(String, usize, usize),     
    CorchDer(String, usize, usize),     
    LlaveIzq(String, usize, usize),     
    LlaveDer(String, usize, usize),     
}

struct Lexer<'a> {
    input: &'a str,
    position: usize,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer {
            input,
            position: 0,
            line: 1,
            column: 1,
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        if self.skip_comment() {
            return self.next_token();
        }
    
        let start_line = self.line;
        let start_column = self.column;
    
        if let Some(current_char) = self.input.chars().nth(self.position) {
            let token = match current_char {
                '\'' => self.collect_char_literal(start_line, start_column),
                ch if ch.is_digit(10) => self.collect_integer_literal(start_line, start_column),
                ch if ch.is_alphabetic() => self.collect_identifier_or_keyword(start_line, start_column),
                ch if "+-*/=<>\\&@%?".contains(ch) || ".:;~,()[]{}".contains(ch) => {
                    self.collect_operator_or_symbol(start_line, start_column)
                },
                _ => return None,
            };
            Some(token)
        } else {
            None
        }
    }


    fn collect_integer_literal(&mut self, line: usize, column: usize) -> Token {
        let start = self.position;
        while let Some(c) = self.input.chars().nth(self.position) {
            if c.is_digit(10) {
                self.advance_position();
            } else {
                break;
            }
        }
        let literal = &self.input[start..self.position];
        Token::IntegerLiteral(literal.to_string(), line, column)
    }

    fn collect_char_literal(&mut self, line: usize, column: usize) -> Token {
        self.advance_position();
        let start = self.position;
        while let Some(c) = self.input.chars().nth(self.position) {
            if c != '\'' {
                self.advance_position(); 
            } else {
                break; 
            }
        }
        let literal = &self.input[start..self.position];
        self.advance_position(); 
        Token::CharLiteral(literal.to_string(), line, column)
    }

    fn collect_identifier_or_keyword(&mut self, line: usize, column: usize) -> Token {
        let start = self.position;
        while let Some(c) = self.input.chars().nth(self.position) {
            if c.is_alphanumeric() || c == '_' {
                self.advance_position();
            } else {
                break;
            }
        }
        let identifier = &self.input[start..self.position];
        if self.is_keyword(identifier) {
            Token::Keyword(identifier.to_string(), line, column)
        } else {
            Token::Identifier(identifier.to_string(), line, column)
        }
    }

    fn collect_operator_or_symbol(&mut self, line: usize, column: usize) -> Token {
        let current_char = self.input.chars().nth(self.position).unwrap();
        self.advance_position();
        
        if current_char == ':' && self.input.chars().nth(self.position) == Some('=') {
            let mut value = String::new();
            value.push(current_char);
            value.push('=');
            self.advance_position();
            return Token::Asignacion(value, line, column); 
        }
        
        match current_char {
            '.' => Token::Punto(current_char.to_string(), line, column),
            ':' => Token::DosPuntos(current_char.to_string(), line, column),
            ';' => Token::PuntoYComa(current_char.to_string(), line, column),
            ',' => Token::Coma(current_char.to_string(), line, column),
            '~' => Token::Complement(current_char.to_string(), line, column),
            '(' => Token::ParenIzq(current_char.to_string(), line, column),
            ')' => Token::ParenDer(current_char.to_string(), line, column),
            '[' => Token::CorchIzq(current_char.to_string(), line, column),
            ']' => Token::CorchDer(current_char.to_string(), line, column),
            '{' => Token::LlaveIzq(current_char.to_string(), line, column),
            '}' => Token::LlaveDer(current_char.to_string(), line, column),
            '+' | '-' | '*' | '/' | '=' | '<' | '>' | '\\' | '&' | '@' | '%' | '?' | '^' => {
                Token::Operator(current_char.to_string(), line, column)
            },
            _ => unreachable!("Unhandled character in lexer: {}", current_char),
        }
    }
    
    


    fn is_keyword(&self, word: &str) -> bool {
        matches!(word, "array" | "begin" | "const" | "do" | "else" | "end" |
                        "func" | "if" | "in" | "let" | "of" | "proc" | "record" |
                        "then" | "type" | "var" | "while" | "Integer" | "Boolean" | "String")
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.input.chars().nth(self.position) {
            if c.is_whitespace() {
                self.advance_position();
                if c == '\n' {
                    self.line += 1;
                    self.column = 0;
                }
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) -> bool {
        if self.input.chars().nth(self.position) == Some('!') {
            while let Some(c) = self.input.chars().nth(self.position) {
                self.advance_position();
                if c == '\n' {
                    self.line += 1;
                    self.column = 0;
                    break;
                }
            }
            true
        } else {
            false
        }
    }
    
    fn advance_position(&mut self) {
        if let Some(c) = self.input.chars().nth(self.position) {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.position += 1;
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: tokenize <input_file> [-o <output_file>]");
        std::process::exit(1);
    }

    let output_dir = "Outputs";
    let input_dir = "Inputs";

    let input_filename = &args[1];
    let input_path = Path::new(input_dir).join(input_filename);

    let output_filename = if args.len() > 3 && args[2] == "-o" {
        &args[3]
    } else {
        "tokens.out"
    };
    let output_path = Path::new(output_dir).join(output_filename);

    let input_file = File::open(&input_path).expect("Error opening input file.");
    let reader = BufReader::new(input_file);
    let mut output_file = File::create(&output_path).expect("Error creating output file.");

    for (index, line) in reader.lines().enumerate() {
        let line = line.expect("Error reading line.");
        let mut lexer = Lexer::new(&line);
        lexer.line = index + 1;
        lexer.column = 1;
        
        while let Some(token) = lexer.next_token() {
            match token {
                Token::IntegerLiteral(val, ln, col) => writeln!(output_file, "Token: {:?}, Tipo: IntegerLiteral, Linea: {}, Columna: {}", val, ln, col),
                Token::CharLiteral(val, ln, col) => writeln!(output_file, "Token: {:?}, Tipo: CharLiteral, Linea: {}, Columna: {}", val, ln, col),
                Token::Identifier(val, ln, col) => writeln!(output_file, "Token: {:?}, Tipo: Identifier, Linea: {}, Columna: {}", val, ln, col),
                Token::Operator(val, ln, col) => writeln!(output_file, "Token: {:?}, Tipo: Operator, Linea: {}, Columna: {}", val, ln, col),
                Token::Keyword(val, ln, col) => writeln!(output_file, "Token: {:?}, Tipo: Keyword, Linea: {}, Columna: {}", val, ln, col),
                Token::Punto(_, ln, col) => writeln!(output_file, "Token: '.', Tipo: Punto, Linea: {}, Columna: {}", ln, col),
                Token::DosPuntos(_, ln, col) => writeln!(output_file, "Token: ':', Tipo: DosPuntos, Linea: {}, Columna: {}", ln, col),
                Token::PuntoYComa(_, ln, col) => writeln!(output_file, "Token: ';', Tipo: PuntoYComa, Linea: {}, Columna: {}", ln, col),
                Token::Coma(_, ln, col) => writeln!(output_file, "Token: ',', Tipo: Coma, Linea: {}, Columna: {}", ln, col),
                Token::Asignacion(_, ln, col) => writeln!(output_file, "Token: ':=', Tipo: Asignacion, Linea: {}, Columna: {}", ln, col),
                Token::Complement(_, ln, col) => writeln!(output_file, "Token: '~', Tipo: Complement, Linea: {}, Columna: {}", ln, col),
                Token::ParenIzq(_, ln, col) => writeln!(output_file, "Token: '(', Tipo: ParenIzq, Linea: {}, Columna: {}", ln, col),
                Token::ParenDer(_, ln, col) => writeln!(output_file, "Token: ')', Tipo: ParenDer, Linea: {}, Columna: {}", ln, col),
                Token::CorchIzq(_, ln, col) => writeln!(output_file, "Token: '[', Tipo: CorchIzq, Linea: {}, Columna: {}", ln, col),
                Token::CorchDer(_, ln, col) => writeln!(output_file, "Token: ']', Tipo: CorchDer, Linea: {}, Columna: {}", ln, col),
                Token::LlaveIzq(_, ln, col) => writeln!(output_file, "Token: '{{', Tipo: LlaveIzq, Linea: {}, Columna: {}", ln, col),
                Token::LlaveDer(_, ln, col) => writeln!(output_file, "Token: '}}', Tipo: LlaveDer, Linea: {}, Columna: {}", ln, col),
                //_ => unreachable!(),
            }.expect("Error writing to output file.");
        }
        writeln!(output_file).expect("Error writing newline to output file.");
    }
}
