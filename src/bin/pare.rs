use std::env;
use std::fs::File;
use std::io::{Read, Write};
use serde::Deserialize;
use petgraph::dot::{Config, Dot};
use petgraph::graph::{Graph, NodeIndex};
use std::path::Path;

#[derive(Debug, Deserialize)]
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
    ProcDeclaration {
        identifier: String,
        parameters: Vec<(String, ASTNode)>,
        command: Box<ASTNode>,
    },
    FuncDeclaration {
        identifier: String,
        parameters: Vec<(String, ASTNode)>,
        return_type: Box<ASTNode>,
        body: Box<ASTNode>,
    },
    IfCommand {
        condition: Box<ASTNode>,
        then_command: Box<ASTNode>,
        else_command: Option<Box<ASTNode>>,
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
    UnaryExpression {
        operator: String,
        operand: Box<ASTNode>,
    },
    Empty,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Uso: pare <archivo_entrada> [-o <archivo_salida>]");
        std::process::exit(1);
    }

    let input_filename = format!("Outputs/{}", args[1]);
    let output_filename = if args.len() > 3 && args[2] == "-o" {
        format!("Outputs/{}", args[3])
    } else {
        "Outputs/tree.dot".to_string()
    };

    let input_path = Path::new(&input_filename);
    let mut file = File::open(&input_path).expect("Error al abrir el archivo de entrada.");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Error al leer el archivo de entrada.");

    let ast: ASTNode = serde_json::from_str(&contents).expect("Error al deserializar el AST.");

    let mut graph = Graph::<String, ()>::new();

    fn build_graph(
        node: &ASTNode,
        graph: &mut Graph<String, ()>,
        parent: Option<NodeIndex>,
    ) -> NodeIndex {
        let label = match node {
            ASTNode::Program(_) => "Program".to_string(),
            ASTNode::CommandSequence(_) => "CommandSequence".to_string(),
            ASTNode::Assignment { .. } => "Assignment".to_string(),
            ASTNode::VName(name) => format!("VName({})", name),
            ASTNode::LetCommand { .. } => "LetCommand".to_string(),
            ASTNode::DeclarationSequence(_) => "DeclarationSequence".to_string(),
            ASTNode::ConstDeclaration { identifier, .. } => format!("ConstDeclaration({})", identifier),
            ASTNode::VarDeclaration { identifier, .. } => format!("VarDeclaration({})", identifier),
            ASTNode::TypeDeclaration { identifier, .. } => format!("TypeDeclaration({})", identifier),
            ASTNode::ProcDeclaration { identifier, .. } => format!("ProcDeclaration({})", identifier),
            ASTNode::FuncDeclaration { identifier, .. } => format!("FuncDeclaration({})", identifier),
            ASTNode::IfCommand { .. } => "IfCommand".to_string(),
            ASTNode::WhileCommand { .. } => "WhileCommand".to_string(),
            ASTNode::CallCommand { identifier, .. } => format!("CallCommand({})", identifier),
            ASTNode::IntegerLiteral(value) => format!("IntegerLiteral({})", value),
            ASTNode::CharacterLiteral(ch) => format!("CharacterLiteral({})", ch),
            ASTNode::Operator(op) => format!("Operator({})", op),
            ASTNode::RecordAggregate(_) => "RecordAggregate".to_string(),
            ASTNode::ArrayAggregate(_) => "ArrayAggregate".to_string(),
            ASTNode::TypeDenoter(name) => format!("TypeDenoter({})", name),
            ASTNode::ArrayType { .. } => "ArrayType".to_string(),
            ASTNode::RecordType(_) => "RecordType".to_string(),
            ASTNode::DotVName { identifier, .. } => format!("DotVName({})", identifier),
            ASTNode::SubscriptVName { .. } => "SubscriptVName".to_string(),
            ASTNode::BinaryExpression { operator, .. } => format!("BinaryExpression({})", operator),
            ASTNode::UnaryExpression { operator, .. } => format!("UnaryExpression({})", operator),
            ASTNode::Empty => "Empty".to_string(),
        };

        let current = graph.add_node(label);

        if let Some(parent_idx) = parent {
            graph.add_edge(parent_idx, current, ());
        }

        match node {
            ASTNode::Program(child) => {
                build_graph(child, graph, Some(current));
            }
            ASTNode::CommandSequence(commands) => {
                for cmd in commands {
                    build_graph(cmd, graph, Some(current));
                }
            }
            ASTNode::Assignment { vname, expression } => {
                build_graph(vname, graph, Some(current));
                build_graph(expression, graph, Some(current));
            }
            ASTNode::VName(_) => {}
            ASTNode::LetCommand { declarations, command } => {
                build_graph(declarations, graph, Some(current));
                build_graph(command, graph, Some(current));
            }
            ASTNode::DeclarationSequence(declarations) => {
                for decl in declarations {
                    build_graph(decl, graph, Some(current));
                }
            }
            ASTNode::ConstDeclaration { expression, .. } => {
                build_graph(expression, graph, Some(current));
            }
            ASTNode::VarDeclaration { type_denoter, .. } => {
                build_graph(type_denoter, graph, Some(current));
            }
            ASTNode::TypeDeclaration { type_denoter, .. } => {
                build_graph(type_denoter, graph, Some(current));
            }
            ASTNode::ProcDeclaration { parameters, command, .. } => {
                for (param_name, param_type) in parameters {
                    let param_label = format!("Param: {}", param_name);
                    let param_node = graph.add_node(param_label);
                    graph.add_edge(current, param_node, ());
                    build_graph(param_type, graph, Some(param_node));
                }
                build_graph(command, graph, Some(current));
            }
            ASTNode::FuncDeclaration { parameters, return_type, body, .. } => {
                for (param_name, param_type) in parameters {
                    let param_label = format!("Param: {}", param_name);
                    let param_node = graph.add_node(param_label);
                    graph.add_edge(current, param_node, ());
                    build_graph(param_type, graph, Some(param_node));
                }
                build_graph(return_type, graph, Some(current));
                build_graph(body, graph, Some(current));
            }
            ASTNode::IfCommand { condition, then_command, else_command } => {
                build_graph(condition, graph, Some(current));
                build_graph(then_command, graph, Some(current));
                if let Some(else_cmd) = else_command {
                    build_graph(else_cmd, graph, Some(current));
                }
            }
            ASTNode::WhileCommand { condition, command } => {
                build_graph(condition, graph, Some(current));
                build_graph(command, graph, Some(current));
            }
            ASTNode::CallCommand { actual_parameters, .. } => {
                for param in actual_parameters {
                    build_graph(param, graph, Some(current));
                }
            }
            ASTNode::RecordAggregate(fields) => {
                for (name, expr) in fields {
                    let field_label = format!("Field: {}", name);
                    let field_node = graph.add_node(field_label);
                    graph.add_edge(current, field_node, ());
                    build_graph(expr, graph, Some(field_node));
                }
            }
            ASTNode::ArrayAggregate(elements) => {
                for elem in elements {
                    build_graph(elem, graph, Some(current));
                }
            }
            ASTNode::TypeDenoter(_) => {}
            ASTNode::ArrayType { size, element_type } => {
                build_graph(size, graph, Some(current));
                build_graph(element_type, graph, Some(current));
            }
            ASTNode::RecordType(fields) => {
                for (name, field_type) in fields {
                    let field_label = format!("Field: {}", name);
                    let field_node = graph.add_node(field_label);
                    graph.add_edge(current, field_node, ());
                    build_graph(field_type, graph, Some(field_node));
                }
            }
            ASTNode::DotVName { vname, .. } => {
                build_graph(vname, graph, Some(current));
            }
            ASTNode::SubscriptVName { vname, expression } => {
                build_graph(vname, graph, Some(current));
                build_graph(expression, graph, Some(current));
            }
            ASTNode::BinaryExpression { left, right, .. } => {
                build_graph(left, graph, Some(current));
                build_graph(right, graph, Some(current));
            }
            ASTNode::UnaryExpression { operand, .. } => {
                build_graph(operand, graph, Some(current));
            }
            ASTNode::IntegerLiteral(_) => {}
            ASTNode::CharacterLiteral(_) => {}
            ASTNode::Operator(_) => {}
            ASTNode::Empty => {}
        }

        current
    }

    build_graph(&ast, &mut graph, None);

    let dot = Dot::with_config(&graph, &[Config::EdgeNoLabel]);

    let output_path = Path::new(&output_filename);
    let mut output_file = File::create(output_path).expect("Error al crear el archivo de salida.");
    write!(output_file, "{:?}", dot).expect("Error al escribir en el archivo de salida.");

    println!("Archivo DOT generado: {}", output_path.display());
    println!("Puede usar Graphviz para visualizar el árbol:");
    println!("dot -Tpng {} -o Outputs/tree.png", output_path.display());
}
