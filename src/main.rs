use clap::Parser;
use regex::Regex;
use std::{fs, process};

#[derive(Debug, PartialEq, Eq)]
enum Token {
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    SemiColon,
    Int,
    Return,
    Void,
    Identifier(String),
    IntLiteral(i32),
}

// Data structures for the AST
#[derive(Debug, Clone)]
enum AstExp {
    Constant(i32),
}

#[derive(Debug, Clone)]
enum AstStatement {
    Return(AstExp),
}

#[derive(Debug, Clone)]
enum AstFuncDef {
    Function { name: String, body: AstStatement },
}

#[derive(Debug, Clone)]
enum AstProg {
    Program(AstFuncDef),
}

// Data Structures for the assembly code
#[derive(Debug, Clone)]
enum AsmOperand {
    Imm(i32),
    Register,
}

#[derive(Debug, Clone)]
enum AsmInstruction {
    Mov { src: AsmOperand, dst: AsmOperand },
    Ret,
}

#[derive(Debug, Clone)]
enum AsmFuncDef {
    Function {
        name: String,
        instructions: Vec<AsmInstruction>,
    },
}

#[derive(Debug, Clone)]
enum AsmProg {
    Program(AsmFuncDef),
}

#[derive(Debug)]
enum LexError {
    UnknownToken(String),
}

#[derive(Debug)]
enum ParseError {
    SyntaxError(String),
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::UnknownToken(tok) => write!(f, "Unknown Token {}", tok),
        }
    }
}

impl std::error::Error for LexError {}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::SyntaxError(tok) => write!(f, "SyntaxError {}", tok),
        }
    }
}

impl std::error::Error for ParseError {}

fn consume<'a>(
    input: &'a str,
    m: regex::Match<'a>,
) -> Result<(&'a str, regex::Match<'a>), LexError> {
    if m.start() == 0 {
        Ok((&input[m.end()..], m))
    } else {
        Err(LexError::UnknownToken(input[0..m.start()].to_string()))
    }
}

#[derive(Parser, Debug)]
struct Args {
    #[arg(short, long)]
    lex: bool,
    #[arg(short, long)]
    parse: bool,
    #[arg(short, long)]
    codegen: bool,
    filename: Option<String>,
}

fn main() {
    let mut arg = Args::parse();
    let input = fs::read_to_string(arg.filename.expect("pass filename as argument"))
        .expect("file should exist");
    let input = input.as_str();
    if arg.codegen {
        arg.parse = true;
    }
    if arg.parse {
        arg.lex = true;
    }

    if arg.lex {
        let lex_r = lex(input);
        let lex_result = match lex_r {
            Ok(x) => {
                println!("{:?}", x);
                x
            }
            Err(e) => {
                println!("{e}");
                process::exit(1)
            }
        };
        if arg.parse {
            let parse_r = parse_ast(lex_result);
            let parse_result = match parse_r {
                Ok(x) => {
                    println!("{:?}", x);
                    x
                }
                Err(e) => {
                    println!("{e}");
                    process::exit(1)
                }
            };
            if arg.codegen {
                let codegen_r = parse_asm(parse_result);
                println!("{:?}", codegen_r);
            }
        }
    }
}

fn lex(mut input: &str) -> Result<Vec<Token>, LexError> {
    let re = Regex::new(
        r"(?P<lparen>\()|(?P<rparen>\))|(?P<rbrace>\})|(?P<lbrace>\{)|(?P<semicolon>;)|(?P<iden>[a-zA-Z_]\w*\b)|(?P<intliteral>[0-9]+\b)|(?P<comment>//.*)|(?P<longcomment>/\*[\s\S]*\*/)",
    )
    .unwrap();
    let mut output = vec![];

    println!("{input}");

    while !input.is_empty() {
        input = input.trim();

        let cap = match re.captures(&input) {
            Some(c) => c,
            None => return Err(LexError::UnknownToken(input.to_string())),
        };

        if let Some(m) = cap.name("lparen") {
            let (rest, _) = consume(input, m)?;
            input = rest;
            output.push(Token::Lparen);
        } else if let Some(m) = cap.name("rparen") {
            let (rest, _) = consume(input, m)?;
            input = rest;
            output.push(Token::Rparen);
        } else if let Some(m) = cap.name("lbrace") {
            let (rest, _) = consume(input, m)?;
            input = rest;
            output.push(Token::Lbrace);
        } else if let Some(m) = cap.name("rbrace") {
            let (rest, _) = consume(input, m)?;
            input = rest;
            output.push(Token::Rbrace);
        } else if let Some(m) = cap.name("semicolon") {
            let (rest, _) = consume(input, m)?;
            input = rest;
            output.push(Token::SemiColon);
        } else if let Some(m) = cap.name("iden") {
            let (rest, m) = consume(input, m)?;
            input = rest;
            match m.as_str() {
                "int" => output.push(Token::Int),
                "void" => output.push(Token::Void),
                "return" => output.push(Token::Return),
                x => output.push(Token::Identifier(x.to_string())),
            }
        } else if let Some(m) = cap.name("intliteral") {
            let (rest, m) = consume(input, m)?;
            input = rest;
            output.push(Token::IntLiteral(
                m.as_str()
                    .parse()
                    .expect("should be abled to convert to integer"),
            ));
        } else if let Some(m) = cap.name("comment") {
            let (rest, _) = consume(input, m)?;
            input = rest;
        } else if let Some(m) = cap.name("longcomment") {
            let (rest, _) = consume(input, m)?;
            input = rest;
        } else {
            return Err(LexError::UnknownToken(input.to_string()));
        }
    }
    Ok(output)
}

fn parse_ast(mut input: Vec<Token>) -> Result<AstProg, ParseError> {
    let program = parse_func(&mut input)?;
    Ok(AstProg::Program(program))
}

fn parse_asm(input: AstProg) -> AsmProg {
    match input {
        AstProg::Program(ast_program) => AsmProg::Program(parse_asm_func(ast_program)),
    }
}

fn parse_asm_func(mut input: AstFuncDef) -> AsmFuncDef {
    match input {
        AstFuncDef::Function { name, body } => {
            let asm_name = parse_asm_name(name);
            let asm_instructions = parse_asm_instruction(body);
            AsmFuncDef::Function {
                name: asm_name,
                instructions: asm_instructions,
            }
        }
    }
}

fn parse_asm_name(input: String) -> String {
    input
}

fn parse_asm_instruction(input: AstStatement) -> Vec<AsmInstruction> {
    match input {
        AstStatement::Return(exp) => {
            let mut inst = vec![];
            let asm_exp = parse_asm_exp(exp);
            inst.push(AsmInstruction::Mov {
                src: asm_exp,
                dst: AsmOperand::Register,
            });
            inst.push(AsmInstruction::Ret);
            inst
        }
    }
}

fn parse_asm_exp(input: AstExp) -> AsmOperand {
    match input {
        AstExp::Constant(x) => AsmOperand::Imm(x),
    }
}

fn parse_func(mut input: &mut Vec<Token>) -> Result<AstFuncDef, ParseError> {
    expect(Token::Int, &mut input)?;
    let identifier = parse_identifier(&mut input)?;
    expect(Token::Lparen, &mut input)?;
    expect(Token::Void, &mut input)?;
    expect(Token::Rparen, &mut input)?;
    expect(Token::Lbrace, &mut input)?;
    let statement = parse_statement(&mut input)?;
    expect(Token::Rbrace, &mut input)?;

    if !input.is_empty() {
        return Err(ParseError::SyntaxError(
            "extra code outside function".to_string(),
        ));
    }

    Ok(AstFuncDef::Function {
        name: identifier,
        body: statement,
    })
}

fn parse_identifier(input: &mut Vec<Token>) -> Result<String, ParseError> {
    let token = take_token(input)?;

    match token {
        Token::Identifier(iden) => Ok(iden),
        _ => Err(ParseError::SyntaxError(
            "invalid token: token not an identifier".to_string(),
        )),
    }
}

fn parse_statement(mut input: &mut Vec<Token>) -> Result<AstStatement, ParseError> {
    expect(Token::Return, &mut input)?;
    let exp = parse_exp(&mut input)?;
    expect(Token::SemiColon, &mut input)?;
    Ok(AstStatement::Return(exp))
}

fn parse_exp(mut input: &mut Vec<Token>) -> Result<AstExp, ParseError> {
    let int = parse_int(&mut input)?;
    Ok(AstExp::Constant(int))
}

fn parse_int(input: &mut Vec<Token>) -> Result<i32, ParseError> {
    let token = take_token(input)?;
    match token {
        Token::IntLiteral(int) => Ok(int),
        _ => Err(ParseError::SyntaxError(
            "invalid token: token not an exp".to_string(),
        )),
    }
}

fn expect(expected: Token, mut input: &mut Vec<Token>) -> Result<(), ParseError> {
    let actual = take_token(&mut input)?;
    if actual != expected {
        return Err(ParseError::SyntaxError(
            format! {"Syntax Error: expected {:?}, saw {:?}", expected, actual},
        ));
    }
    Ok(())
}

fn take_token(input: &mut Vec<Token>) -> Result<Token, ParseError> {
    if input.is_empty() {
        return Err(ParseError::SyntaxError(
            "invalid identifier: end of file".to_string(),
        ));
    }
    Ok(input.remove(0))
}
