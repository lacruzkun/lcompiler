use clap::Parser;
use regex::Regex;
use std::error::Error;
use std::{
    fs,
    path::PathBuf,
    process::{self, Command},
};

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

fn main() -> Result<(), Box<dyn Error>> {
    // assume Args::parse() comes from clap or similar
    let mut arg = Args::parse();
    let input_path = PathBuf::from(arg.filename.expect("pass filename as argument"));

    // compute filenames in a robust way (file.c -> file.i, file.s, output -> file)
    let processed_path = input_path.with_extension("i");
    let assembly_path = input_path.with_extension("s");
    let output_path = input_path.with_extension("");
    let output_path = PathBuf::from(output_path);

    // Preprocess: gcc -E -P <input> -o <processed>
    let status = Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(&input_path)
        .arg("-o")
        .arg(&processed_path)
        .status()?;
    if !status.success() {
        return Err(format!("gcc preprocessing failed: {}", status).into());
    }

    // Read preprocessed input
    let input = fs::read_to_string(&processed_path)?;
    let input = input.as_str();

    // preserve the original shorthand semantics:
    if arg.codegen {
        arg.parse = true;
    }
    if arg.parse {
        arg.lex = true;
    }

    // Execute phases according to flags.
    // We'll only write assembly and run final gcc when codegen is requested (safer).
    let maybe_assembly: Option<String> = if arg.lex || arg.parse || arg.codegen {
        // lex_phase takes &str in your original code
        let lex_result = lex_phase(input);

        // parse if requested (parse_phase consumes the lex_result in your code)
        let parse_result = if arg.parse || arg.codegen {
            parse_phase(lex_result)
        } else {
            // If parse not requested we still call parse_phase for downstream compatibility,
            // but this branch should not normally be reachable because flags were forced above.
            parse_phase(lex_result)
        };

        // codegen only if requested
        if arg.codegen {
            Some(codegen_phase(parse_result))
        } else {
            None
        }
    } else {
        let lex_result = lex_phase(input);
        let parse_result = { parse_phase(lex_result) };
        Some(codegen_phase(parse_result))
    };

    // If we produced assembly, write it and run gcc to link/build the final binary
    if let Some(assembly_text) = maybe_assembly {
        fs::write(&assembly_path, &assembly_text)?;

        let status = Command::new("gcc")
            .arg(&assembly_path)
            .arg("-o")
            .arg(&output_path)
            .status()?;
        if !status.success() {
            return Err(format!("gcc assembly/link failed: {}", status).into());
        }
    } else {
        // no codegen requested — nothing to compile
        println!("No codegen requested; skipping assembly and linking.");
    }

    Ok(())
}

fn lex_phase(input: &str) -> Vec<Token> {
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
    lex_result
}

fn parse_phase(input: Vec<Token>) -> AstProg {
    let parse_r = parse_ast(input);
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
    parse_result
}

fn codegen_phase(input: AstProg) -> String {
    let codegen_r = parse_asm(input);
    let output = emit_code_prog(codegen_r);
    output
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

// Asm ast generation
fn parse_asm(input: AstProg) -> AsmProg {
    match input {
        AstProg::Program(ast_program) => AsmProg::Program(parse_asm_func(ast_program)),
    }
}

fn parse_asm_func(input: AstFuncDef) -> AsmFuncDef {
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

fn emit_code_prog(input: AsmProg) -> String {
    let mut output = String::new();
    match input {
        AsmProg::Program(program) => output.push_str(emit_code_func(program).as_str()),
    }
    output.push_str(format!("{:4}", "").as_str());
    output.push_str(".section .note.GNU-stack,\"\",@progbits\n");
    output
}

fn emit_code_func(input: AsmFuncDef) -> String {
    let mut output = String::new();
    output.push_str(format!("{:4}", "").as_str());
    match input {
        AsmFuncDef::Function { name, instructions } => output.push_str(
            format!(".global {name}\n{name}:\n{}", emit_code_inst(instructions)).as_str(),
        ),
    }
    output
}

fn emit_code_inst(input: Vec<AsmInstruction>) -> String {
    let mut output = String::new();
    for inst in input {
        output.push_str(format!("{:4}", "").as_str());
        match inst {
            AsmInstruction::Mov { src, dst } => output.push_str(
                format!("movl {}, {}\n", emit_code_ope(src), emit_code_ope(dst)).as_str(),
            ),
            AsmInstruction::Ret => output.push_str("ret\n"),
        }
    }
    output
}

fn emit_code_ope(input: AsmOperand) -> String {
    let mut output = String::new();
    match input {
        AsmOperand::Imm(int) => output.push_str(format!("${int}").as_str()),
        AsmOperand::Register => output.push_str("%eax"),
    }
    output
}
