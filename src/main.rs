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
#[derive(Debug, Clone)]
enum Exp {
    Constant(i32),
}

#[derive(Debug, Clone)]
enum Statement {
    Return(Exp),
}

// #[derive(Debug, Clone)]
// enum Identifier {
//     String,
// }

#[derive(Debug, Clone)]
enum FuncDef {
    Function { name: String, body: Statement },
}

#[derive(Debug, Clone)]
enum Prog {
    Program(FuncDef),
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
    if arg.codegen || arg.parse {
        arg.parse = true;
        if arg.parse {
            arg.lex = true;
        }
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
            let parse_r = parse(lex_result);
            let _parse_resutl = match parse_r {
                Ok(x) => {
                    println!("{:?}", x);
                    x
                }
                Err(e) => {
                    println!("{e}");
                    process::exit(1)
                }
            };
        }
    }
    // } else if arg.codegen {
    //     lex(input.clone());
    //     parse(input.clone());
    //     codegen(input.clone());
    // }
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

fn parse(mut input: Vec<Token>) -> Result<Prog, ParseError> {
    let program = parse_func(&mut input)?;
    Ok(Prog::Program(program))
}

fn parse_func(mut input: &mut Vec<Token>) -> Result<FuncDef, ParseError> {
    expect(Token::Int, &mut input)?;
    let identifier = parse_identifier(&mut input)?;
    expect(Token::Lparen, &mut input)?;
    expect(Token::Void, &mut input)?;
    expect(Token::Rparen, &mut input)?;
    expect(Token::Lbrace, &mut input)?;
    let statement = parse_statement(&mut input)?;
    expect(Token::Rbrace, &mut input)?;
    Ok(FuncDef::Function {
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

fn parse_statement(mut input: &mut Vec<Token>) -> Result<Statement, ParseError> {
    expect(Token::Return, &mut input)?;
    let exp = parse_exp(&mut input)?;
    expect(Token::SemiColon, &mut input)?;
    Ok(Statement::Return(exp))
}

fn parse_exp(mut input: &mut Vec<Token>) -> Result<Exp, ParseError> {
    let int = parse_int(&mut input)?;
    Ok(Exp::Constant(int))
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
// fn codegen(mut _input: String) -> Result<(), Box<dyn Error>> {
//     Ok(())
// }
