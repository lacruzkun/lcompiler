use clap::Parser;
use regex::Regex;
use std::{error::Error, fs, process};

#[derive(Debug)]
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
// fn main() {
//     let re = Regex::new(r"Hello (?<name>\w+)!").unwrap();
//     let Some(caps) = re.captures("Hello Murphy! Hello Cappy!") else {
//         println!("no match!");
//         return;
//     };
//     println!("The name is: {}", &caps["name"]);
// }

fn main() {
    let arg = Args::parse();
    let input = fs::read_to_string(arg.filename.expect("pass filename as argument"))
        .expect("file should exist");
    if arg.lex {
        match lex(input.clone()) {
            Ok(x) => println!("{:?}", x),
            Err(_) => process::exit(1),
        }
    }
    // } else if arg.parse {
    //     lex(input.clone());
    //     parse(input.clone());
    // } else if arg.codegen {
    //     lex(input.clone());
    //     parse(input.clone());
    //     codegen(input.clone());
    // }
}

fn lex(mut input: String) -> Result<Vec<Token>, Box<dyn Error>> {
    let re = Regex::new(
        r"(?P<lparen>\()|(?P<rparen>\))|(?P<rbrace>\})|(?P<lbrace>\{)|(?P<semicolon>;)|(?P<iden>[a-zA-Z_]\w*\b)|(?P<intliteral>[0-9]+\b)|(?P<comment>\//)|(?P<longcomment>/\*[\s\S]*\*/)",
    )
    .unwrap();
    let mut output = vec![];

    println!("{input}");

    while !input.is_empty() {
        input = input.trim().to_string();

        if let Some(cap) = re.captures(&input) {
            if let Some(m) = cap.name("lparen") {
                if m.start() == 0 {
                    input = input[m.end()..].to_string();
                    output.push(Token::Lparen);
                } else {
                    println!(
                        "Encountered an unknowned token {}",
                        input[0..m.start()].to_string()
                    );
                    return Err("Unknown token".into());
                }
            } else if let Some(m) = cap.name("rparen") {
                if m.start() == 0 {
                    output.push(Token::Rparen);
                    input = input[m.end()..].to_string();
                } else {
                    println!(
                        "Encountered an unknowned token {}",
                        input[0..m.start()].to_string()
                    );
                    return Err("Unknown token".into());
                }
            } else if let Some(m) = cap.name("lbrace") {
                if m.start() == 0 {
                    output.push(Token::Lbrace);
                    input = input[m.end()..].to_string();
                } else {
                    println!(
                        "Encountered an unknowned token {}",
                        input[0..m.start()].to_string()
                    );
                    return Err("Unknown token".into());
                }
            } else if let Some(m) = cap.name("rbrace") {
                if m.start() == 0 {
                    output.push(Token::Rbrace);
                    input = input[m.end()..].to_string();
                } else {
                    println!(
                        "Encountered an unknowned token {}",
                        input[0..m.start()].to_string()
                    );
                    return Err("Unknown token".into());
                }
            } else if let Some(m) = cap.name("semicolon") {
                if m.start() == 0 {
                    output.push(Token::SemiColon);
                    input = input[m.end()..].to_string();
                } else {
                    println!(
                        "Encountered an unknowned token {}",
                        input[0..m.start()].to_string()
                    );
                    return Err("Unknown token".into());
                }
            } else if let Some(m) = cap.name("iden") {
                if m.start() == 0 {
                    match m.as_str() {
                        "int" => output.push(Token::Int),
                        "void" => output.push(Token::Void),
                        "return" => output.push(Token::Return),
                        x => output.push(Token::Identifier(x.to_string())),
                    }
                    input = input[m.end()..].to_string();
                } else {
                    println!(
                        "Encountered an unknowned token {}",
                        input[0..m.start()].to_string()
                    );
                    return Err("Unknown token".into());
                }
            } else if let Some(m) = cap.name("intliteral") {
                if m.start() == 0 {
                    output.push(Token::IntLiteral(
                        m.as_str()
                            .parse()
                            .expect("should be abled to convert to integer"),
                    ));
                    input = input[m.end()..].to_string();
                } else {
                    println!(
                        "Encountered an unknowned token {}",
                        input[0..m.start()].to_string()
                    );
                    return Err("Unknown token".into());
                }
            } else if let Some(m) = cap.name("comment") {
                if m.start() == 0 {
                    println!("comment");
                    input = input[input.find('\n').unwrap_or(1) + 1..].to_string();
                } else {
                    println!(
                        "Encountered an unknowned token {}",
                        input[0..m.start()].to_string()
                    );
                    return Err("Unknown token".into());
                }
            } else if let Some(m) = cap.name("longcomment") {
                println!("{:?}", m);
                if m.start() == 0 {
                    println!("longcomment");
                    input = input[input.find('\n').unwrap_or(1) + 1..].to_string();
                }
            } else {
                println!("Encountered an unknowned token first else {}", input);
                return Err("Unknown token".into());
            }
        } else {
            println!("Encountered an unknowned token, {}", input);
            return Err("Unknown token".into());
        }
    }
    Ok(output)
}

// fn parse(mut _input: String) -> Result<(), Box<dyn Error>> {
//     Ok(())
// }
// fn codegen(mut _input: String) -> Result<(), Box<dyn Error>> {
//     Ok(())
// }
