use std::fs;

#[derive(Debug)]
enum Token{
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    SemiColon,
    Int,
    Return,
    Identifier,
    IntLiteral
}

fn main() {
    let argv: Vec<String> = std::env::args().collect();
    println!("{:?}", argv[1]);
    lex(argv[1].clone()).unwrap();
}

fn lex(file_name: String) -> Result<Vec<Token>, String>{
    let content = fs::read_to_string(file_name).map_err(|e| e.to_string())?;

    let mut buf = String::new();
    let mut list = Vec::new();
    let mut token_list = Vec::new();
    let mut previous_ch: char = 0 as char;

    let mut first_pass = true;

    // split the file by text
    for ch in content.chars(){
        if ch.is_alphabetic(){
            if previous_ch.is_ascii_punctuation(){
                buf.clear();
            }
            else if !previous_ch.is_alphabetic() && !first_pass && !buf.is_empty(){
                list.push(buf.clone());
                buf.clear();
            }
            buf.push(ch);
            first_pass = false;
        }
        else if ch.is_numeric(){
            if !previous_ch.is_numeric(){
                list.push(buf.clone());
                buf.clear();
            }
            buf.push(ch);
        }
        else if ch.is_ascii_punctuation(){
            if !buf.is_empty(){
                if !previous_ch.is_ascii_punctuation(){
                    list.push(buf.clone());
                }
            }
            buf.clear();
            buf.push(ch);
            list.push(buf.clone());
        }
        else if ch.is_whitespace(){
            if previous_ch.is_ascii_punctuation(){
                continue;
            }
            if !buf.is_empty(){
                list.push(buf.clone());
                buf.clear();
            }
            continue;
        }
        previous_ch = ch;

    }
    println!("{:?}", list);

    // tokenize the text
    for token in list {
        if token == "("{
            token_list.push(Token::OpenParen);
        }
        else if token == ")"{
            token_list.push(Token::CloseParen);
        }
        else if token == "{"{
            token_list.push(Token::OpenBrace);
        }
        else if token == "}"{
            token_list.push(Token::CloseBrace);
        }

        else if token == ";"{
            token_list.push(Token::SemiColon);
        }
        else if token == "int"{
            token_list.push(Token::Int);
        }
        else if token == "return"{
            token_list.push(Token::Return);
        }
        else if token.chars().all(|c| c.is_ascii_digit()){
            token_list.push(Token::IntLiteral);
        }
        else {
            token_list.push(Token::Identifier);
        }
    }

    println!("{:?}", token_list);


    return Ok(token_list);

}
