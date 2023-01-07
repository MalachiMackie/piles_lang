use crate::{routines::{Routine, IntrinsicRoutine, RoutineSigniture}, Token, Type, Value, Block};
use std::str::Chars;

struct Parser<I : Iterator<Item = char>> {
    chars: I,
    next_token: usize,
    block_stack: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq)]
enum TokenType {
    ConstantI32,
    ConstantChar,
    ConstantString,
    ConstantBool,
    Routine,
    OpenBlock,
    CloseBlock,
}

#[derive(Debug, PartialEq)]
pub(crate) enum ParseError {
    CharMissingOpeningTick,
    CharMissingClosingTick,
    CharMissingChar,
    CharExtraCharacters,
    StringMissingOpeningQuote,
    StringNoStringContent,
    StringMissingClosingQuote,
    BoolInvalid,
    ParseIntError,
    InvalidRoutine,
    MissingOpenBlock,
}

impl<'a> Parser<Chars<'a>> {
    fn new(input: &'a str) -> Self {
        let chars = input.chars();
        Self {
            chars,
            next_token: 0,
            block_stack: Vec::new(),
        }
    }

    fn next(&mut self) -> Option<Result<Token, ParseError>> {
        let mut current_part = Vec::new();
        // todo: add all values
        let mut possible_token_types = vec![
            TokenType::ConstantChar,
            TokenType::ConstantString,
            TokenType::ConstantI32,
            TokenType::ConstantBool,
            TokenType::Routine,
            TokenType::OpenBlock,
            TokenType::CloseBlock,
        ];
        let mut token_type = None;
        let mut delimeter: &dyn Fn(char) -> bool = &char::is_whitespace;
        while let Some(next_char) = self.chars.next() {
            current_part.push(next_char);
            if delimeter(next_char) {
                if current_part.iter().collect::<String>().trim().is_empty() {
                    continue;
                }
                break;
            }
 
            if token_type.is_none() {
                let current_str: String = current_part.iter().collect();
                evaluate_possible_tokens(&current_str.trim(), &mut possible_token_types);
            }
            if possible_token_types.len() == 1 && token_type.is_none() {
                token_type = Some(possible_token_types[0].clone());
                if let Some(TokenType::ConstantString) = token_type {
                    delimeter = &|c: char| c == '"';
                }
            }
        }
        let string_value: String = current_part.into_iter().collect();
        let Some(token_type) = token_type else {
            return None;
        };
        let token_number = self.next_token;
        self.next_token += 1;
        match token_type {
            TokenType::ConstantChar => Some(parse_char(string_value.trim(), token_number)),
            TokenType::ConstantString => Some(parse_string(string_value.trim(), token_number)),
            TokenType::ConstantI32 => Some(string_value.trim().parse().map(|i32_value| Token::Constant(token_number, Value::I32(i32_value))).map_err(|e| ParseError::ParseIntError)),
            TokenType::ConstantBool => Some(parse_bool(string_value.trim(), token_number)),
            TokenType::OpenBlock => {
                self.block_stack.push(token_number);
                Some(Ok(Token::Block(token_number, Block::Open)))
            },
            TokenType::CloseBlock => {
                if let Some(open_position) = self.block_stack.pop() {
                    Some(Ok(Token::Block(token_number, Block::Close { open_position })))
                } else {
                    Some(Err(ParseError::MissingOpenBlock))
                }
            },
            TokenType::Routine => Some(parse_routine(string_value.trim(), token_number)),
        }
    }
}

fn evaluate_possible_tokens(value: &str, current_possibilities: &mut Vec<TokenType>) {
    if !value.starts_with("'") {
        if let Some(found_index) = current_possibilities.iter().position(|p| p == &TokenType::ConstantChar) {
            current_possibilities.remove(found_index);
        }
    }
    if !value.starts_with("\"") {
        if let Some(found_index) = current_possibilities.iter().position(|p| p == &TokenType::ConstantString) {
            current_possibilities.remove(found_index);
        }
    }
    if !value.starts_with("!") {
        if let Some(found_index) = current_possibilities.iter().position(|p| p == &TokenType::Routine) {
            current_possibilities.remove(found_index);
        }
    }
    if value.find(|c: char| c == '.' || !(c.is_numeric() || c == '-')).is_some() {
        if let Some(found_index) = current_possibilities.iter().position(|p| p == &TokenType::ConstantI32) {
            current_possibilities.remove(found_index);
        }
    }
    if value != "{" {
        if let Some(found_index) = current_possibilities.iter().position(|p| p == &TokenType::OpenBlock) {
            current_possibilities.remove(found_index);
        }
    }
    if value != "}" {
        if let Some(found_index) = current_possibilities.iter().position(|p| p == &TokenType::CloseBlock) {
            current_possibilities.remove(found_index);
        }
    }
    const true_str: &str = "true";
    const false_str: &str = "false";
    if let Some(found_index) = current_possibilities.iter().position(|p| p == &TokenType::ConstantBool) {
        let mut true_chars = true_str.chars();
        let mut false_chars = false_str.chars();
        for (index, char_value) in value.chars().enumerate() {
            let true_char = true_chars.next();
            let false_char = false_chars.next();
            let is_invalid = match (true_char, false_char) {
                (None, None) => true,
                (Some(true_char), None) => true_char != char_value,
                (None, Some(false_char)) => false_char != char_value,
                (Some(true_char), Some(false_char)) => true_char != char_value && false_char != char_value,
            };
            if is_invalid {
                current_possibilities.remove(found_index);
                break;
            }
        }
    }

    if value.find(|c: char| !c.is_numeric() && c != '.').is_some() {
        // todo: floats
    }
}

fn parse_routine(value: &str, token_number: usize) -> Result<Token, ParseError> {
    // todo: add compile type checks
    match value {
        "!add" => Ok(Token::Routine(token_number, Routine::Intrinsic{signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::AddI32), routine: IntrinsicRoutine::AddI32})),
        "!minus" => Ok(Token::Routine(token_number, Routine::Intrinsic{signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::MinusI32), routine: IntrinsicRoutine::MinusI32})),
        "!printc" => Ok(Token::Routine(token_number, Routine::Intrinsic{signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::PrintChar), routine: IntrinsicRoutine::PrintChar})),
        "!prints" => Ok(Token::Routine(token_number, Routine::Intrinsic{ signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::PrintString), routine: IntrinsicRoutine::PrintString})),
        _ => Err(ParseError::InvalidRoutine),
    }
}

fn parse_char(value: &str, token_number: usize) -> Result<Token, ParseError> {
    if !value.starts_with('\'') {
        return Err(ParseError::CharMissingOpeningTick);
    } else if !value.ends_with('\'') || value.len() == 1 {
        return Err(ParseError::CharMissingClosingTick);
    };
    let middle_chars: Vec<_> = value.chars().skip(1).collect();
    // todo: allow escaping
    match (middle_chars.len(), middle_chars.first()) {
        (1, _) => Err(ParseError::CharMissingChar),
        (2, Some(char_value)) => Ok(Token::Constant(token_number, Value::Char(*char_value))),
        _ => Err(ParseError::CharExtraCharacters),
    }
}

fn parse_string(value: &str, token_number: usize) -> Result<Token, ParseError> {
    println!("{}", value);
    if !value.starts_with('"') {
        return Err(ParseError::StringMissingOpeningQuote);
    } else if !value.ends_with('"') || value.len() == 1 {
        return Err(ParseError::StringMissingClosingQuote);
    }
    let mut chars = value.chars().skip(1);
    let rest: Vec<_> = chars.collect();
    let Some(last_char) = rest.last() else {
        return Err(ParseError::StringNoStringContent);
    };
    if last_char != &'"' {
        return Err(ParseError::StringMissingClosingQuote);
    }
    let len = rest.len();
    Ok(Token::Constant(token_number, Value::String(rest.into_iter().take(len - 1).collect::<String>())))
}

fn parse_bool(value: &str, token_number: usize) -> Result<Token, ParseError> {
    match value {
        "true" => Ok(Token::Constant(token_number, Value::Bool(true))),
        "false" => Ok(Token::Constant(token_number, Value::Bool(false))),
        _ => Err(ParseError::BoolInvalid),
    }
}

pub(crate) fn parse_input(input: &str) -> Result<Box<[Token]>, ParseError> {
    let mut parser = Parser::new(input);
    let mut tokens = Vec::new();

    while let Some(result) = parser.next() {
        let token = result?;
        tokens.push(token);
    }
    
    Ok(tokens.into_boxed_slice())
}

#[cfg(test)]
mod tests {
    use super::*;
    fn test_input(description: &str, input: &str, expected_tokens: &[Token]) -> Result<(), String> {
        let parsed = parse_input(input).map_err(|e| format!(r#"test "{}" failed: {:?}"#, description, e))?;
        if parsed.as_ref() != expected_tokens {
            Err(format!("Expected {:?}, found {:?}", expected_tokens, parsed))
        } else {
            Ok(())
        }
    }

    #[test]
    fn test_parsing() {
        let results = vec![
            test_input("constant i32 pushing",
                       "10 5",
                       &vec![Token::Constant(0, Value::I32(10)), Token::Constant(1, Value::I32(5))]),
            test_input("routine",
                       "!add",
                       &vec![Token::Routine(0, Routine::Intrinsic {
                            signiture: RoutineSigniture::new("add",
                                                            &vec![Type::I32, Type::I32],
                                                            &vec![Type::I32]),
                            routine: IntrinsicRoutine::AddI32
                       })]),
            test_input("negative number",
                       "-124",
                       &vec![
                            Token::Constant(0, Value::I32(-124)),
                       ]),
            test_input("constants and routine",
                       "10 5 !minus 15",
                       &vec![
                            Token::Constant(0, Value::I32(10)),
                            Token::Constant(1, Value::I32(5)),
                            Token::Routine(2, Routine::Intrinsic {
                                signiture: RoutineSigniture::new("minus",
                                                            &vec![Type::I32, Type::I32],
                                                            &vec![Type::I32]),
                                routine: IntrinsicRoutine::MinusI32
                            }),
                            Token::Constant(3, Value::I32(15)),
                       ]),
            test_input("char",
                       "'a' '$'",
                       &vec![
                            Token::Constant(0, Value::Char('a')),
                            Token::Constant(1, Value::Char('$'))
                       ]),
            test_input("bool",
                       "true false",
                       &vec![
                            Token::Constant(0, Value::Bool(true)),
                            Token::Constant(1, Value::Bool(false)),
                       ]),
            test_input("empty string",
                       r#""""#,
                       &vec![
                            Token::Constant(0, Value::String(String::new()))
                       ]),
            test_input("string with space",
                       r#""some string""#,
                       &vec![
                            Token::Constant(0, Value::String("some string".to_owned())),
                       ]),
            test_input("string then routine",
                       r#""Hello World!" !prints"#,
                       &vec![
                            Token::Constant(0, Value::String("Hello World!".to_owned())),
                            Token::Routine(1, Routine::Intrinsic {
                                signiture: RoutineSigniture::new("prints",
                                                                 &vec![Type::String],
                                                                 &Vec::new()),
                                routine: IntrinsicRoutine::PrintString,
                            }),
                       ]),
            test_input("block",
                       "{ }",
                       &vec![
                            Token::Block(0, Block::Open),
                            Token::Block(1, Block::Close { open_position: 0 }),
                       ]),
            test_input("nested block",
                       "{ { } }",
                       &vec![
                            Token::Block(0, Block::Open),
                            Token::Block(1, Block::Open),
                            Token::Block(2, Block::Close { open_position: 1 }),
                            Token::Block(3, Block::Close { open_position: 0 }),
                       ]),
        ];

        let result: Result<Vec<_>, _> = results.into_iter().collect();
        if let Err(err) = result {
            println!("{}", err);
            panic!();
        }
    }

    fn test_invalid_input(description: &str, input: &str, expected_error: ParseError) -> Result<(), String> {
        let parsed = parse_input(input);
        match parsed {
            Ok(tokens) => Err(format!("Input: \"{}\". Expected parse failure {:?}, but found successful: {:?}", input, expected_error, tokens)),
            Err(err) if err != expected_error => Err(format!("Input: \"{}\". Expected parse failure {:?}, but found parse failure {:?}", input, expected_error, err)),
            Err(err) => Ok(()),
        }
    }

    #[test]
    fn invalid_tokens() {
        let results = [
            test_invalid_input(
                "char missing closing tick",
                "'a",
                ParseError::CharMissingClosingTick),
            test_invalid_input(
                "char missing character",
                "''",
                ParseError::CharMissingChar),
            test_invalid_input(
                "char only opening tick",
                "'",
                ParseError::CharMissingClosingTick),
            test_invalid_input(
                "char extra characters",
                "'abc'",
                ParseError::CharExtraCharacters),
            test_invalid_input(
                "char extra characters after tick",
                "'a'abc",
                ParseError::CharMissingClosingTick),
            test_invalid_input(
                "i32 with number",
                "1a",
                ParseError::ParseIntError),
            test_invalid_input(
                "string missing closing quote",
                r#""something"#,
                ParseError::StringMissingClosingQuote),
// todo: fix this test case
//            test_invalid_input(
//                "string extra characters",
//                r#""something"abc"#,
//                ParseError::StringMissingClosingQuote),
            test_invalid_input(
                "string single quote only",
                r#"""#,
                ParseError::StringMissingClosingQuote),
            test_invalid_input(
                "missing open block",
                "}",
                ParseError::MissingOpenBlock),
        ];

        let result: Result<Vec<_>, _> = results.into_iter().collect();
        if let Err(err) = result {
            println!("{}", err);
            panic!();
        }
    }
}
