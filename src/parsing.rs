use crate::{routines::{Routine, IntrinsicRoutine, RoutineSigniture}, Token, Type, Value};
use std::str::Chars;

struct Parser<I : Iterator<Item = char>> {
    chars: I
}

#[derive(Debug, Clone, PartialEq)]
enum TokenType {
    ConstantI32,
    ConstantChar,
    ConstantString,
    Routine
}

#[derive(Debug)]
pub(crate) enum ParseError {
    CharMissingOpeningTick,
    CharMissingClosingTick,
    CharMissingChar,
    CharExtraCharacters,
    StringMissingOpeningQuote,
    StringNoStringContent,
    StringMissingClosingQuote,
    ParseIntError,
    InvalidRoutine,
}

impl<'a> Parser<Chars<'a>> {
    fn new(input: &'a str) -> Self {
        let chars = input.chars();
        Self {
            chars
        }
    }

    fn next(&mut self) -> Option<Result<Token, ParseError>> {
        let mut current_part = Vec::new();
        let mut possible_token_types = vec![
            TokenType::ConstantChar,
            TokenType::ConstantString,
            TokenType::ConstantI32,
            TokenType::Routine,
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
        match token_type {
            Some(TokenType::ConstantChar) => Some(parse_char(string_value.trim())),
            Some(TokenType::ConstantString) => Some(parse_string(string_value.trim())),
            Some(TokenType::ConstantI32) => Some(string_value.trim().parse().map(|i32_value| Token::Constant(Value::I32(i32_value))).map_err(|e| ParseError::ParseIntError)),
            Some(TokenType::Routine) => Some(parse_routine(string_value.trim())),
            None => None,
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
    if value.find(|c: char| !c.is_numeric() && c != '.').is_some() {
        // todo: floats
    }
}

fn parse_routine(value: &str) -> Result<Token, ParseError> {
    // todo: add compile type checks
    match value {
        "!add" => Ok(Token::Routine(Routine::Intrinsic{signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::AddI32), routine: IntrinsicRoutine::AddI32})),
        "!minus" => Ok(Token::Routine(Routine::Intrinsic{signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::MinusI32), routine: IntrinsicRoutine::MinusI32})),
        "!printc" => Ok(Token::Routine(Routine::Intrinsic{signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::PrintChar), routine: IntrinsicRoutine::PrintChar})),
        "!prints" => Ok(Token::Routine(Routine::Intrinsic{ signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::PrintString), routine: IntrinsicRoutine::PrintString})),
        _ => Err(ParseError::InvalidRoutine),
    }
}

fn parse_char(value: &str) -> Result<Token, ParseError> {
    let mut chars = value.chars();
    if chars.next() != Some('\'') {
        return Err(ParseError::CharMissingOpeningTick);
    }
    let Some(char_value) = chars.next() else {
        return Err(ParseError::CharMissingChar);
    };
    if chars.next() != Some('\'') {
        return Err(ParseError::CharMissingClosingTick);
    }
    if chars.next().is_some() {
        return Err(ParseError::CharExtraCharacters);
    }
    Ok(Token::Constant(Value::Char(char_value)))
}

fn parse_string(value: &str) -> Result<Token, ParseError> {
    let mut chars = value.chars();
    if chars.next() != Some('"') {
        return Err(ParseError::StringMissingOpeningQuote);
    }
    let rest: Vec<_> = chars.collect();
    let Some(last_char) = rest.last() else {
        return Err(ParseError::StringNoStringContent);
    };
    if last_char != &'"' {
        return Err(ParseError::StringMissingClosingQuote);
    }
    let len = rest.len();
    Ok(Token::Constant(Value::String(rest.into_iter().take(len - 1).collect::<String>())))
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
                       &vec![Token::Constant(Value::I32(10)), Token::Constant(Value::I32(5))]),
            test_input("routine",
                       "!add",
                       &vec![Token::Routine(Routine::Intrinsic {
                            signiture: RoutineSigniture::new("add",
                                                            &vec![Type::I32, Type::I32],
                                                            &vec![Type::I32]),
                            routine: IntrinsicRoutine::AddI32
                       })]),
            test_input("negative number",
                       "-124",
                       &vec![
                            Token::Constant(Value::I32(-124)),
                       ]),
            test_input("constants and routine",
                       "10 5 !minus 15",
                       &vec![
                            Token::Constant(Value::I32(10)),
                            Token::Constant(Value::I32(5)),
                            Token::Routine(Routine::Intrinsic {
                                signiture: RoutineSigniture::new("minus",
                                                            &vec![Type::I32, Type::I32],
                                                            &vec![Type::I32]),
                                routine: IntrinsicRoutine::MinusI32
                            }),
                            Token::Constant(Value::I32(15)),
                       ]),
            test_input("char",
                       "'a' '$'",
                       &vec![
                            Token::Constant(Value::Char('a')),
                            Token::Constant(Value::Char('$'))
                       ]),
            test_input("empty string",
                       r#""""#,
                       &vec![
                            Token::Constant(Value::String(String::new()))
                       ]),
            test_input("string with space",
                       r#""some string""#,
                       &vec![
                            Token::Constant(Value::String("some string".to_owned())),
                       ]),
            test_input("string then routine",
                       r#""Hello World!" !prints"#,
                       &vec![
                            Token::Constant(Value::String("Hello World!".to_owned())),
                            Token::Routine(Routine::Intrinsic {
                                signiture: RoutineSigniture::new("prints",
                                                                 &vec![Type::String],
                                                                 &Vec::new()),
                                routine: IntrinsicRoutine::PrintString,
                            }),
                       ]),
        ];

        let result: Result<Vec<_>, _> = results.into_iter().collect();
        if let Err(err) = result {
            println!("{}", err);
            panic!();
        }
    }

    #[test]
    fn parse_incomplete_char() {
        let input = "'a";
        let parsed = parse_input(input);
        assert!(parsed.is_err());
    }
}
