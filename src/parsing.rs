use crate::{
    routines::{IntrinsicRoutine, Routine, RoutineSigniture},
    Block, Token, Type, Value, PileProgram
};
use std::str::Chars;

struct Parser<I: Iterator<Item = char>> {
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
    RoutineCall,
    OpenBlock,
    CloseBlock,
    If,
    While,
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

    fn parse(&mut self) -> Result<PileProgram, ParseError> {
        let mut tokens = Vec::new();
        while let Some(result) = self.next(tokens.len()) {
            let token_position = tokens.len();
            let token = result?;
            if let Token::Block(Block::Close { open_position }) = token {
                if let Some(Token::Block(Block::Open { close_position })) =
                    tokens.get_mut(open_position)
                {
                    *close_position = token_position;
                }
            }
            tokens.push(token);
        }
        Ok(PileProgram {
            tokens: tokens.into_boxed_slice(),
        })
    }

    fn next(&mut self, token_number: usize) -> Option<Result<Token, ParseError>> {
        let mut current_part = Vec::new();
        // todo: add all values
        let mut possible_token_types = vec![
            TokenType::ConstantChar,
            TokenType::ConstantString,
            TokenType::ConstantI32,
            TokenType::ConstantBool,
            TokenType::RoutineCall,
            TokenType::OpenBlock,
            TokenType::CloseBlock,
            TokenType::If,
            TokenType::While,
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
        match token_type {
            TokenType::ConstantChar => Some(parse_char(string_value.trim())),
            TokenType::ConstantString => Some(parse_string(string_value.trim())),
            TokenType::ConstantI32 => Some(
                string_value
                    .trim()
                    .parse()
                    .map(|i32_value| Token::Constant(Value::I32(i32_value)))
                    .map_err(|e| ParseError::ParseIntError),
            ),
            TokenType::ConstantBool => Some(parse_bool(string_value.trim())),
            TokenType::OpenBlock => {
                self.block_stack.push(token_number);
                Some(Ok(Token::Block(
                    Block::Open { close_position: 0 },
                )))
            }
            TokenType::CloseBlock => {
                if let Some(open_position) = self.block_stack.pop() {
                    Some(Ok(Token::Block(
                        Block::Close { open_position },
                    )))
                } else {
                    Some(Err(ParseError::MissingOpenBlock))
                }
            }
            TokenType::If => Some(Ok(Token::If)),
            TokenType::While => Some(Ok(Token::While)),
            TokenType::RoutineCall => Some(parse_routine_call(string_value.trim())),
        }
    }
}

fn evaluate_possible_tokens(value: &str, current_possibilities: &mut Vec<TokenType>) {
    if !value.starts_with("'") {
        if let Some(found_index) = current_possibilities
            .iter()
            .position(|p| p == &TokenType::ConstantChar)
        {
            current_possibilities.remove(found_index);
        }
    }
    if !value.starts_with("\"") {
        if let Some(found_index) = current_possibilities
            .iter()
            .position(|p| p == &TokenType::ConstantString)
        {
            current_possibilities.remove(found_index);
        }
    }
    if !value.starts_with("!") {
        if let Some(found_index) = current_possibilities
            .iter()
            .position(|p| p == &TokenType::RoutineCall)
        {
            current_possibilities.remove(found_index);
        }
    }
    if value
        .find(|c: char| c == '.' || !(c.is_numeric() || c == '-'))
        .is_some()
    {
        if let Some(found_index) = current_possibilities
            .iter()
            .position(|p| p == &TokenType::ConstantI32)
        {
            current_possibilities.remove(found_index);
        }
    }
    if value != "{" {
        if let Some(found_index) = current_possibilities
            .iter()
            .position(|p| p == &TokenType::OpenBlock)
        {
            current_possibilities.remove(found_index);
        }
    }
    if value != "}" {
        if let Some(found_index) = current_possibilities
            .iter()
            .position(|p| p == &TokenType::CloseBlock)
        {
            current_possibilities.remove(found_index);
        }
    }
    if !"if".starts_with(value) {
        if let Some(found_index) = current_possibilities
            .iter()
            .position(|p| p == &TokenType::If)
        {
            current_possibilities.remove(found_index);
        }
    }
    if !"while".starts_with(value) {
        if let Some(found_index) = current_possibilities
            .iter()
            .position(|p| p == &TokenType::While)
        {
            current_possibilities.remove(found_index);
        }
    }
    const true_str: &str = "true";
    const false_str: &str = "false";
    if let Some(found_index) = current_possibilities
        .iter()
        .position(|p| p == &TokenType::ConstantBool)
    {
        let mut true_chars = true_str.chars();
        let mut false_chars = false_str.chars();
        for (index, char_value) in value.chars().enumerate() {
            let true_char = true_chars.next();
            let false_char = false_chars.next();
            let is_invalid = match (true_char, false_char) {
                (None, None) => true,
                (Some(true_char), None) => true_char != char_value,
                (None, Some(false_char)) => false_char != char_value,
                (Some(true_char), Some(false_char)) => {
                    true_char != char_value && false_char != char_value
                }
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

fn parse_routine_call(value: &str) -> Result<Token, ParseError> {
    // todo: add compile type checks
    match value {
        "!add" => Ok(Token::Routine(
            Routine::new_intrinsic(IntrinsicRoutine::AddI32),
        )),
        "!minus" => Ok(Token::Routine(
            Routine::new_intrinsic(IntrinsicRoutine::MinusI32),
        )),
        "!print" => Ok(Token::Routine(
            Routine::new_intrinsic(IntrinsicRoutine::Print),
        )),
        "!eq" => Ok(Token::Routine(
            Routine::new_intrinsic(IntrinsicRoutine::Eq),
        )),
        "!not" => Ok(Token::Routine(
            Routine::new_intrinsic(IntrinsicRoutine::Not),
        )),
        "!clone" => Ok(Token::Routine(
            Routine::new_intrinsic(IntrinsicRoutine::Clone),
        )),
        "!swap" => Ok(Token::Routine(
            Routine::new_intrinsic(IntrinsicRoutine::Swap),
        )),
        _ => Err(ParseError::InvalidRoutine),
    }
}

fn parse_char(value: &str) -> Result<Token, ParseError> {
    if !value.starts_with('\'') {
        return Err(ParseError::CharMissingOpeningTick);
    } else if !value.ends_with('\'') || value.len() == 1 {
        return Err(ParseError::CharMissingClosingTick);
    };
    let middle_chars: Vec<_> = value.chars().skip(1).collect();
    // todo: allow escaping
    match (middle_chars.len(), middle_chars.first()) {
        (1, _) => Err(ParseError::CharMissingChar),
        (2, Some(char_value)) => Ok(Token::Constant(Value::Char(*char_value))),
        _ => Err(ParseError::CharExtraCharacters),
    }
}

fn parse_string(value: &str) -> Result<Token, ParseError> {
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
    Ok(Token::Constant(
        Value::String(rest.into_iter().take(len - 1).collect::<String>()),
    ))
}

fn parse_bool(value: &str) -> Result<Token, ParseError> {
    match value {
        "true" => Ok(Token::Constant(Value::Bool(true))),
        "false" => Ok(Token::Constant(Value::Bool(false))),
        _ => Err(ParseError::BoolInvalid),
    }
}

impl PileProgram {
    pub(crate) fn parse(input: &str) -> Result<Self, ParseError> {
        let mut parser = Parser::new(input);
        parser.parse()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn test_input(description: &str, input: &str, expected_tokens: &[Token]) -> Result<(), String> {
        let parsed =
            PileProgram::parse(input).map_err(|e| format!(r#"test "{}" failed: {:?}"#, description, e))?;
        if parsed.tokens.as_ref() != expected_tokens {
            Err(format!(
                "Expected {:?}, found {:?}",
                expected_tokens, parsed
            ))
        } else {
            Ok(())
        }
    }

    #[test]
    fn test_parsing() {
        let results = vec![
            test_input(
                "constant i32 pushing",
                "10 5",
                &vec![
                    Token::Constant(0, Value::I32(10)),
                    Token::Constant(1, Value::I32(5)),
                ],
            ),
            test_input(
                "routine",
                "!add",
                &vec![Token::Routine(
                    0,
                    Routine::new_intrinsic(IntrinsicRoutine::AddI32),
                )],
            ),
            test_input(
                "negative number",
                "-124",
                &vec![Token::Constant(0, Value::I32(-124))],
            ),
            test_input(
                "constants and routine",
                "10 5 !minus 15",
                &vec![
                    Token::Constant(0, Value::I32(10)),
                    Token::Constant(1, Value::I32(5)),
                    Token::Routine(2, Routine::new_intrinsic(IntrinsicRoutine::MinusI32)),
                    Token::Constant(3, Value::I32(15)),
                ],
            ),
            test_input(
                "char",
                "'a' '$'",
                &vec![
                    Token::Constant(0, Value::Char('a')),
                    Token::Constant(1, Value::Char('$')),
                ],
            ),
            test_input(
                "bool",
                "true false",
                &vec![
                    Token::Constant(0, Value::Bool(true)),
                    Token::Constant(1, Value::Bool(false)),
                ],
            ),
            test_input(
                "empty string",
                r#""""#,
                &vec![Token::Constant(0, Value::String(String::new()))],
            ),
            test_input(
                "string with space",
                r#""some string""#,
                &vec![Token::Constant(0, Value::String("some string".to_owned()))],
            ),
            test_input(
                "string then routine",
                r#""Hello World!" !print"#,
                &vec![
                    Token::Constant(0, Value::String("Hello World!".to_owned())),
                    Token::Routine(1, Routine::new_intrinsic(IntrinsicRoutine::Print)),
                ],
            ),
            test_input(
                "block",
                "{ }",
                &vec![
                    Token::Block(0, Block::Open { close_position: 1 }),
                    Token::Block(1, Block::Close { open_position: 0 }),
                ],
            ),
            test_input(
                "nested block",
                "{ { } }",
                &vec![
                    Token::Block(0, Block::Open { close_position: 3 }),
                    Token::Block(1, Block::Open { close_position: 2 }),
                    Token::Block(2, Block::Close { open_position: 1 }),
                    Token::Block(3, Block::Close { open_position: 0 }),
                ],
            ),
            test_input(
                "if block",
                r"true if {
                            'a' !print
                        }",
                &vec![
                    Token::Constant(0, Value::Bool(true)),
                    Token::If(1),
                    Token::Block(2, Block::Open { close_position: 5 }),
                    Token::Constant(3, Value::Char('a')),
                    Token::Routine(4, Routine::new_intrinsic(IntrinsicRoutine::Print)),
                    Token::Block(5, Block::Close { open_position: 2 }),
                ],
            ),
        ];

        let result: Result<Vec<_>, _> = results.into_iter().collect();
        if let Err(err) = result {
            println!("{}", err);
            panic!();
        }
    }

    fn test_invalid_input(
        description: &str,
        input: &str,
        expected_error: ParseError,
    ) -> Result<(), String> {
        let parsed = PileProgram::parse(input);
        match parsed {
            Ok(tokens) => Err(format!(
                "Input: \"{}\". Expected parse failure {:?}, but found successful: {:?}",
                input, expected_error, tokens
            )),
            Err(err) if err != expected_error => Err(format!(
                "Input: \"{}\". Expected parse failure {:?}, but found parse failure {:?}",
                input, expected_error, err
            )),
            Err(err) => Ok(()),
        }
    }

    #[test]
    fn invalid_tokens() {
        let results = [
            test_invalid_input(
                "char missing closing tick",
                "'a",
                ParseError::CharMissingClosingTick,
            ),
            test_invalid_input("char missing character", "''", ParseError::CharMissingChar),
            test_invalid_input(
                "char only opening tick",
                "'",
                ParseError::CharMissingClosingTick,
            ),
            test_invalid_input(
                "char extra characters",
                "'abc'",
                ParseError::CharExtraCharacters,
            ),
            test_invalid_input(
                "char extra characters after tick",
                "'a'abc",
                ParseError::CharMissingClosingTick,
            ),
            test_invalid_input("i32 with number", "1a", ParseError::ParseIntError),
            test_invalid_input(
                "string missing closing quote",
                r#""something"#,
                ParseError::StringMissingClosingQuote,
            ),
            // todo: fix this test case
            //            test_invalid_input(
            //                "string extra characters",
            //                r#""something"abc"#,
            //                ParseError::StringMissingClosingQuote),
            test_invalid_input(
                "string single quote only",
                r#"""#,
                ParseError::StringMissingClosingQuote,
            ),
            test_invalid_input("missing open block", "}", ParseError::MissingOpenBlock),
        ];

        let result: Result<Vec<_>, _> = results.into_iter().collect();
        if let Err(err) = result {
            println!("{}", err);
            panic!();
        }
    }
}
