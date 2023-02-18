use crate::{
    routines::{IntrinsicRoutine, Routine, RoutineSigniture},
    Block, PileProgram, Token, Type, Value,
};
use std::str::Chars;
use std::collections::HashMap;

struct Parser<I: Iterator<Item = char>> {
    chars: I,
    block_stack: Vec<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum TokenType {
    ConstantI32,
    ConstantChar,
    ConstantString,
    ConstantBool,
    RoutineCall,
    RoutineDefinitionName,
    RoutineDefinitionSeparator,
    Type,
    RoutineSignitureSeparator,
    OpenBlock,
    CloseBlock,
    If,
    While,
    Comment,
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
    MissingOpenBlock,
    AlreadyDefiningRoutine,
    UnexpectedRoutineDefinitionSeparator,
    UnexpectedTypeName,
    UnexpectedRoutineSignitureSeparator,
    InvalidRoutineDefinitionName,
    InvalidCharsInRoutineDefinitionName,
    UnknownType,
    RoutineMissingBang,
    RoutineInvalidCharacters,
    RoutineMissingName,
    RoutineDefinitionMissingDefinitionName,
    RoutineDefinitionMissingSeparator,
    RoutineExtraNoneType,
    RoutineMissingTypes,
    RoutineSignitureUnexpectedToken,
    RoutineMissingTokens,
    InvalidCloseBlockOpenPosition,
    UnknownToken,
}

#[derive(Debug, Clone)]
enum IntermediateType {
    Type(Type),
    None
}

#[derive(Debug, Clone)]
enum IntermediateToken {
    Constant(Value),
    Block(Block),
    If,
    While,
    RoutineCall(String),
    RoutineDefinitionName(String),
    RoutineDefinitionSeparator,
    Type(IntermediateType),
    RoutineSignitureSeparator,
    Comment
}

impl<'a> Parser<Chars<'a>> {
    fn new(input: &'a str) -> Self {
        let chars = input.chars();
        Self {
            chars,
            block_stack: Vec::new(),
        }
    }

    fn parse(&mut self) -> Result<PileProgram, ParseError> {
        let mut intermediate_tokens = Vec::new();
        while let Some(result) = self.next(intermediate_tokens.len()) {
            intermediate_tokens.push(result?);
        }
        
        let program = Self::convert_intermediate_tokens(&intermediate_tokens)?;
        
        Ok(program)
    }

    fn clean_block_positions(tokens: &mut [Token]) -> Result<(), ParseError> {
        let mut open_positions = Vec::new();
        let mut index = 0;
        while let Some(token) = tokens.get_mut(index){
            match token {
                Token::Block(Block::Open { close_position: _ }) => {
                    open_positions.push(index);
                },
                Token::Block(Block::Close { open_position }) => {
                    *open_position = open_positions.pop().ok_or(ParseError::MissingOpenBlock)?;
                    let Token::Block(Block::Open { close_position }) = &mut tokens[*open_position] else {
                        return Err(ParseError::InvalidCloseBlockOpenPosition);
                    };
                    *close_position = index;
                },
                _ => ()
            }
            index += 1;
        }
        Ok(())
    }

    fn convert_intermediate_tokens(intermediate_tokens: &[IntermediateToken]) -> Result<PileProgram, ParseError> {
        let mut routines = IntrinsicRoutine::get_routine_dictionary();
        let mut tokens = Vec::new();
        let mut current_routine_definition_tokens: Option<Vec<_>> = None;
        let mut current_routine_body_tokens: Option<Vec<_>> = None;
        let mut current_routine_signiture = None;
        let mut routine_nest_level = 0;
        fn push_token(token: Token, current_routine_body_tokens: &mut Option<Vec<Token>>, tokens: &mut Vec<Token>) {
            if let Some(routine_tokens) = current_routine_body_tokens {
                routine_tokens.push(token);
            } else {
                tokens.push(token);
            }
        }
        for token in intermediate_tokens {
            match token.clone() {
                IntermediateToken::Constant(value) => {
                    push_token(Token::Constant(value.clone()), &mut current_routine_body_tokens, &mut tokens);
                },
                IntermediateToken::If => {
                    push_token(Token::If, &mut current_routine_body_tokens, &mut tokens);
                },
                IntermediateToken::While => {
                    push_token(Token::While, &mut current_routine_body_tokens, &mut tokens);
                },
                IntermediateToken::Comment => (),
                IntermediateToken::Block(Block::Open { close_position }) => {
                    if let (Some(definition_tokens), None) = (&current_routine_definition_tokens, &current_routine_signiture) {
                        let mut definition_tokens = definition_tokens.iter();
                        let Some(IntermediateToken::RoutineDefinitionName(routine_name)) = definition_tokens.next() else {
                            return Err(ParseError::RoutineDefinitionMissingDefinitionName);
                        };
                        if !matches!(definition_tokens.next(), Some(IntermediateToken::RoutineDefinitionSeparator)) {
                            return Err(ParseError::RoutineDefinitionMissingSeparator);
                        }
                        let mut input_types = Vec::new();
                        let mut input_none = false;
                        loop {
                            match definition_tokens.next() {
                                Some(IntermediateToken::Type(IntermediateType::None)) if input_types.is_empty() => input_none = true,
                                Some(IntermediateToken::Type(IntermediateType::None)) => return Err(ParseError::RoutineExtraNoneType),
                                Some(IntermediateToken::Type(IntermediateType::Type(found_type))) => input_types.push(found_type.clone()),
                                Some(IntermediateToken::RoutineSignitureSeparator) if input_none || !input_types.is_empty() => break,
                                Some(IntermediateToken::RoutineSignitureSeparator) => return Err(ParseError::RoutineMissingTypes),
                                Some(_) => return Err(ParseError::RoutineSignitureUnexpectedToken),
                                None => return Err(ParseError::RoutineMissingTokens),
                            }
                        }
                        let mut output_types = Vec::new();
                        let mut output_none = false;
                        loop {
                            match definition_tokens.next() {
                                Some(IntermediateToken::Type(IntermediateType::None)) if output_types.is_empty() => output_none = true,
                                Some(IntermediateToken::Type(IntermediateType::None)) => return Err(ParseError::RoutineExtraNoneType),
                                Some(IntermediateToken::Type(IntermediateType::Type(found_type))) => output_types.push(found_type.clone()),
                                Some(_) => return Err(ParseError::RoutineSignitureUnexpectedToken),
                                None if output_none || !output_types.is_empty() => break,
                                None => return Err(ParseError::RoutineMissingTokens),
                            }
                        }
                        current_routine_signiture = Some(RoutineSigniture::new(routine_name, &input_types, &output_types));
                        current_routine_definition_tokens = None;
                        current_routine_body_tokens = Some(Vec::new());
                    } else if let Some(body_tokens) = &mut current_routine_body_tokens {
                            routine_nest_level += 1;
                            body_tokens.push(Token::Block(Block::Open { close_position }));
                        } else {
                            tokens.push(Token::Block(Block::Open { close_position }))
                        }
                },
                IntermediateToken::Block(Block::Close { open_position }) => {
                    if routine_nest_level == 0 {
                        if let (Some(routine_signiture), Some(body_tokens)) = (current_routine_signiture.clone(), current_routine_body_tokens.clone()) {
                            routines.insert(routine_signiture.name().to_owned(), Routine::Pile{ signiture: routine_signiture, routine: body_tokens.clone().into_boxed_slice() });
                            current_routine_signiture = None;
                            current_routine_body_tokens = None;
                            continue;
                        }
                    }
                    if let Some(body_tokens) = &mut current_routine_body_tokens {
                        routine_nest_level -= 1;
                        body_tokens.push(Token::Block(Block::Close { open_position }));
                    } else {
                        tokens.push(Token::Block(Block::Close { open_position }));
                    }
                }
                IntermediateToken::RoutineCall(routine_name) => {
                    push_token(Token::RoutineCall(routine_name.to_owned()), &mut current_routine_body_tokens, &mut tokens);
                },
                IntermediateToken::RoutineDefinitionName(_) => {
                    if current_routine_definition_tokens.is_some() {
                        return Err(ParseError::AlreadyDefiningRoutine);
                    }
                    current_routine_definition_tokens = Some(vec![token.clone()]);
                },
                IntermediateToken::RoutineDefinitionSeparator => {
                    let Some(current_routine_definition_tokens) = &mut current_routine_definition_tokens else {
                        return Err(ParseError::UnexpectedRoutineDefinitionSeparator);
                    };
                    current_routine_definition_tokens.push(token.clone());
                },
                IntermediateToken::Type(_) => {
                    let Some(current_routine_definition_tokens) = &mut current_routine_definition_tokens else {
                        return Err(ParseError::UnexpectedTypeName);
                    };
                    current_routine_definition_tokens.push(token.clone());
                },
                IntermediateToken::RoutineSignitureSeparator => {
                    let Some(current_routine_definition_tokens) = &mut current_routine_definition_tokens else {
                        return Err(ParseError::UnexpectedRoutineSignitureSeparator);
                    };
                    current_routine_definition_tokens.push(token.clone());
                },
            }
        }

        for routine in &mut routines {
            if let (_, Routine::Pile { signiture: _, routine: tokens }) = routine {
                Self::clean_block_positions(&mut *tokens)?;
            }
        }

        let mut tokens = tokens.into_boxed_slice();

        Self::clean_block_positions(&mut tokens)?;

        Ok(PileProgram { routines, tokens })
    }

    fn next(&mut self, token_number: usize) -> Option<Result<IntermediateToken, ParseError>> {
        let mut current_part = Vec::new();
        // todo: add all values at compile time
        let mut token_type = None;
        let mut delimeter: &dyn Fn(char) -> bool = &char::is_whitespace;
        for next_char in self.chars.by_ref() {
            if current_part.is_empty() && next_char.is_whitespace() {
                continue;
            }
            current_part.push(next_char);
            if delimeter(next_char) {
                if current_part.iter().collect::<String>().trim().is_empty() {
                    continue;
                }
                break;
            }

            if token_type.is_none() {
                let possible_token_types = evaluate_possible_tokens(current_part.iter().collect::<String>().trim());
                if possible_token_types.len() == 1 {
                    token_type = Some(possible_token_types[0]);
                    match token_type {
                        Some(TokenType::ConstantString) => delimeter = &|c: char| c == '"',
                        Some(TokenType::Comment) => delimeter = &|c: char| c == '\n',
                        _ => ()
                    }
                }
            }
        }

        if current_part.iter().filter(|p| !p.is_whitespace()).count() == 0 {
            return None;
        }

        let string_value: String = current_part.into_iter().collect();
        let Some(token_type) = token_type else {
            return Some(Err(ParseError::UnknownToken));
        };
        match token_type {
            TokenType::Comment => Some(Ok(IntermediateToken::Comment)),
            TokenType::ConstantChar => Some(parse_char(string_value.trim())),
            TokenType::ConstantString => Some(parse_string(string_value.trim())),
            TokenType::ConstantI32 => Some(
                string_value
                    .trim()
                    .parse()
                    .map(|i32_value| IntermediateToken::Constant(Value::I32(i32_value)))
                    .map_err(|_e| ParseError::ParseIntError),
            ),
            TokenType::ConstantBool => Some(parse_bool(string_value.trim())),
            TokenType::OpenBlock => {
                self.block_stack.push(token_number);
                Some(Ok(IntermediateToken::Block(Block::Open { close_position: 0 })))
            }
            TokenType::CloseBlock => {
                if let Some(open_position) = self.block_stack.pop() {
                    Some(Ok(IntermediateToken::Block(Block::Close { open_position })))
                } else {
                    Some(Err(ParseError::MissingOpenBlock))
                }
            }
            TokenType::If => Some(Ok(IntermediateToken::If)),
            TokenType::While => Some(Ok(IntermediateToken::While)),
            TokenType::RoutineCall => Some(parse_routine_call(string_value.trim())),
            TokenType::RoutineDefinitionName => Some(parse_routine_definition_name(string_value.trim())),
            TokenType::RoutineDefinitionSeparator => Some(Ok(IntermediateToken::RoutineDefinitionSeparator)),
            TokenType::RoutineSignitureSeparator => Some(Ok(IntermediateToken::RoutineSignitureSeparator)),
            TokenType::Type => Some(parse_type_name(string_value.trim())),
        }
    }
}

fn evaluate_possible_tokens(value: &str) -> Box<[TokenType]> {
    let mut possibilities = Vec::new();
    fn add_type_if(possibilities: &mut Vec<TokenType>, token_type: TokenType, condition: bool) {
        if condition {
            possibilities.push(token_type);
        }
    }
    add_type_if(&mut possibilities, TokenType::Comment, value.len() == 1 && value == "/" || value.len() > 1 && value.starts_with("//"));
    add_type_if(&mut possibilities, TokenType::ConstantChar, value.starts_with('\''));
    add_type_if(&mut possibilities, TokenType::ConstantString, value.starts_with('"'));
    add_type_if(&mut possibilities, TokenType::RoutineCall, value.starts_with('!') && value.chars().nth(1).filter(|c| !c.is_alphabetic()).is_none());
    add_type_if(&mut possibilities, TokenType::RoutineDefinitionName, value.starts_with('!') && value.len() <= 1 || value.starts_with("!!"));
    add_type_if(&mut possibilities, TokenType::RoutineSignitureSeparator, "->".starts_with(value));
    add_type_if(&mut possibilities, TokenType::RoutineDefinitionSeparator, value == "|");
    if value
        .find(|c: char| c == '.' || !(c.is_numeric() || c == '-'))
        .is_none()
    {
        possibilities.push(TokenType::ConstantI32);
    }
    add_type_if(&mut possibilities, TokenType::OpenBlock, value == "{");
    add_type_if(&mut possibilities, TokenType::CloseBlock, value == "}");
    add_type_if(&mut possibilities, TokenType::If, "if".starts_with(value));
    add_type_if(&mut possibilities, TokenType::While, "while".starts_with(value));
    add_type_if(&mut possibilities, TokenType::ConstantBool, "true".starts_with(value) || "false".starts_with(value));
    

    let type_names = TYPE_NAMES.iter().map(|(name, _)| name);
    add_type_if(&mut possibilities, TokenType::Type, type_names.filter(|name| name.starts_with(value)).count() > 0);
    possibilities.into_boxed_slice()
}

static TYPE_NAMES: [(&str, IntermediateType); 5] = [
    ("i32", IntermediateType::Type(Type::I32)),
    ("str", IntermediateType::Type(Type::String)),
    ("bool", IntermediateType::Type(Type::Bool)),
    ("char", IntermediateType::Type(Type::Char)),
    ("None", IntermediateType::None),
];

fn parse_type_name(value: &str) -> Result<IntermediateToken, ParseError> {
    let type_names: HashMap<_, _> = TYPE_NAMES.iter().cloned().collect();

    if let Some(found_type) = type_names.get(value) {
        Ok(IntermediateToken::Type(found_type.clone()))
    } else {
        Err(ParseError::UnknownType)
    }
}

fn parse_routine_call(value: &str) -> Result<IntermediateToken, ParseError> {
    fn is_char_allowed(char_value: char) -> bool {
        char_value.is_alphabetic() || char_value == '_'
    }
    if !value.starts_with('!') {
        Err(ParseError::RoutineMissingBang)
    } else if value.chars().skip(1).count() == 0 {
        Err(ParseError::RoutineMissingName)
    } else if value.chars().skip(1).filter(|c| !is_char_allowed(*c)).count() > 0 {
        Err(ParseError::RoutineInvalidCharacters)
    } else {
        Ok(IntermediateToken::RoutineCall(value[1..].to_string()))
    }
}

fn parse_routine_definition_name(value: &str) -> Result<IntermediateToken, ParseError> {
    fn is_char_allowed(char_value: char) -> bool {
        char_value.is_alphabetic() || char_value == '_'
    }
    if !value.starts_with("!!") {
        Err(ParseError::InvalidRoutineDefinitionName)
    } else if value.chars().skip(2).filter(|c| !is_char_allowed(*c)).count() > 0 {
        Err(ParseError::InvalidCharsInRoutineDefinitionName)
    } else {
        Ok(IntermediateToken::RoutineDefinitionName(value[2..].to_string()))
    }
}

fn parse_char(value: &str) -> Result<IntermediateToken, ParseError> {
    if !value.starts_with('\'') {
        return Err(ParseError::CharMissingOpeningTick);
    } else if !value.ends_with('\'') || value.len() == 1 {
        return Err(ParseError::CharMissingClosingTick);
    };
    let middle_chars: Vec<_> = value.chars().skip(1).collect();
    // todo: allow escaping
    match (middle_chars.len(), middle_chars.first()) {
        (1, _) => Err(ParseError::CharMissingChar),
        (2, Some(char_value)) => Ok(IntermediateToken::Constant(Value::Char(*char_value))),
        _ => Err(ParseError::CharExtraCharacters),
    }
}

fn parse_string(value: &str) -> Result<IntermediateToken, ParseError> {
    if !value.starts_with('"') {
        return Err(ParseError::StringMissingOpeningQuote);
    } else if !value.ends_with('"') || value.len() == 1 {
        return Err(ParseError::StringMissingClosingQuote);
    }
    let chars = value.chars().skip(1);
    let rest: Vec<_> = chars.collect();
    let Some(last_char) = rest.last() else {
        return Err(ParseError::StringNoStringContent);
    };
    if last_char != &'"' {
        return Err(ParseError::StringMissingClosingQuote);
    }
    let len = rest.len();
    Ok(IntermediateToken::Constant(Value::String(
        rest.into_iter().take(len - 1).collect::<String>(),
    )))
}

fn parse_bool(value: &str) -> Result<IntermediateToken, ParseError> {
    match value {
        "true" => Ok(IntermediateToken::Constant(Value::Bool(true))),
        "false" => Ok(IntermediateToken::Constant(Value::Bool(false))),
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
        let parsed = PileProgram::parse(input)
            .map_err(|e| format!(r#"test "{}" failed: {:?}"#, description, e))?;
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
                &[
                    Token::Constant(Value::I32(10)),
                    Token::Constant(Value::I32(5)),
                ],
            ),
            test_input(
                "routine",
                "!add",
                &[Token::RoutineCall(
                    "add".to_owned(),
                )],
            ),
            test_input(
                "negative number",
                "-124",
                &[Token::Constant(Value::I32(-124))],
            ),
            test_input(
                "constants and routine",
                "10 5 !minus 15",
                &[
                    Token::Constant(Value::I32(10)),
                    Token::Constant(Value::I32(5)),
                    Token::RoutineCall("minus".to_owned()),
                    Token::Constant(Value::I32(15)),
                ],
            ),
            test_input(
                "char",
                "'a' '$'",
                &[
                    Token::Constant(Value::Char('a')),
                    Token::Constant(Value::Char('$')),
                ],
            ),
            test_input(
                "bool",
                "true false",
                &[
                    Token::Constant(Value::Bool(true)),
                    Token::Constant(Value::Bool(false)),
                ],
            ),
            test_input(
                "empty string",
                r#""""#,
                &[Token::Constant(Value::String(String::new()))],
            ),
            test_input(
                "string with space",
                r#""some string""#,
                &[Token::Constant(Value::String("some string".to_owned()))],
            ),
            test_input(
                "string then routine",
                r#""Hello World!" !print"#,
                &[
                    Token::Constant(Value::String("Hello World!".to_owned())),
                    Token::RoutineCall("print".to_owned()),
                ],
            ),
            test_input(
                "block",
                "{ }",
                &[
                    Token::Block(Block::Open { close_position: 1 }),
                    Token::Block(Block::Close { open_position: 0 }),
                ],
            ),
            test_input(
                "nested block",
                "{ { } }",
                &[
                    Token::Block(Block::Open { close_position: 3 }),
                    Token::Block(Block::Open { close_position: 2 }),
                    Token::Block(Block::Close { open_position: 1 }),
                    Token::Block(Block::Close { open_position: 0 }),
                ],
            ),
            test_input(
                "if block",
                r"true if {
                            'a' !print
                        }",
                &[
                    Token::Constant(Value::Bool(true)),
                    Token::If,
                    Token::Block(Block::Open { close_position: 5 }),
                    Token::Constant(Value::Char('a')),
                    Token::RoutineCall("print".to_owned()),
                    Token::Block(Block::Close { open_position: 2 }),
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
                "description: {}.\nInput: \"{}\".\nExpected parse failure {:?}, but found successful: {:?}",
                description, input, expected_error, tokens
            )),
            Err(err) if err != expected_error => Err(format!(
                "description: {}.\nInput: \"{}\".\nExpected parse failure {:?}, but found parse failure {:?}",
                description, input, expected_error, err
            )),
            Err(_) => Ok(()),
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
                       // test_invalid_input(
                           // "string extra characters",
                           // r#""something"abc"#,
                           // ParseError::StringMissingClosingQuote),
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

    #[test]
    fn routine_definition() {
        let input = r#"
!!my_routine | i32 -> str {
    !print
    "Hello World"
}

10 !my_routine
!print
"#;
        let program = PileProgram::parse(input);
        let expected_program = PileProgram::new(&[
            Token::Constant(Value::I32(10)),
            Token::RoutineCall("my_routine".to_owned()),
            Token::RoutineCall("print".to_owned()),
        ], [("my_routine".to_owned(), Routine::Pile {
            signiture: RoutineSigniture::new("my_routine", &[Type::I32], &vec![Type::String]),
            routine: vec![
                Token::RoutineCall("print".to_owned()),
                Token::Constant(Value::String("Hello World".to_owned())),
            ].into_boxed_slice(),
        })].into_iter().collect());

        assert_eq!(Ok(expected_program), program);
    }

    #[test]
    fn recursive_routine() {
        let input = r#"
!!my_recursive_routine | i32 -> None {
	!clone 0 !eq !not if {
		!clone !print
		1 !swap !minus !my_recursive_routine
	}
}


10 !my_recursive_routine
"#;

        let program = PileProgram::parse(input);
        let expected_program = PileProgram::new(&[
            Token::Constant(Value::I32(10)),
            Token::RoutineCall("my_recursive_routine".to_owned()),
        ],
        [
            (
                "my_recursive_routine".to_owned(),
                Routine::Pile {
                    signiture: RoutineSigniture::new("my_recursive_routine", &[Type::I32], &[]),
                    routine: vec![
                        Token::RoutineCall("clone".to_owned()),
                        Token::Constant(Value::I32(0)),
                        Token::RoutineCall("eq".to_owned()),
                        Token::RoutineCall("not".to_owned()),
                        Token::If,
                        Token::Block(Block::Open { close_position: 12 }),
                        Token::RoutineCall("clone".to_owned()),
                        Token::RoutineCall("print".to_owned()),
                        Token::Constant(Value::I32(1)),
                        Token::RoutineCall("swap".to_owned()),
                        Token::RoutineCall("minus".to_owned()),
                        Token::RoutineCall("my_recursive_routine".to_owned()),
                        Token::Block(Block::Close { open_position: 5 }),
                    ].into_boxed_slice()
                }
            )
        ].into_iter().collect());

        assert_eq!(program, Ok(expected_program));

    }

    #[test]
    fn comments() {
        let input = r#"
!!my_routine | i32 -> str {
    !print !println
    // this is fine
    // "Hi"
    "Hello World"
}

// this is a comment
10 
!my_routine
// !print
!print
"#;

        let program = PileProgram::parse(input);
        let expected_program = PileProgram::new(&[
            Token::Constant(Value::I32(10)),
            Token::RoutineCall("my_routine".to_owned()),
            Token::RoutineCall("print".to_owned()),
        ],
        [
            (
                "my_routine".to_owned(),
                Routine::Pile {
                    signiture: RoutineSigniture::new("my_routine", &[Type::I32], &[Type::String]),
                    routine: vec![
                        Token::RoutineCall("print".to_owned()),
                        Token::RoutineCall("println".to_owned()),
                        Token::Constant(Value::String("Hello World".to_owned())),
                    ].into_boxed_slice()
                }
            )
        ].into_iter().collect());

        assert_eq!(program, Ok(expected_program)); 
    }

    #[test]
    fn routine_and_if_blocks() {
        let input = r#"
!!my_routine | bool -> None {
    !clone if {
        "condition was true" !print
    }
    !not if {
        "condition was false" !print
    }

    "Hello World. This will always print" !print

}

true !my_routine
false !my_routine

true if {
    "Hello World. This is after the routines" !print
}

10 !print

"#;

        let program = PileProgram::parse(input);
        let expected_program = PileProgram::new(&[
            Token::Constant(Value::Bool(true)),
            Token::RoutineCall("my_routine".to_owned()),
            Token::Constant(Value::Bool(false)),
            Token::RoutineCall("my_routine".to_owned()),
            Token::Constant(Value::Bool(true)),
            Token::If,
            Token::Block(Block::Open { close_position: 9 }),
            Token::Constant(Value::String("Hello World. This is after the routines".to_owned())),
            Token::RoutineCall("print".to_owned()),
            Token::Block(Block::Close { open_position: 6 }),
            Token::Constant(Value::I32(10)),
            Token::RoutineCall("print".to_owned()),
        ],
        [
            (
                "my_routine".to_owned(),
                Routine::Pile {
                    signiture: RoutineSigniture::new("my_routine", &[Type::Bool], &[]),
                    routine: vec![
                        Token::RoutineCall("clone".to_owned()),
                        Token::If,
                        Token::Block(Block::Open { close_position: 5 }),
                        Token::Constant(Value::String("condition was true".to_owned())),
                        Token::RoutineCall("print".to_owned()),
                        Token::Block(Block::Close { open_position: 2 }),
                        Token::RoutineCall("not".to_owned()),
                        Token::If,
                        Token::Block(Block::Open { close_position: 11 }),
                        Token::Constant(Value::String("condition was false".to_owned())),
                        Token::RoutineCall("print".to_owned()),
                        Token::Block(Block::Close { open_position: 8 }),
                        Token::Constant(Value::String("Hello World. This will always print".to_owned())),
                        Token::RoutineCall("print".to_owned()),
                    ].into_boxed_slice()
                }
            )
        ].into_iter().collect());

        assert_eq!(program, Ok(expected_program));
    }
}
