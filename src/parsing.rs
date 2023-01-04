use crate::{routines::{Routine, IntrinsicRoutine, RoutineSigniture}, Token, Type, Value};

fn parse_routine(value: &str) -> Option<Routine> {
    // todo: add compile type checks
    match value {
        "!add" => Some(Routine::Intrinsic{signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::AddI32), routine: IntrinsicRoutine::AddI32}),
        "!minus" => Some(Routine::Intrinsic{signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::MinusI32), routine: IntrinsicRoutine::MinusI32}),
        _ => None
    }
}

fn parse_token(value: &str) -> Result<Token, ParsingError> {
    if let Ok(i32_value) = value.parse::<i32>() {
        Ok(Token::Constant(Value::I32(i32_value)))
    } else if let Some(routine) = parse_routine(value) {
        Ok(Token::Routine(routine))
    } else {
        println!("{}", value);
        Err(ParsingError::UnexpectedToken)
    }
}

pub(crate) fn parse_input(input: &str) -> Result<Box<[Token]>, ParsingError> {
    let mut tokens = Vec::new(); 
    for value in input.split_whitespace() {
        let token = parse_token(value.trim())?;
        tokens.push(token);
    }
    Ok(tokens.into_boxed_slice())
}

#[derive(Debug)]
pub(crate) enum ParsingError {
    UnexpectedToken
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_constant_i32_pushing() {
        let input = "10 5";
        let parsed = parse_input(input);
        assert!(parsed.is_ok());
        let token_stack = parsed.unwrap();
        let expected_stack = vec![Token::Constant(Value::I32(10)), Token::Constant(Value::I32(5))].into_boxed_slice();
        assert_eq!(token_stack, expected_stack);
    }

    #[test]
    fn parse_routine() {
        let input = "!add";
        let parsed = parse_input(input);
        assert!(parsed.is_ok());
        let token_stack = parsed.unwrap();
        let expected_stack = vec![
            Token::Routine(Routine::Intrinsic {
                signiture: RoutineSigniture::new("add",
                                                 &vec![Type::I32, Type::I32],
                                                 &vec![Type::I32]),
                routine: IntrinsicRoutine::AddI32
            })
        ].into_boxed_slice();
        assert_eq!(token_stack, expected_stack);
    }

    #[test]
    fn parse_constants_and_routine() {
        let input = "10 5 !minus 15";
        let parsed = parse_input(input);
        assert!(parsed.is_ok());
        let token_stack = parsed.unwrap();
        let expected_stack = vec![
            Token::Constant(Value::I32(10)),
            Token::Constant(Value::I32(5)),
            Token::Routine(Routine::Intrinsic {
                signiture: RoutineSigniture::new("minus",
                                                 &vec![Type::I32, Type::I32],
                                                 &vec![Type::I32]),
                routine: IntrinsicRoutine::MinusI32,
            }),
            Token::Constant(Value::I32(15)),
        ].into_boxed_slice();
        assert_eq!(token_stack, expected_stack);
    }
}
