use crate::{Token, Routine, Value, Type};
use std::collections::VecDeque;

#[derive(Debug)]
pub(crate) enum TypeCheckError {
    NotEnoughItems,
    IncorrectType
}

pub(crate) fn type_check(tokens: &[Token]) -> Result<(), TypeCheckError> {
    let mut type_stack = VecDeque::new();
    for token in tokens.iter() {
        match token {
            Token::Constant(Value::I32(_)) => type_stack.push_back(Type::I32),
            Token::Constant(Value::Char(_)) => type_stack.push_back(Type::Char),
            Token::Constant(Value::String(_)) => type_stack.push_back(Type::String),
            Token::Routine(Routine::Intrinsic{signiture, routine: _}) => {
                for input in signiture.inputs() {
                    let top = match type_stack.pop_back() {
                        Some(top) => top,
                        None => return Err(TypeCheckError::NotEnoughItems),
                    };
                    if top != *input {
                        return Err(TypeCheckError::IncorrectType);
                    }
                }
                for output in signiture.outputs() {
                    type_stack.push_back(output.clone());
                }
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::routines::{IntrinsicRoutine, RoutineSigniture};

    #[test]
    fn empty_program_should_succeed() {
        let tokens = [];
        let result = type_check(&tokens);
        assert!(result.is_ok());
    }

    #[test]
    fn constast_pushing_should_succeed() {
        let tokens = [Token::Constant(Value::I32(10))];
        let result = type_check(&tokens);
        assert!(result.is_ok());
    }

    #[test]
    fn routine_call_should_succeed() {
        let tokens = [
            Token::Constant(Value::I32(10)),
            Token::Constant(Value::I32(10)),
            Token::Routine(Routine::Intrinsic {
                signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::AddI32),
                routine: IntrinsicRoutine::AddI32
            })
        ];
        let result = type_check(&tokens);
        assert!(result.is_ok());
    }

    #[test]
    fn routine_call_should_fail_when_not_enough_tokens() {
        let tokens = [
            Token::Constant(Value::I32(10)),
            Token::Routine(Routine::Intrinsic {
                signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::AddI32),
                routine: IntrinsicRoutine::AddI32
            })
        ];
        let result = type_check(&tokens);
        assert!(matches!(result, Err(TypeCheckError::NotEnoughItems)));
    }

    #[test]
    fn routine_call_should_fail_when_incorrect_types() {
        let tokens = [
            Token::Constant(Value::I32(10)),
            Token::Constant(Value::Char('a')),
            Token::Routine(Routine::Intrinsic {
                signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::AddI32),
                routine: IntrinsicRoutine::AddI32,
            })
        ];
        let result = type_check(&tokens);
        assert!(matches!(result, Err(TypeCheckError::IncorrectType)));
    }
}
