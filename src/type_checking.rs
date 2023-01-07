use crate::{Token, Routine, Value, Type, Block};
use std::collections::VecDeque;

#[derive(Debug)]
pub(crate) enum TypeCheckError {
    NotEnoughItems,
    IncorrectType,
    MissingCloseBlock,
    MissingOpenBlock,
    ClosingIncorrectBlock,
    NonEmptyStackAfterBlock,
    IfNotBeforeBlock,
}

pub(crate) fn type_check(tokens: &[Token]) -> Result<(), TypeCheckError> {
    let mut type_stacks = vec![Vec::new()];
    let mut block_stack = VecDeque::new();
    let mut after_if = false;
    for token in tokens.iter() {
        if after_if && !matches!(&token, Token::Block(_, Block::Open)) {
            return Err(TypeCheckError::IfNotBeforeBlock);
        }
        after_if = false;
        match token {
            Token::Constant(_, Value::I32(_)) => {
                let Some(type_stack) = type_stacks.last_mut() else {
                    panic!("Missing type stack");
                };
                type_stack.push(Type::I32)
            },
            Token::Constant(_, Value::Char(_)) => {
                let Some(type_stack) = type_stacks.last_mut() else {
                    panic!("Missing type stack");
                };
                type_stack.push(Type::Char);
            },
            Token::Constant(_, Value::String(_)) => {
                let Some(type_stack) = type_stacks.last_mut() else {
                    panic!("Missing type stack");
                };
                type_stack.push(Type::String);
            },
            Token::Constant(_, Value::Bool(_)) => {
                let Some(type_stack) = type_stacks.last_mut() else {
                    panic!("Missing type stack");
                };
                type_stack.push(Type::Bool);
            },
            Token::Block(position, Block::Open) => {
                block_stack.push_back(position);
                type_stacks.push(Vec::new());
            },
            Token::Block(_, Block::Close { open_position }) => {
                let Some(open_block) = block_stack.pop_back() else {
                    return Err(TypeCheckError::MissingOpenBlock);
                };
                if open_block != open_position {
                    return Err(TypeCheckError::ClosingIncorrectBlock);
                }
                if let Some(top_stack) = type_stacks.pop() {
                    if !top_stack.is_empty() {
                        return Err(TypeCheckError::NonEmptyStackAfterBlock);
                    }
                }
            },
            Token::If(_) => {
                after_if = true;
                let Some(type_stack) = type_stacks.last_mut() else {
                    panic!("Missing type stack");
                };
                match type_stack.pop() {
                    None => return Err(TypeCheckError::NotEnoughItems),
                    Some(Type::Bool) => (),
                    _ => return Err(TypeCheckError::IncorrectType),
                }
            },
            Token::Routine(_, Routine::Intrinsic{signiture, routine: _}) => {
                let Some(type_stack) = type_stacks.last_mut() else {
                    panic!("Missing type stack");
                };
                for input in signiture.inputs() {
                    let top = match type_stack.pop() {
                        Some(top) => top,
                        None => return Err(TypeCheckError::NotEnoughItems),
                    };
                    if top != *input {
                        return Err(TypeCheckError::IncorrectType);
                    }
                }
                for output in signiture.outputs() {
                    type_stack.push(output.clone());
                }
            }
        }
    }
    if !block_stack.is_empty() {
        return Err(TypeCheckError::MissingCloseBlock);
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
        let tokens = [Token::Constant(0, Value::I32(10))];
        let result = type_check(&tokens);
        assert!(result.is_ok());
    }

    #[test]
    fn routine_call_should_succeed() {
        let tokens = [
            Token::Constant(0, Value::I32(10)),
            Token::Constant(1, Value::I32(10)),
            Token::Routine(2, Routine::Intrinsic {
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
            Token::Constant(0, Value::I32(10)),
            Token::Routine(1, Routine::Intrinsic {
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
            Token::Constant(0, Value::I32(10)),
            Token::Constant(1, Value::Char('a')),
            Token::Routine(2, Routine::Intrinsic {
                signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::AddI32),
                routine: IntrinsicRoutine::AddI32,
            })
        ];
        let result = type_check(&tokens);
        assert!(matches!(result, Err(TypeCheckError::IncorrectType)));
    }

    #[test]
    fn block_succeeds() {
        let tokens = [
            Token::Constant(0, Value::I32(10)),
            Token::Block(1, Block::Open),
            Token::Constant(2, Value::Char('a')),
            Token::Routine(3, Routine::Intrinsic {
                signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::PrintChar),
                routine: IntrinsicRoutine::PrintChar,
            }),
            Token::Block(4, Block::Close { open_position: 1 }),
            Token::Constant(5, Value::String("Hello World".to_owned())),
        ];
        let result = type_check(&tokens);
        assert!(result.is_ok());
    }

    #[test]
    fn empty_block() {
        let tokens = [
            Token::Block(0, Block::Open),
            Token::Block(1, Block::Close { open_position: 0 }),
        ];
        let result = type_check(&tokens);
        assert!(result.is_ok());
    }


    #[test]
    fn nested_blocks() {
        let tokens = [
            Token::Block(0, Block::Open),
            Token::Block(1, Block::Open),
            Token::Block(2, Block::Close { open_position: 1 }),
            Token::Block(3, Block::Close { open_position: 0 }),
        ];
        let result = type_check(&tokens);
        assert!(result.is_ok());
    }

    #[test]
    fn missing_opening_block() {
        let tokens = [
            Token::Block(0, Block::Close { open_position: 0 }),
        ];
        let result = type_check(&tokens);
        assert!(matches!(result, Err(TypeCheckError::MissingOpenBlock)));
    }

    #[test]
    fn missing_closing_block() {
        let tokens = [
            Token::Block(0, Block::Open),
        ];
        let result = type_check(&tokens);
        assert!(matches!(result, Err(TypeCheckError::MissingCloseBlock)));
    }

    #[test]
    fn closing_incorrect_open_block() {
        let tokens = [
            Token::Block(0, Block::Open),
            Token::Block(1, Block::Open),
            Token::Block(2, Block::Close { open_position: 0 }),
            Token::Block(3, Block::Close { open_position: 1 }),
        ];
        let result = type_check(&tokens);
        assert!(matches!(result, Err(TypeCheckError::ClosingIncorrectBlock)));
    }

    #[test]
    fn block_leaves_items_on_the_stack() {
        let tokens = [
            Token::Block(0, Block::Open),
            Token::Constant(1, Value::I32(10)),
            Token::Block(2, Block::Close { open_position: 0 }),
        ];
        let result = type_check(&tokens);
        assert!(matches!(result, Err(TypeCheckError::NonEmptyStackAfterBlock)));
    }

    #[test]
    fn block_consumes_items_on_stack() {
        let tokens = [
            Token::Block(0, Block::Open),
            Token::Constant(1, Value::Char('a')),
            Token::Routine(2, Routine::Intrinsic {
                signiture: RoutineSigniture::from_intrinsic(IntrinsicRoutine::PrintChar),
                routine: IntrinsicRoutine::PrintChar,
            }),
            Token::Block(3, Block::Close { open_position: 0 }),
        ];
        let result = type_check(&tokens);
        assert!(result.is_ok());
    }

    #[test]
    fn if_test() {
        let tokens = [
            Token::Constant(0, Value::Bool(true)),
            Token::If(1),
            Token::Block(2, Block::Open),
            Token::Block(3, Block::Close { open_position: 2 }),
        ];
        let result = type_check(&tokens);
        assert!(result.is_ok());
    }

    #[test]
    fn if_fails_after_non_bool() {
        let tokens = [
            Token::Constant(0, Value::Char('a')),
            Token::If(1),
            Token::Block(2, Block::Open),
            Token::Block(3, Block::Close { open_position: 2 }),
        ];
        let result = type_check(&tokens);
        assert!(matches!(result, Err(TypeCheckError::IncorrectType)));
    }

    #[test]
    fn if_empty_stack() {
        let tokens = [
            Token::If(0),
            Token::Block(1, Block::Open),
            Token::Block(2, Block::Close { open_position: 1 }),
        ];
        let result = type_check(&tokens);
        assert!(matches!(result, Err(TypeCheckError::NotEnoughItems)));
    }

    #[test]
    fn if_fails_when_not_before_block() {
        let tokens = [
            Token::Constant(0, Value::Bool(true)),
            Token::If(1),
            Token::Constant(2, Value::I32(10)),
        ];
        let result = type_check(&tokens);
        assert!(matches!(result, Err(TypeCheckError::IfNotBeforeBlock)));
    }
}
