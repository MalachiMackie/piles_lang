use crate::{routines::RoutineSigniture, Block, Routine, Token, Type, Value, PileProgram};
use std::collections::{HashMap, VecDeque};

#[derive(Debug)]
pub(crate) enum TypeCheckError {
    NotEnoughItems,
    IncorrectType,
    MissingCloseBlock,
    MissingOpenBlock,
    ClosingIncorrectBlock,
    NonEmptyStackAfterBlock,
    IfNotBeforeBlock,
    MissingGenericOutput,
}

impl PileProgram {

pub(crate) fn type_check(&self) -> Result<(), TypeCheckError> {
    let tokens = &self.tokens;
    let mut type_stack = Vec::new();
    let mut block_stack = Vec::new();
    let mut after_condition_checker = false;
    for token in tokens.iter() {
        if after_condition_checker
            && !matches!(&token, Token::Block(_, Block::Open { close_position: _ }))
        {
            return Err(TypeCheckError::IfNotBeforeBlock);
        }
        after_condition_checker = false;
        match token {
            Token::Constant(_, Value::I32(_)) => type_stack.push(Type::I32),
            Token::Constant(_, Value::Char(_)) => {
                type_stack.push(Type::Char);
            }
            Token::Constant(_, Value::String(_)) => {
                type_stack.push(Type::String);
            }
            Token::Constant(_, Value::Bool(_)) => {
                type_stack.push(Type::Bool);
            }
            Token::Block(position, Block::Open { close_position }) => {
                block_stack.push((position, type_stack.clone()));
            }
            Token::Block(position, Block::Close { open_position }) => {
                let Some((open_block, mut open_stack_state)) = block_stack.pop() else {
                    return Err(TypeCheckError::MissingOpenBlock);
                };
                if open_block != open_position {
                    return Err(TypeCheckError::ClosingIncorrectBlock);
                }
                if let Token::While(_) = &tokens[open_position - 1] {
                    // while loop expects a bool at the top
                    open_stack_state.push(Type::Bool);
                }
                if type_stack != open_stack_state {
                    return Err(TypeCheckError::NonEmptyStackAfterBlock);
                }
            }
            Token::If(_) => {
                after_condition_checker = true;
                match type_stack.pop() {
                    None => return Err(TypeCheckError::NotEnoughItems),
                    Some(Type::Bool) => (),
                    _ => return Err(TypeCheckError::IncorrectType),
                }
            }
            Token::While(_) => {
                after_condition_checker = true;
                match type_stack.pop() {
                    None => return Err(TypeCheckError::NotEnoughItems),
                    Some(Type::Bool) => (),
                    _ => return Err(TypeCheckError::IncorrectType),
                }
            }
            Token::Routine(_, routine) => {
                type_check_routine(routine.signiture(), &mut type_stack)?;
            }
        }
    }
    if !block_stack.is_empty() {
        return Err(TypeCheckError::MissingCloseBlock);
    }
    Ok(())
}
}

fn type_check_routine(
    signiture: &RoutineSigniture,
    type_stack: &mut Vec<Type>,
) -> Result<(), TypeCheckError> {
    let mut generic_types: HashMap<String, Type> = HashMap::new();
    for input in signiture.inputs() {
        let top = match type_stack.pop() {
            Some(top) => top,
            None => return Err(TypeCheckError::NotEnoughItems),
        };
        match input {
            Type::Generic { name } => {
                if let Some(found_type) = generic_types.get(name) {
                    if *found_type != top {
                        return Err(TypeCheckError::IncorrectType);
                    }
                } else {
                    generic_types.insert(name.to_owned(), top);
                }
            }
            _ if top != *input => {
                return Err(TypeCheckError::IncorrectType);
            }
            _ => (),
        }
    }
    for output in signiture.outputs() {
        match output {
            Type::Generic { name } => {
                let Some(found_type) = generic_types.get(name) else {
                                return Err(TypeCheckError::MissingGenericOutput);
                            };
                type_stack.push(found_type.clone());
            }
            output => {
                type_stack.push(output.clone());
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
        let program = PileProgram::new(&[]);
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn constast_pushing_should_succeed() {
        let program = PileProgram::new(&[Token::Constant(0, Value::I32(10))]);
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn intrinsic_routine_call_should_succeed() {
        let program = PileProgram::new(&[
            Token::Constant(0, Value::I32(10)),
            Token::Constant(1, Value::I32(10)),
            Token::Routine(2, Routine::new_intrinsic(IntrinsicRoutine::AddI32)),
        ]);
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn pile_routine_call_should_succeed() {
        let program = PileProgram::new(&[
            Token::Constant(0, Value::I32(10)),
            Token::Routine(
                1,
                Routine::Pile {
                    signiture: RoutineSigniture::new("Something", &[Type::I32], &[Type::String]),
                    routine: Vec::new().into_boxed_slice(),
                },
            ),
            Token::Routine(2, Routine::new_intrinsic(IntrinsicRoutine::Print)),
        ]);
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn routine_call_should_fail_when_not_enough_tokens() {
        let program = PileProgram::new(&[
            Token::Constant(0, Value::I32(10)),
            Token::Routine(1, Routine::new_intrinsic(IntrinsicRoutine::AddI32)),
        ]);
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::NotEnoughItems)));
    }

    #[test]
    fn routine_call_should_fail_when_incorrect_types() {
        let program = PileProgram::new(&[
            Token::Constant(0, Value::I32(10)),
            Token::Constant(1, Value::Char('a')),
            Token::Routine(2, Routine::new_intrinsic(IntrinsicRoutine::AddI32)),
        ]);
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::IncorrectType)));
    }

    #[test]
    fn block_succeeds() {
        let program = PileProgram::new(&[
            Token::Constant(0, Value::I32(10)),
            Token::Block(1, Block::Open { close_position: 4 }),
            Token::Constant(2, Value::Char('a')),
            Token::Routine(3, Routine::new_intrinsic(IntrinsicRoutine::Print)),
            Token::Block(4, Block::Close { open_position: 1 }),
            Token::Constant(5, Value::String("Hello World".to_owned())),
        ]);
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn missing_opening_block() {
        let program = PileProgram::new(&[Token::Block(0, Block::Close { open_position: 0 })]);
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::MissingOpenBlock)));
    }

    #[test]
    fn missing_closing_block() {
        let program = PileProgram::new(&[Token::Block(0, Block::Open { close_position: 0 })]);
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::MissingCloseBlock)));
    }

    #[test]
    fn closing_incorrect_open_block() {
        let program = PileProgram::new(&[
            Token::Block(0, Block::Open { close_position: 2 }),
            Token::Block(1, Block::Open { close_position: 3 }),
            Token::Block(2, Block::Close { open_position: 0 }),
            Token::Block(3, Block::Close { open_position: 1 }),
        ]);
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::ClosingIncorrectBlock)));
    }

    #[test]
    fn block_leaves_items_on_the_stack() {
        let program = PileProgram::new(&[
            Token::Constant(0, Value::Bool(true)),
            Token::If(1),
            Token::Block(2, Block::Open { close_position: 4 }),
            Token::Constant(3, Value::I32(10)),
            Token::Block(4, Block::Close { open_position: 2 }),
        ]);
        let result = program.type_check();
        assert!(matches!(
            result,
            Err(TypeCheckError::NonEmptyStackAfterBlock)
        ));
    }

    #[test]
    fn block_consumes_items_on_stack() {
        let program = PileProgram::new(&[
            Token::Constant(0, Value::Bool(true)),
            Token::If(1),
            Token::Block(2, Block::Open { close_position: 5 }),
            Token::Constant(3, Value::Char('a')),
            Token::Routine(4, Routine::new_intrinsic(IntrinsicRoutine::Print)),
            Token::Block(5, Block::Close { open_position: 2 }),
        ]);
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn if_test() {
        let program = PileProgram::new(&[
            Token::Constant(0, Value::Bool(true)),
            Token::If(1),
            Token::Block(2, Block::Open { close_position: 3 }),
            Token::Block(3, Block::Close { open_position: 2 }),
        ]);
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn if_fails_after_non_bool() {
        let program = PileProgram::new(&[
            Token::Constant(0, Value::Char('a')),
            Token::If(1),
            Token::Block(2, Block::Open { close_position: 3 }),
            Token::Block(3, Block::Close { open_position: 2 }),
        ]);
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::IncorrectType)));
    }

    #[test]
    fn if_empty_stack() {
        let program = PileProgram::new(&[
            Token::If(0),
            Token::Block(1, Block::Open { close_position: 2 }),
            Token::Block(2, Block::Close { open_position: 1 }),
        ]);
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::NotEnoughItems)));
    }

    #[test]
    fn if_fails_when_not_before_block() {
        let program = PileProgram::new(&[
            Token::Constant(0, Value::Bool(true)),
            Token::If(1),
            Token::Constant(2, Value::I32(10)),
        ]);
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::IfNotBeforeBlock)));
    }

    #[test]
    fn while_test() {
        let program = PileProgram::new(&[
            Token::Constant(0, Value::String("This gets printed in the loop".to_owned())),
            Token::Constant(1, Value::Bool(true)),
            Token::While(2),
            Token::Block(3, Block::Open { close_position: 7 }),
            Token::Routine(4, Routine::new_intrinsic(IntrinsicRoutine::Clone)),
            Token::Routine(5, Routine::new_intrinsic(IntrinsicRoutine::Print)),
            Token::Constant(6, Value::Bool(false)),
            Token::Block(7, Block::Close { open_position: 3 }),
        ]);
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn while_fails_after_non_bool() {
        let program = PileProgram::new(&[
            Token::Constant(0, Value::Char('a')),
            Token::While(1),
            Token::Block(2, Block::Open { close_position: 3 }),
            Token::Block(3, Block::Close { open_position: 2 }),
        ]);
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::IncorrectType)));
    }

    #[test]
    fn while_empty_stack() {
        let program = PileProgram::new(&[
            Token::While(0),
            Token::Block(1, Block::Open { close_position: 2 }),
            Token::Block(2, Block::Close { open_position: 1 }),
        ]);
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::NotEnoughItems)));
    }

    #[test]
    fn while_fails_when_not_before_block() {
        let program = PileProgram::new(&[
            Token::Constant(0, Value::Bool(true)),
            Token::While(1),
            Token::Constant(1, Value::Bool(true)),
        ]);
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::IfNotBeforeBlock)));
    }

    #[test]
    fn generic_succeeds() {
        let program = PileProgram::new(&[
            Token::Constant(0, Value::String("Some String".to_owned())),
            Token::Constant(1, Value::Char('a')),
            Token::Routine(2, Routine::new_intrinsic(IntrinsicRoutine::Clone)),
            Token::Routine(2, Routine::new_intrinsic(IntrinsicRoutine::Eq)),
            Token::Routine(3, Routine::new_intrinsic(IntrinsicRoutine::Print)),
            Token::Routine(4, Routine::new_intrinsic(IntrinsicRoutine::Print)),
        ]);
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn generic_fail_empty_stack() {
        let program = PileProgram::new(&[Token::Routine(
            0,
            Routine::new_intrinsic(IntrinsicRoutine::Eq),
        )]);
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::NotEnoughItems)));
    }

    #[test]
    fn generic_incorrect_types() {
        let program = PileProgram::new(&[
            Token::Constant(0, Value::Char('a')),
            Token::Constant(1, Value::String("Some String".to_owned())),
            Token::Routine(2, Routine::new_intrinsic(IntrinsicRoutine::Eq)),
        ]);
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::IncorrectType)));
    }
}
