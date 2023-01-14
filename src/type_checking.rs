use crate::{routines::RoutineSigniture, Block, PileProgram, Routine, Token, Type, Value};
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
    MissingRoutine,
    RoutineExtraOutput,
    RoutineMissingOutput,
    RoutineIncorrectOutputType,
}

impl PileProgram {
    pub(crate) fn type_check(&self) -> Result<(), TypeCheckError> {
        self.type_check_with_stack(&[])?;
        for (routine_name, routine) in self.routines.iter() {
            if let Routine::Pile { signiture, routine: tokens } = routine {
                let mini_program = PileProgram::new(tokens, self.routines.clone());
                let output_stack = mini_program.type_check_with_stack(signiture.inputs())?;

                let mut expected_outputs = signiture.outputs().iter();
                let mut actual_outputs = output_stack.iter();
                while let Some(expected_output_type) = expected_outputs.next() {
                    let Some(actual_output_type) = actual_outputs.next() else {
                        return Err(TypeCheckError::RoutineMissingOutput);
                    };
                    if dbg!(actual_output_type) != dbg!(expected_output_type) {
                        return Err(TypeCheckError::RoutineIncorrectOutputType);
                    }
                }
                if actual_outputs.count() > 0 {
                    println!("3");
                    return Err(TypeCheckError::RoutineExtraOutput);
                }
            }
        }
        Ok(())
    }

    fn type_check_with_stack(&self, existing_stack: &[Type]) -> Result<Box<[Type]>, TypeCheckError> {
        let tokens = &self.tokens;
        let mut type_stack = existing_stack.to_vec();
        let mut block_stack: Vec<(usize, Vec<Type>)> = Vec::new();
        let mut after_condition_checker = false;
        for (index, token) in tokens.iter().enumerate() {
            if after_condition_checker
                && !matches!(&token, Token::Block(Block::Open { close_position: _ }))
            {
                return Err(TypeCheckError::IfNotBeforeBlock);
            }
            after_condition_checker = false;
            match token {
                Token::Constant(Value::I32(_)) => type_stack.push(Type::I32),
                Token::Constant(Value::Char(_)) => {
                    type_stack.push(Type::Char);
                }
                Token::Constant(Value::String(_)) => {
                    type_stack.push(Type::String);
                }
                Token::Constant(Value::Bool(_)) => {
                    type_stack.push(Type::Bool);
                }
                Token::Block(Block::Open { close_position }) => {
                    block_stack.push((index, type_stack.clone()));
                }
                Token::Block(Block::Close { open_position }) => {
                    let Some((open_block, mut open_stack_state)) = block_stack.pop() else {
                    return Err(TypeCheckError::MissingOpenBlock);
                };
                    if open_block != *open_position {
                        return Err(TypeCheckError::ClosingIncorrectBlock);
                    }
                    if let Token::While = &tokens[open_position - 1] {
                        // while loop expects a bool at the top
                        open_stack_state.push(Type::Bool);
                    }
                    if type_stack != open_stack_state {
                        return Err(TypeCheckError::NonEmptyStackAfterBlock);
                    }
                }
                Token::If => {
                    after_condition_checker = true;
                    match type_stack.pop() {
                        None => return Err(TypeCheckError::NotEnoughItems),
                        Some(Type::Bool) => (),
                        _ => return Err(TypeCheckError::IncorrectType),
                    }
                }
                Token::While => {
                    after_condition_checker = true;
                    match type_stack.pop() {
                        None => return Err(TypeCheckError::NotEnoughItems),
                        Some(Type::Bool) => (),
                        _ => return Err(TypeCheckError::IncorrectType),
                    }
                }
                Token::RoutineCall(routine_name) => {
                    let Some(routine) = self.routines.get(routine_name) else {
                        return Err(TypeCheckError::MissingRoutine);
                    };
                    type_check_routine(routine.signiture(), &mut type_stack)?;
                }
            }
        }
        if !block_stack.is_empty() {
            return Err(TypeCheckError::MissingCloseBlock);
        }
        Ok(type_stack.into_boxed_slice())
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
        let program = PileProgram::new(&[], HashMap::new());
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn constast_pushing_should_succeed() {
        let program = PileProgram::new(&[Token::Constant(Value::I32(10))], HashMap::new());
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn intrinsic_routine_call_should_succeed() {
        let program = PileProgram::new(&[
            Token::Constant(Value::I32(10)),
            Token::Constant(Value::I32(10)),
            Token::RoutineCall("add".to_owned()),
        ], HashMap::new());
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn pile_routine_call_should_succeed() {
        let program = PileProgram::new(&[
            Token::Constant(Value::I32(10)),
            Token::RoutineCall("Something".to_owned()),
            Token::RoutineCall("print".to_owned()),
        ], [("Something".to_owned(), Routine::Pile {
                signiture: RoutineSigniture::new("Something", &[Type::I32], &[Type::String]),
                routine: vec![
                    Token::RoutineCall("print".to_owned()),
                    Token::Constant(Value::String("Hello World".to_owned())),
                ].into_boxed_slice(),
            })].into_iter().collect());
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn routine_call_should_fail_when_not_enough_tokens() {
        let program = PileProgram::new(&[
            Token::Constant(Value::I32(10)),
            Token::RoutineCall("add".to_owned()),
        ], HashMap::new());
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::NotEnoughItems)));
    }

    #[test]
    fn routine_call_should_fail_when_incorrect_types() {
        let program = PileProgram::new(&[
            Token::Constant(Value::I32(10)),
            Token::Constant(Value::Char('a')),
            Token::RoutineCall("add".to_owned()),
        ], HashMap::new());
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::IncorrectType)));
    }

    #[test]
    fn missing_routine() {
        let program = PileProgram::new(&[
            Token::RoutineCall("missing_fn".to_owned()),
        ], HashMap::new());
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::MissingRoutine)));
    }

    #[test]
    fn block_succeeds() {
        let program = PileProgram::new(&[
            Token::Constant(Value::I32(10)),
            Token::Block(Block::Open { close_position: 4 }),
            Token::Constant(Value::Char('a')),
            Token::RoutineCall("print".to_owned()),
            Token::Block(Block::Close { open_position: 1 }),
            Token::Constant(Value::String("Hello World".to_owned())),
        ], HashMap::new());
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn missing_opening_block() {
        let program = PileProgram::new(&[Token::Block(Block::Close { open_position: 0 })], HashMap::new());
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::MissingOpenBlock)));
    }

    #[test]
    fn missing_closing_block() {
        let program = PileProgram::new(&[Token::Block(Block::Open { close_position: 0 })], HashMap::new());
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::MissingCloseBlock)));
    }

    #[test]
    fn closing_incorrect_open_block() {
        let program = PileProgram::new(&[
            Token::Block(Block::Open { close_position: 2 }),
            Token::Block(Block::Open { close_position: 3 }),
            Token::Block(Block::Close { open_position: 0 }),
            Token::Block(Block::Close { open_position: 1 }),
        ], HashMap::new());
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::ClosingIncorrectBlock)));
    }

    #[test]
    fn block_leaves_items_on_the_stack() {
        let program = PileProgram::new(&[
            Token::Constant(Value::Bool(true)),
            Token::If,
            Token::Block(Block::Open { close_position: 4 }),
            Token::Constant(Value::I32(10)),
            Token::Block(Block::Close { open_position: 2 }),
        ], HashMap::new());
        let result = program.type_check();
        assert!(matches!(
            result,
            Err(TypeCheckError::NonEmptyStackAfterBlock)
        ));
    }

    #[test]
    fn block_consumes_items_on_stack() {
        let program = PileProgram::new(&[
            Token::Constant(Value::Bool(true)),
            Token::If,
            Token::Block(Block::Open { close_position: 5 }),
            Token::Constant(Value::Char('a')),
            Token::RoutineCall("print".to_owned()),
            Token::Block(Block::Close { open_position: 2 }),
        ], HashMap::new());
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn if_test() {
        let program = PileProgram::new(&[
            Token::Constant(Value::Bool(true)),
            Token::If,
            Token::Block(Block::Open { close_position: 3 }),
            Token::Block(Block::Close { open_position: 2 }),
        ], HashMap::new());
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn if_fails_after_non_bool() {
        let program = PileProgram::new(&[
            Token::Constant(Value::Char('a')),
            Token::If,
            Token::Block(Block::Open { close_position: 3 }),
            Token::Block(Block::Close { open_position: 2 }),
        ], HashMap::new());
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::IncorrectType)));
    }

    #[test]
    fn if_empty_stack() {
        let program = PileProgram::new(&[
            Token::If,
            Token::Block(Block::Open { close_position: 2 }),
            Token::Block(Block::Close { open_position: 1 }),
        ], HashMap::new());
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::NotEnoughItems)));
    }

    #[test]
    fn if_fails_when_not_before_block() {
        let program = PileProgram::new(&[
            Token::Constant(Value::Bool(true)),
            Token::If,
            Token::Constant(Value::I32(10)),
        ], HashMap::new());
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::IfNotBeforeBlock)));
    }

    #[test]
    fn while_test() {
        let program = PileProgram::new(&[
            Token::Constant(Value::String("This gets printed in the loop".to_owned())),
            Token::Constant(Value::Bool(true)),
            Token::While,
            Token::Block(Block::Open { close_position: 7 }),
            Token::RoutineCall("clone".to_owned()),
            Token::RoutineCall("print".to_owned()),
            Token::Constant(Value::Bool(false)),
            Token::Block(Block::Close { open_position: 3 }),
        ], HashMap::new());
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn while_fails_after_non_bool() {
        let program = PileProgram::new(&[
            Token::Constant(Value::Char('a')),
            Token::While,
            Token::Block(Block::Open { close_position: 3 }),
            Token::Block(Block::Close { open_position: 2 }),
        ], HashMap::new());
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::IncorrectType)));
    }

    #[test]
    fn while_empty_stack() {
        let program = PileProgram::new(&[
            Token::While,
            Token::Block(Block::Open { close_position: 2 }),
            Token::Block(Block::Close { open_position: 1 }),
        ], HashMap::new());
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::NotEnoughItems)));
    }

    #[test]
    fn while_fails_when_not_before_block() {
        let program = PileProgram::new(&[
            Token::Constant(Value::Bool(true)),
            Token::While,
            Token::Constant(Value::Bool(true)),
        ], HashMap::new());
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::IfNotBeforeBlock)));
    }

    #[test]
    fn generic_succeeds() {
        let program = PileProgram::new(&[
            Token::Constant(Value::String("Some String".to_owned())),
            Token::Constant(Value::Char('a')),
            Token::RoutineCall("clone".to_owned()),
            Token::RoutineCall("eq".to_owned()),
            Token::RoutineCall("print".to_owned()),
            Token::RoutineCall("print".to_owned()),
        ], HashMap::new());
        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn generic_fail_empty_stack() {
        let program = PileProgram::new(&[Token::RoutineCall(
            "eq".to_owned()
        )], HashMap::new());
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::NotEnoughItems)));
    }

    #[test]
    fn generic_incorrect_types() {
        let program = PileProgram::new(&[
            Token::Constant(Value::Char('a')),
            Token::Constant(Value::String("Some String".to_owned())),
            Token::RoutineCall("eq".to_owned()),
        ], HashMap::new());
        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::IncorrectType)));
    }

    #[test]
    fn routine_definition_successful() {
        let program = PileProgram::new(&[
            Token::Constant(Value::String("Hello World".to_owned())),
            Token::RoutineCall("my_routine".to_owned()),
        ], [(
            "my_routine".to_owned(),
            Routine::Pile {
                signiture: RoutineSigniture::new("my_routine", &vec![Type::String], &vec![Type::I32]),
                routine: vec![
                    Token::RoutineCall("print".to_owned()),
                    Token::Constant(Value::I32(10)),
                ].into_boxed_slice()
            }
        )].into_iter().collect());

        let result = program.type_check();
        assert!(result.is_ok());
    }

    #[test]
    fn routine_definition_missing_output() {
        let program = PileProgram::new(&[
            Token::Constant(Value::String("Hello World".to_owned())),
            Token::RoutineCall("my_routine".to_owned()),
        ], [(
            "my_routine".to_owned(),
            Routine::Pile {
                signiture: RoutineSigniture::new("my_routine", &vec![Type::String], &vec![Type::I32]),
                routine: vec![
                    Token::RoutineCall("print".to_owned()),
                ].into_boxed_slice()
            }
        )].into_iter().collect());

        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::RoutineMissingOutput)));
    }

    #[test]
    fn routine_definition_incorrect_output() {
        let program = PileProgram::new(&[
            Token::Constant(Value::String("Hello World".to_owned())),
            Token::RoutineCall("my_routine".to_owned()),
        ], [(
            "my_routine".to_owned(),
            Routine::Pile {
                signiture: RoutineSigniture::new("my_routine", &vec![Type::String], &vec![Type::I32]),
                routine: vec![
                    Token::RoutineCall("print".to_owned()),
                    Token::Constant(Value::String("Hello World".to_owned())),
                ].into_boxed_slice()
            }
        )].into_iter().collect());

        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::RoutineIncorrectOutputType)));
    }

    #[test]
    fn routine_definition_extra_output() {
        let program = PileProgram::new(&[
            Token::Constant(Value::String("Hello World".to_owned())),
            Token::RoutineCall("my_routine".to_owned()),
        ], [(
            "my_routine".to_owned(),
            Routine::Pile {
                signiture: RoutineSigniture::new("my_routine", &vec![Type::String], &vec![Type::I32]),
                routine: vec![
                    Token::RoutineCall("print".to_owned()),
                    Token::Constant(Value::I32(10)),
                    Token::Constant(Value::I32(10)),
                ].into_boxed_slice()
            }
        )].into_iter().collect());

        let result = program.type_check();
        assert!(matches!(result, Err(TypeCheckError::RoutineExtraOutput)));
    }
}
