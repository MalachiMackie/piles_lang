use crate::{Stack, Token, Type, Value, Block};
use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone)]
pub(crate) enum Routine {
    Pile {
        signiture: RoutineSigniture,
        routine: Box<[Token]>,
    },
    Intrinsic {
        signiture: RoutineSigniture,
        routine: IntrinsicRoutine,
    },
}

impl Routine {
    pub(crate) fn signiture(&self) -> &RoutineSigniture {
        match self {
            Routine::Intrinsic {
                signiture,
                routine: _,
            }
            | Routine::Pile {
                signiture,
                routine: _,
            } => signiture,
        }
    }

    pub(crate) fn new_intrinsic(routine: IntrinsicRoutine) -> Self {
        Self::Intrinsic {
            signiture: RoutineSigniture::from_intrinsic(&routine),
            routine,
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub(crate) struct RoutineSigniture {
    name: String,
    inputs: Box<[Type]>,
    outputs: Box<[Type]>,
}

impl RoutineSigniture {
    pub(crate) fn new(name: &str, inputs: &[Type], outputs: &[Type]) -> Self {
        Self {
            name: name.to_owned(),
            inputs: inputs
                .iter()
                .cloned()
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            outputs: outputs
                .iter()
                .cloned()
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn from_intrinsic(routine: &IntrinsicRoutine) -> Self {
        match routine {
            IntrinsicRoutine::AddI32 => Self {
                inputs: vec![Type::I32, Type::I32].into_boxed_slice(),
                outputs: vec![Type::I32].into_boxed_slice(),
                name: "add".to_owned(),
            },
            IntrinsicRoutine::MinusI32 => Self {
                inputs: vec![Type::I32, Type::I32].into_boxed_slice(),
                outputs: vec![Type::I32].into_boxed_slice(),
                name: "minus".to_owned(),
            },
            IntrinsicRoutine::Print => Self {
                inputs: vec![Type::Generic {
                    name: "A".to_owned(),
                }]
                .into_boxed_slice(),
                outputs: Vec::new().into_boxed_slice(),
                name: "print".to_owned(),
            },
            IntrinsicRoutine::Eq => Self {
                inputs: vec![
                    Type::Generic {
                        name: "A".to_owned(),
                    },
                    Type::Generic {
                        name: "A".to_owned(),
                    },
                ]
                .into_boxed_slice(),
                outputs: vec![Type::Bool].into_boxed_slice(),
                name: "eq".to_owned(),
            },
            IntrinsicRoutine::GreaterThan => Self {
                inputs: vec![Type::I32, Type::I32].into_boxed_slice(),
                outputs: vec![Type::Bool].into_boxed_slice(),
                name: "gt".to_owned(),
            },
            IntrinsicRoutine::Not => Self {
                inputs: vec![Type::Bool].into_boxed_slice(),
                outputs: vec![Type::Bool].into_boxed_slice(),
                name: "not".to_owned(),
            },
            IntrinsicRoutine::Clone => Self {
                inputs: vec![Type::Generic {
                    name: "A".to_owned(),
                }]
                .into_boxed_slice(),
                outputs: vec![
                    Type::Generic {
                        name: "A".to_owned(),
                    },
                    Type::Generic {
                        name: "A".to_owned(),
                    },
                ]
                .into_boxed_slice(),
                name: "clone".to_owned(),
            },
            IntrinsicRoutine::Swap => Self {
                inputs: vec![
                    Type::Generic {
                        name: "A".to_owned(),
                    },
                    Type::Generic {
                        name: "B".to_owned(),
                    },
                ]
                .into_boxed_slice(),
                outputs: vec![
                    Type::Generic {
                        name: "A".to_owned(),
                    },
                    Type::Generic {
                        name: "B".to_owned(),
                    },
                ]
                .into_boxed_slice(),
                name: "swap".to_owned(),
            },
            IntrinsicRoutine::Drop => Self {
                inputs: vec![
                    Type::Generic { name: "A".to_owned() }
                ].into_boxed_slice(),
                outputs: Vec::new().into_boxed_slice(),
                name: "drop".to_owned(),
            },
            IntrinsicRoutine::CloneOver => Self {
                inputs: vec![
                    Type::Generic { name: "A".to_owned() },
                    Type::Generic { name: "B".to_owned() },
                ].into_boxed_slice(),
                outputs: vec![
                    Type::Generic { name: "B".to_owned() },
                    Type::Generic { name: "A".to_owned() },
                    Type::Generic { name: "B".to_owned() },
                ].into_boxed_slice(),
                name: "clone_over".to_owned(),
            }
        }
    }

    pub(crate) fn inputs(&self) -> &[Type] {
        self.inputs.as_ref()
    }

    pub(crate) fn outputs(&self) -> &[Type] {
        self.outputs.as_ref()
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) enum IntrinsicRoutine {
    AddI32,
    MinusI32,
    Print,
    Eq,
    GreaterThan,
    Not,
    Clone,
    Swap,
    Drop,
    CloneOver,
}

impl IntrinsicRoutine {
    pub(crate) fn get_routine_dictionary() -> HashMap<String, Routine> {
        let intrinsics = [
            Routine::new_intrinsic(IntrinsicRoutine::AddI32),
            Routine::new_intrinsic(IntrinsicRoutine::MinusI32),
            Routine::new_intrinsic(IntrinsicRoutine::Print),
            Routine::new_intrinsic(IntrinsicRoutine::Eq),
            Routine::new_intrinsic(IntrinsicRoutine::GreaterThan),
            Routine::new_intrinsic(IntrinsicRoutine::Not),
            Routine::new_intrinsic(IntrinsicRoutine::Clone),
            Routine::new_intrinsic(IntrinsicRoutine::Swap),
            Routine::new_intrinsic(IntrinsicRoutine::Drop),
            Routine::new_intrinsic(IntrinsicRoutine::CloneOver),
        ];

        intrinsics.into_iter()
            .map(|routine| (routine.signiture().name.clone(), routine))
            .collect()
    }

    pub(crate) fn run(&self, stack: &mut Stack) {
        match self {
        IntrinsicRoutine::AddI32 => {
            let a = stack.pop_i32().expect("Type checking failed");
            let b = stack.pop_i32().expect("Type checking failed");
            stack.push(Value::I32(a + b));
        }
        IntrinsicRoutine::MinusI32 => {
            let a = stack.pop_i32().expect("Type checking failed");
            let b = stack.pop_i32().expect("Type checking failed");
            stack.push(Value::I32(a - b));
        }
        IntrinsicRoutine::Print => {
            let a = stack.pop().expect("Type checking failed");
            println!("{}", a);
        }
        IntrinsicRoutine::Eq => {
            let a = stack.pop().expect("Type checking failed");
            let b = stack.pop().expect("Type checking failed");
            stack.push(Value::Bool(a == b));
        }
        IntrinsicRoutine::Not => {
            let a = stack.pop_bool().expect("Type checking_failed");
            stack.push(Value::Bool(!a));
        }
        IntrinsicRoutine::Clone => {
            let a = stack.pop().expect("Type checking failed");
            stack.push(a.clone());
            stack.push(a);
        }
        IntrinsicRoutine::Swap => {
            let a = stack.pop().expect("Type checking failed");
            let b = stack.pop().expect("Type checking failed");
            stack.push(a);
            stack.push(b);
        }
        IntrinsicRoutine::Drop => {
            stack.pop().expect("Type checking failed");
        },
        IntrinsicRoutine::CloneOver => {
            let a = stack.pop().expect("Type checking failed");
            let b = stack.pop().expect("Type checking failed");
            stack.push(b.clone());
            stack.push(a);
            stack.push(b);
        }
        IntrinsicRoutine::GreaterThan => {
            let a = stack.pop_i32().expect("Type checking failed");
            let b = stack.pop_i32().expect("Type checking failed");
            stack.push(Value::Bool(a > b));
        }
    }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_routine(input_stack: &[Value], routine: IntrinsicRoutine, expected_stack: &[Value]) {
        let mut stack = Stack::from_values(input_stack);
        routine.run(&mut stack);
        let expected_stack = Stack::from_values(expected_stack);
        assert_eq!(stack, expected_stack);
    }

    #[test]
    #[should_panic]
    fn run_routine_should_panic_when_stack_doesnt_have_enough_items() {
        let mut stack = Stack::from_values(&vec![Value::I32(10)]);
        let routine = IntrinsicRoutine::AddI32;
        routine.run(&mut stack);
    }

    #[test]
    fn test_add() {
        test_routine(&[Value::I32(10), Value::I32(15)], IntrinsicRoutine::AddI32, &[Value::I32(25)]);
    }

    #[test]
    fn test_not() {
        test_routine(&[Value::Bool(true)], IntrinsicRoutine::Not, &[Value::Bool(false)]);
        test_routine(&[Value::Bool(false)], IntrinsicRoutine::Not, &[Value::Bool(true)]);
    }

    #[test]
    fn test_clone() {
        test_routine(&[Value::Bool(true)], IntrinsicRoutine::Clone, &[Value::Bool(true), Value::Bool(true)]);
        test_routine(&[Value::String("Hello World".to_owned())], IntrinsicRoutine::Clone, &[Value::String("Hello World".to_owned()), Value::String("Hello World".to_owned())]);
    }

    #[test]
    fn test_swap() {
        test_routine(&[Value::Bool(true), Value::I32(35)], IntrinsicRoutine::Swap, &[Value::I32(35), Value::Bool(true)]);
    }

    #[test]
    fn test_drop() {
        test_routine(&[Value::Bool(true)], IntrinsicRoutine::Drop, &[]);
        test_routine(&[Value::I32(145)], IntrinsicRoutine::Drop, &[]);
        test_routine(&[Value::I32(145), Value::String("Hi".to_owned())], IntrinsicRoutine::Drop, &[Value::I32(145)]);
    }

    #[test]
    fn test_minus() {
        test_routine(&[Value::I32(10), Value::I32(5)], IntrinsicRoutine::MinusI32, &[Value::I32(-5)]);
    }

    #[test]
    fn test_print_char() {
        // todo: test std::out
        let mut stack = Stack::from_values(&vec![Value::Char('a')]);
        let routine = IntrinsicRoutine::Print;
        routine.run(&mut stack);
        let expected_stack = Stack::new();
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_print_string() {
        let mut stack = Stack::from_values(&vec![Value::String("Hello World!".to_owned())]);
        let routine = IntrinsicRoutine::Print;
        routine.run(&mut stack);
        let expected_stack = Stack::new();
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_clone_over() {
        test_routine(&[Value::String("Hi".to_owned()), Value::I32(15)], IntrinsicRoutine::CloneOver, &[Value::String("Hi".to_owned()), Value::I32(15), Value::String("Hi".to_owned())]);
    }

    #[test]
    fn test_greater_than() {
        test_routine(&[Value::I32(15), Value::I32(10)], IntrinsicRoutine::GreaterThan, &[Value::Bool(false)]);
        test_routine(&[Value::I32(10), Value::I32(15)], IntrinsicRoutine::GreaterThan, &[Value::Bool(true)]);
    }
}
