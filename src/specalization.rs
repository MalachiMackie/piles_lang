use std::collections::{HashMap, BTreeSet, BTreeMap};

use crate::{PileProgram, routines::{RoutineSigniture, Routine}, Type, Token, Block};

type RoutineReplacement = (Routine, BTreeMap<String, Type>);

impl PileProgram {
    pub(crate) fn specalize(&mut self) {
        let routine_calls = Self::traverse_calls(&self.tokens, self.routines.clone(), Vec::new());
        let keys: Vec<_> = self.routines.keys().cloned().collect();

        let original_routines = self.routines.clone();

        let mut new_routines: HashMap<String, Vec<_>> = HashMap::new();

        for routine_name in keys {
            let routine = self.routines.get(&routine_name).unwrap();
            let Routine::Pile { signiture, routine: tokens } = routine else {
                continue;
            };

            let Some(generic_inputs) = routine_calls.get(signiture) else {
                self.routines.remove(&routine_name);
                continue;
            };
            
            if generic_inputs.is_empty() {
                // is used, but not generic
                continue;
            }

            for generic_input in generic_inputs {
                let mut input_strings = vec![routine_name.clone()];
                let inputs: Vec<_> = signiture.inputs().iter().map(|input| {
                    match input {
                        Type::Generic { name } => {
                            let input_type = generic_input.get(name).unwrap().clone();
                            input_strings.push(format!("{input_type}"));
                            input_type
                        },
                        _ => input.clone()
                    }
                }).collect();
                let outputs: Vec<_> = signiture.outputs().iter().map(|output| {
                    match output {
                        Type::Generic { name } => {
                            generic_input.get(name).unwrap().clone()
                        },
                        _ => output.clone()
                    }
                }).collect();

                let new_signiture = RoutineSigniture::new(&input_strings.join("-"), &inputs, &outputs);
                let new_routine = Routine::Pile { signiture: new_signiture, routine: tokens.clone() };
                if let Some(new_routines) = new_routines.get_mut(&routine_name) {
                    new_routines.push((new_routine, generic_input.clone()));
                } else {
                    new_routines.insert(routine_name.clone(), vec![(new_routine, generic_input.clone())]);
                }
            }

            self.routines.remove(&routine_name);
        }

        for (_, new_routines) in new_routines.iter() {
            for (new_routine, _generic_inputs) in new_routines {
                self.routines.insert(new_routine.signiture().name().to_owned(), new_routine.clone());
            }
        }

        let replacement_tokens = Self::replace_generic_calls(&self.tokens, Vec::new(), &original_routines, &new_routines);
        self.tokens = replacement_tokens;

        for routine in self.routines.values_mut() {
            if let Routine::Pile { signiture, routine: routine_tokens } = routine {
                let replacement_tokens = Self::replace_generic_calls(routine_tokens, signiture.inputs().into(), &original_routines, &new_routines);
                *routine_tokens = replacement_tokens;
            }
        }
    }

    fn replace_generic_calls(input_tokens: &[Token], mut type_stack: Vec<Type>, original_routines: &HashMap<String, Routine>, new_routines: &HashMap<String, Vec<RoutineReplacement>>) -> Box<[Token]> {
        let mut replacement_tokens = Vec::new();
        let mut previous_control_flow = None;
        for token in input_tokens.iter() {
            match token {
                Token::Constant(value) => {
                    type_stack.push(value.get_type());
                    replacement_tokens.push(token.clone());
                },
                Token::If | Token::While => {
                    previous_control_flow = Some(token);
                    replacement_tokens.push(token.clone());
                },
                Token::Block(Block::Open { close_position: _ }) => {
                    type_stack.pop();
                    replacement_tokens.push(token.clone());
                },
                Token::Block(Block::Close { open_position: _ }) => {
                    if let Some(Token::While) = previous_control_flow {
                        type_stack.pop();
                    }
                    replacement_tokens.push(token.clone());
                },
                Token::RoutineCall(routine_name) => {
                    let original_routine = original_routines.get(routine_name).unwrap();
                    let signiture = original_routine.signiture();
                    let mut generic_inputs = BTreeMap::new();
                    let mut routine_type_stack = Vec::new();
                    for input in signiture.inputs() {
                        match input {
                            Type::Generic { name } => {
                                let top_type = type_stack.pop().unwrap();
                                routine_type_stack.push(top_type.clone());
                                if !generic_inputs.contains_key(name) {
                                    generic_inputs.insert(name.to_owned(), top_type);
                                }
                            },
                            _ => {
                                routine_type_stack.push(type_stack.pop().unwrap());
                            }
                        }
                    }
                    if let Some(replacements) = new_routines.get(routine_name) {
                        let Some((replacement_routine, _)) = replacements.iter().find(|(_, inputs)| {
                            inputs == &generic_inputs
                        }) else {
                            panic!();
                        };
                        replacement_tokens.push(Token::RoutineCall(replacement_routine.signiture().name().to_owned()));
                    } else {
                        replacement_tokens.push(token.clone());
                    }

                    for output in signiture.outputs() {
                        match output {
                            Type::Generic { name } => {
                                type_stack.push(generic_inputs.get(name).unwrap().clone());
                            },
                            _ => type_stack.push(output.clone())
                        }
                    }
                }
            }
        }

        replacement_tokens.into()
    }

    fn traverse_calls(tokens: &[Token],
                      routines: HashMap<String, Routine>,
                      input_types: Vec<Type>) -> BTreeMap<RoutineSigniture, BTreeSet<BTreeMap<String, Type>>> {
        let mut routine_calls = BTreeMap::new();
        let mut type_stack = input_types;
        let mut previous_control_flow = None;
        for token in tokens.iter() {
            match token {
                Token::Constant(value) => {
                    type_stack.push(value.get_type())
                },
                Token::If | Token::While => previous_control_flow = Some(token),
                Token::Block(Block::Open { close_position: _ }) => {
                    type_stack.pop();
                },
                Token::Block(Block::Close { open_position: _ }) => {
                    if let Some(Token::While) = previous_control_flow {
                        // pop the bool left on the stack after the while loop
                        type_stack.pop();
                    }
                },
                Token::RoutineCall(routine_name) => {
                    let routine = routines.get(routine_name).unwrap();
                    let signiture = routine.signiture();
                    let mut generic_inputs = BTreeMap::new();
                    let mut routine_type_stack = Vec::new();
                    for input in signiture.inputs() {
                        match input {
                            Type::Generic { name } => {
                                let top_type = type_stack.pop().unwrap();
                                routine_type_stack.push(top_type.clone());
                                if !generic_inputs.contains_key(name) {
                                    generic_inputs.insert(name.to_owned(), top_type);
                                }
                            },
                            _ => {
                                routine_type_stack.push(type_stack.pop().unwrap());
                            }
                        }
                    }
                    
                    if let Routine::Pile { signiture, routine: tokens } = routine {
                        if !routine_calls.contains_key(signiture) {
                            routine_calls.insert(signiture.clone(), [generic_inputs.clone()].into());
                        } else {
                            let call_types: &mut BTreeSet<BTreeMap<String, Type>> = routine_calls.get_mut(signiture).unwrap();
                            call_types.insert(generic_inputs.clone());
                        }

                        let inner_routine_calls = Self::traverse_calls(tokens, routines.clone(), routine_type_stack);

                        for (inner_signiture, inner_call_types) in inner_routine_calls {
                            if !routine_calls.contains_key(&inner_signiture) {
                                routine_calls.insert(inner_signiture.clone(), inner_call_types);
                            } else {
                                let call_types = routine_calls.get_mut(&inner_signiture).unwrap();
                                call_types.extend(inner_call_types);
                            }
                        }
                    }

                    for output in signiture.outputs() {
                        match output {
                            Type::Generic { name } => {
                                type_stack.push(generic_inputs.get(name).unwrap().clone());
                            },
                            _ => {
                                type_stack.push(output.clone());
                            }
                        }
                    }
                }
            }
        }

        routine_calls
    }
}
