mod parsing;
mod routines;
mod type_checking;

use parsing::{parse_input};
use routines::{Routine, run_routine};
use type_checking::type_check;
use std::collections::{HashMap,VecDeque};
use std::env;
use std::io::prelude::*;

fn main() {
    let args: Vec<_> = env::args().collect();
    let Some(file) = args.get(1) else {
        todo!("Usage other than single file input");
    };
    if args.len() != 2 {
        todo!("Usage other than single file input");
    }
    println!("{}", file);

    let mut contents = String::new();
    let mut file = std::fs::File::open(&file).unwrap();
    file.read_to_string(&mut contents).unwrap();
    println!("{}", contents);
    
    let tokens = match parsing::parse_input(&contents) {
        Ok(tokens) => tokens,
        Err(err) => {
            println!("Error parsing: {:?}", err);
            return;
        }
    };
    println!("{:?}", tokens);
    if let Err(err) = type_check(&tokens) {
        println!("Type Checking Failed: {:?}", err);
        return;
    }

    let output_stack = run(&tokens);
    println!("{:?}", output_stack);
}

fn run(tokens: &[Token]) -> Stack {
    let mut stack = Stack::new();
    let mut continue_until_block_closes = None;
    for token in tokens.iter() {
        match (continue_until_block_closes, token) {
            (Some(open_position), Token::Block(_, Block::Close { open_position: block_open_position })) if *block_open_position == open_position => {
                continue_until_block_closes = None;
            },
            (None, _) => (),
            (Some(_), _) => {continue;}
        }
        match token {
            Token::Constant(_, value) => stack.push(value.clone()),
            Token::Routine(_, routine) => run_routine(routine, &mut stack),
            Token::Block(_, block) => (),
            Token::If(position) => {
                if let Ok(false) = stack.pop_bool() {
                    // position + 1 assumes type checking ensured that if is followed by opening
                    // block
                    continue_until_block_closes = Some(position + 1);
                }
            }
        }
    }
    stack
}

#[derive(PartialEq, Debug, Clone)]
enum Token {
    Constant(usize, Value),
    Routine(usize, Routine),
    Block(usize, Block),
    If(usize),
}

#[derive(PartialEq, Debug, Clone)]
enum Block {
    Open,
    Close { open_position: usize },
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum Type {
    I32,
    Char,
    String,
    Bool,
}

#[derive(PartialEq, Debug, Clone)]
enum Value {
    I32(i32),
    Char(char),
    String(String),
    Bool(bool),
}

#[derive(Debug, PartialEq)]
struct Stack(VecDeque<Value>);

#[derive(Debug)]
enum PopError {
    StackEmpty,
    InvalidType
}

impl Stack {
    fn new() -> Self {
        Self(VecDeque::new())
    }

    fn from_values(values: &[Value]) -> Self {
        Self(values.iter().cloned().collect())
    }

    fn push(&mut self, value: Value) {
        self.0.push_back(value);
    }

    fn pop(&mut self) -> Option<Value> {
        self.0.pop_back()
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn pop_i32(&mut self) -> Result<i32, PopError> {
        match self.pop() {
            Some(Value::I32(value)) => Ok(value),
            Some(_) => Err(PopError::InvalidType),
            None => Err(PopError::StackEmpty)
        }
    }

    fn pop_char(&mut self) -> Result<char, PopError> {
        match self.pop() {
            Some(Value::Char(value)) => Ok(value),
            Some(_) => Err(PopError::InvalidType),
            None => Err(PopError::StackEmpty),
        }
    }

    fn pop_string(&mut self) -> Result<String, PopError> {
        match self.pop() {
            Some(Value::String(value)) => Ok(value),
            Some(_) => Err(PopError::InvalidType),
            None => Err(PopError::StackEmpty),
        }
    }

    fn pop_bool(&mut self) -> Result<bool, PopError> {
        match self.pop() {
            Some(Value::Bool(value)) => Ok(value),
            Some(_) => Err(PopError::InvalidType),
            None => Err(PopError::StackEmpty),
        }
    }
}
