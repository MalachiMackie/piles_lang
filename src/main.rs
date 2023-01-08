mod parsing;
mod routines;
mod type_checking;

use parsing::{parse_input};
use routines::{Routine, run_routine};
use type_checking::type_check;
use std::collections::{HashMap,VecDeque};
use std::env;
use std::io::prelude::*;
use std::fmt::{Display, Formatter};

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
    if tokens.is_empty() {
        return stack;
    }
    let mut index = 0;
    while index < tokens.len() {
        let token = &tokens[index];
        match token {
            Token::Constant(_, value) => {
                stack.push(value.clone());
            },
            Token::Routine(_, routine) => {
                run_routine(&routine, &mut stack);
            },
            Token::Block(position, Block::Open { close_position }) => {
                
            },
            Token::Block(_, Block::Close { open_position }) => {
                if let Token::While(while_position) = &tokens[open_position - 1] {
                    index = while_position - 1;
                }
            },
            Token::If(_) | Token::While(_) => {
                let Token::Block(_, Block::Open { close_position }) = &tokens[index + 1] else {
                    panic!("Expected open block after if or while");
                };

                if !stack.pop_bool().expect("Type Checking Failed") {
                    index = *close_position;
                }
            },
        }
        index += 1;
    }
    stack
}

#[derive(PartialEq, Debug, Clone)]
enum Token {
    Constant(usize, Value),
    Routine(usize, Routine),
    Block(usize, Block),
    If(usize),
    While(usize),
}

#[derive(PartialEq, Debug, Clone)]
enum Block {
    Open { close_position: usize },
    Close { open_position: usize },
}

#[derive(PartialEq, Debug, Clone)]
enum Type {
    Generic { name: String },
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

impl Display for Value {
    fn fmt(&self, mut formatter: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::I32(value) => value.fmt(&mut formatter),
            Value::Char(value) => value.fmt(&mut formatter),
            Value::String(value) => value.fmt(&mut formatter),
            Value::Bool(value) => value.fmt(&mut formatter),
        }
    }
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
