mod parsing;
mod routines;
mod type_checking;
mod compile;

use routines::{Routine, IntrinsicRoutine, RoutineSigniture};
use std::collections::{HashMap, VecDeque};
use std::env;
use std::fmt::{Display, Formatter};
use std::io::prelude::*;
use clap::{Arg, ArgAction, Command, Parser, crate_name};

const INTERPRET_COMMAND: &str = "interpret";
const COMPILE_COMMAND: &str = "compile";
const FILE_ARG: &str = "file";

fn get_command() -> Command {
    Command::new(crate_name!())
        .about("todo")
        .subcommand_required(true)
        .subcommand(
            Command::new(INTERPRET_COMMAND)
                .about("todo")
                .arg(Arg::new(FILE_ARG).required(true))
        )
        .subcommand(
            Command::new(COMPILE_COMMAND)
                .about("todo")
                .arg(Arg::new(FILE_ARG).required(true))
        )
}

#[derive(Debug)]
enum Operation {
    Interpret,
    Compile
}


fn main() {
    let mut command = get_command();
    let matches = command.clone().get_matches_from(env::args());
    let Some((operation, file_name)) = (match matches.subcommand() {
        Some((INTERPRET_COMMAND, sub_m)) => Some((Operation::Interpret, sub_m.get_one::<String>(FILE_ARG).unwrap())),
        Some((COMPILE_COMMAND, sub_m)) => Some((Operation::Compile, sub_m.get_one::<String>(FILE_ARG).unwrap())),
        _ => None,
    }) else {
        command.print_help();
        return;
    };

    let mut contents = String::new();
    println!("{}", file_name);
    let mut file = std::fs::File::open(&file_name).unwrap();
    println!("{}", contents);
    file.read_to_string(&mut contents).unwrap();

    let program = match PileProgram::parse(&contents) {
        Ok(program) => program,
        Err(err) => {
            println!("Error parsing: {:?}", err);
            return;
        }
    };
    if let Err(err) = program.type_check() {
        println!("Type Checking Failed: {:?}", err);
        return;
    }

    match operation {
        Operation::Interpret => _ = program.run(),
        Operation::Compile => program.compile(file_name).unwrap(),
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct PileProgram {
    tokens: Box<[Token]>,
    routines: HashMap<String, Routine>,
}

impl Display for PileProgram {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        for (routine_name, routine) in self.routines.iter() {
            let Routine::Pile { signiture, routine } = routine else {
                continue;
            };
            formatter.write_str(format!("!!{} | ", routine_name).as_str())?;
            if signiture.inputs().len() == 0 {
                formatter.write_str("None ")?;
            } else {
                for input in signiture.inputs().iter() {
                    match input {
                        Type::String => formatter.write_str("String ")?,
                        Type::I32 => formatter.write_str("i32 ")?,
                        Type::Bool => formatter.write_str("bool ")?,
                        Type::Char => formatter.write_str("char ")?,
                        Type::Generic { name } => formatter.write_str(format!("'{}", name).as_str())?,
                    }
                }
                
            }

            formatter.write_str("-> ")?;

            if signiture.outputs().len() == 0 {
                formatter.write_str("None ")?;
            } else {
                for input in signiture.outputs().iter() {
                    match input {
                        Type::String => formatter.write_str("str ")?,
                        Type::I32 => formatter.write_str("i32 ")?,
                        Type::Bool => formatter.write_str("bool ")?,
                        Type::Char => formatter.write_str("char ")?,
                        Type::Generic { name } => formatter.write_str(format!("'{}", name).as_str())?,
                    }
                }
            }

            formatter.write_str("{\n")?;
            for token in routine.iter() {
                formatter.write_str(format!("\t{}\n", token).as_str())?;
            }
            formatter.write_str("}\n\n")?;
        }

        for token in self.tokens.iter() {
            formatter.write_str(format!("{}\n", token).as_str())?;
        }

        Ok(())
    }
}

impl PileProgram {
    fn new(tokens: &[Token], routines: HashMap<String, Routine>) -> Self {
        let mut intrinsic_routines = IntrinsicRoutine::get_routine_dictionary();
        intrinsic_routines.extend(routines);
        Self {
            tokens: tokens.to_vec().into_boxed_slice(),
            routines: intrinsic_routines,
        }
    }

    fn run_tokens(&self, tokens: &[Token], mut stack: &mut Stack) {
        let mut index = 0;
        while index < tokens.len() {
            let token = &tokens[index];
            match token {
                Token::Constant(value) => {
                    stack.push(value.clone());
                }
                Token::RoutineCall(routine_name) => {
                    let routine = self.routines.get(routine_name).expect("type checking failed if routine is missing");
                    match routine {
                        Routine::Intrinsic { signiture, routine } => routine.run(&mut stack),
                        Routine::Pile { signiture, routine } => self.run_tokens(routine, &mut stack)
                    }
                },
                Token::Block(Block::Open { close_position }) => {}
                Token::Block(Block::Close { open_position }) => {
                    if let Token::While = &tokens[open_position - 1] {
                        index = open_position - 2;
                    }
                }
                Token::If | Token::While => {
                    let Token::Block(Block::Open { close_position }) = &tokens[index + 1] else {
                        panic!("Expected open block after if or while");
                    };

                    if !stack.pop_bool().expect("Type Checking Failed") {
                        index = *close_position;
                    }
                }
            }
            index += 1;
        }
    }

    fn run(&self) -> Stack {
        let tokens = &self.tokens;
        let mut stack = Stack::new();

        self.run_tokens(tokens, &mut stack);
        
        stack
    }
}

#[derive(PartialEq, Debug, Clone)]
enum Token {
    Constant(Value),
    RoutineCall(String),
    Block(Block),
    If,
    While,
}

impl Display for Token {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Token::Constant(Value::String(str_value)) => formatter.write_str(format!(r#""{}""#, str_value).as_str()),
            Token::Constant(Value::Char(char_value)) => formatter.write_str(format!("'{}'", char_value).as_str()),
            Token::Constant(Value::Bool(true)) => formatter.write_str("True"),
            Token::Constant(Value::Bool(false)) => formatter.write_str("False"),
            Token::Constant(Value::I32(i32_value)) => formatter.write_str(format!("{}", i32_value).as_str()),
            Token::RoutineCall(routine_name) => formatter.write_str(format!("!{}", routine_name).as_str()),
            Token::Block(Block::Open { close_position: _ }) => formatter.write_str("{"),
            Token::Block(Block::Close { open_position: _ }) => formatter.write_str("}"),
            Token::If => formatter.write_str("if"),
            Token::While => formatter.write_str("while")
        }
    }
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
    InvalidType,
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
            None => Err(PopError::StackEmpty),
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
