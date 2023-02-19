use std::collections::HashMap;

use inkwell::basic_block::BasicBlock;
use inkwell::targets::TargetTriple;
use inkwell::IntPredicate;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::{IntType, VoidType, PointerType, BasicType};
use inkwell::module::{Module, Linkage};
use inkwell::AddressSpace;
use inkwell::values::{BasicValueEnum, PointerValue, IntValue, AggregateValueEnum, FunctionValue, BasicMetadataValueEnum};
use crate::routines::RoutineSigniture;
use crate::{Token, Block, Type};
use crate::{PileProgram, Value, routines::{Routine, IntrinsicRoutine}};


#[allow(unused)]
#[derive(Debug)]
pub(crate) enum CompilerError{
    EmptyTypeStack,
    MissingRoutine(String),
    MissingGlobal(String),
    PrintToFileFailure(String),
    InvalidIntByteLength(u32),
    MissingFirstParam(String),
    MissingReturnValue(String),
    MissingOpenBlock,
    MissingCloseBlock,
    UnexpectedPreOpenBlockToken,
    InvalidParamType { expected: String, found: String, fn_name: String },
    InvalidInsertValueResult { expected: String, found: String },
    InvalidLoadType { expected: String, found: String },
    InvalidExtractType { expected: String, found: String },
    InvalidReturnValue { expected: String, found: String, fn_name: String },
    InvalidTypeStackTypes { expected: Box<[LLVMType]>, found: Box<[Option<LLVMType>]> },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum LLVMString {
    Constant,
    Stack,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum LLVMType {
    String(LLVMString),
    I32,
    Char,
    Bool
}

// todo: enum
const PUSH_BOOL: &str = "push_bool";
const PUSH_BYTE: &str = "push_byte";
const PUSH_I16: &str = "push_i16";
const PUSH_I32: &str = "push_i32";
const PUSH_I64: &str = "push_i64";
const PUSH_STR_PTR: &str = "push_str_ptr";

const POP_BOOL: &str = "pop_bool";
const POP_BYTE: &str = "pop_byte";
const POP_I16: &str = "pop_i16";
const POP_I32: &str = "pop_i32";
const POP_I64: &str = "pop_i64";
const POP_STR_PTR: &str = "pop_str_ptr";

const PRINT_STR: &str = "print_str";
const PRINT_I32: &str = "print_i32";
const PRINT_BOOL: &str = "print_bool";
const PRINT_CHAR: &str = "print_char";

const STR_CONCAT: &str = "str_concat";
const STR_COMPARE: &str = "str_compare";

const PRINTF: &str = "printf";
const PRINT_D: &str = "print_d";
const PRINT_S: &str = "print_s";

const MALLOC: &str = "malloc";
const FREE: &str = "free";


impl PileProgram {
    pub(crate) fn compile(&self, file_name: &str) -> Result<(), CompilerError> {

        let slash_index = file_name.chars().rev().position(|c| c == '/' || c == '\\').map(|index| file_name.len() - 1 - index);
        let file_part = if let Some(slash_index) = slash_index {
            &file_name[(slash_index + 1)..]
        } else {
            file_name
        };
        
        let context = Context::create();
        let code_gen = CodeGen::new(file_part, &context);

        code_gen.build_push_int(2, PUSH_I16)?;
        code_gen.build_push_int(4, PUSH_I32)?;
        code_gen.build_push_int(8, PUSH_I64)?;
        code_gen.build_push_int(1, PUSH_BYTE)?;
        code_gen.build_push_bool()?;
        code_gen.build_push_str_ptr()?;

        code_gen.build_pop_int(2, POP_I16)?;
        code_gen.build_pop_int(4, POP_I32)?;
        code_gen.build_pop_int(8, POP_I64)?;
        code_gen.build_pop_int(1, POP_BYTE)?;
        code_gen.build_pop_bool()?;
        code_gen.build_pop_str_ptr()?;

        code_gen.build_print_i32()?;
        code_gen.build_print_str()?;
        code_gen.build_print_bool()?;
        code_gen.build_print_char()?;

        code_gen.build_str_concat()?;
        code_gen.build_string_compare()?;

        let functions = code_gen.declare_routines(&self.routines);
        let main_fn = code_gen.declare_routine("main");

        for (routine, fn_value) in functions.iter() {
            let Routine::Pile { signiture, routine: tokens } = routine else {
                panic!("declare_routines should only build pile routines");
            };
            code_gen.build_routine(*fn_value, tokens, signiture, &self.routines)?;
        }

        let main_signiture = RoutineSigniture::new("main", &[], &[]);

        code_gen.build_routine(main_fn, &self.tokens, &main_signiture, &self.routines)?;


        let ll_file = &file_name[..(file_name.len() - 3)];
        code_gen.module.print_to_file(format!("{}.ll", ll_file)).map_err(|err| CompilerError::PrintToFileFailure(err.to_string()))?;
        Ok(())
    }
}

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    types: LLVMValueTypes<'ctx>,
}

const STACK_NAME: &str = "pile_stack";
const STACK_SIZE: u32 = 1024u32;
const STACK_POINTER: &str = "stack_pointer";

struct LLVMValueTypes<'a> {
    bool_type: IntType<'a>,
    byte_type: IntType<'a>,
    i16_type: IntType<'a>,
    i32_type: IntType<'a>,
    i64_type: IntType<'a>,
    void_type: VoidType<'a>,
    generic_address_space: AddressSpace,
    str_type: PointerType<'a>,
}

impl<'ctx> CodeGen<'ctx> {
    fn new(module_name: &str, context: &'ctx Context) -> Self {
        let generic_address_space = AddressSpace::from(0u16);
        let char_type = context.i8_type();
        let str_type = char_type.ptr_type(generic_address_space);
        let byte_type = context.i8_type();
        let stack_type = byte_type.array_type(STACK_SIZE);
        let types = LLVMValueTypes {
            bool_type: context.bool_type(),
            byte_type,
            i16_type: context.i16_type(),
            i32_type: context.i32_type(),
            i64_type: context.i64_type(),
            void_type: context.void_type(),
            generic_address_space,
            str_type,
        };

        let builder = context.create_builder();
        let module = context.create_module(module_name);

        let stack = module.add_global(stack_type, Some(generic_address_space), STACK_NAME);
        let stack_pointer = module.add_global(types.byte_type, Some(generic_address_space), STACK_POINTER);

        let zero_byte = types.byte_type.const_zero();
        let empty_array: Vec<_> = (0..STACK_SIZE).into_iter().map(|_| zero_byte).collect();

        stack.set_linkage(Linkage::Internal);
        stack.set_initializer(&types.byte_type.const_array(&empty_array));

        stack_pointer.set_linkage(Linkage::Internal);
        stack_pointer.set_initializer(&types.byte_type.const_zero());

        let malloc_type = types.byte_type.ptr_type(types.generic_address_space).fn_type(&[types.i32_type.into()], false);
        module.add_function(MALLOC, malloc_type, Some(Linkage::External));

        let free_type = types.void_type.fn_type(&[types.byte_type.ptr_type(types.generic_address_space).into()], false);
        module.add_function(FREE, free_type, Some(Linkage::External));

        let printf_type = types.i32_type.fn_type(&[types.str_type.into()], true);
        module.add_function(PRINTF, printf_type, Some(Linkage::External));

        let new_line_value = context.const_string("\r\n".as_ref(), true);
        let new_line = module.add_global(new_line_value.get_type(), Some(types.generic_address_space), "new_line");
        new_line.set_linkage(Linkage::Internal);
        new_line.set_initializer(&new_line_value);

        let print_d_value = context.const_string("%d".as_ref(), true);
        let print_d = module.add_global(print_d_value.get_type(), Some(types.generic_address_space), PRINT_D);
        print_d.set_linkage(Linkage::Internal);
        print_d.set_initializer(&print_d_value);

        let print_s_value = context.const_string("%S".as_ref(), true);
        let print_s = module.add_global(print_s_value.get_type(), Some(types.generic_address_space), PRINT_S);
        print_s.set_linkage(Linkage::Internal);
        print_s.set_initializer(&print_s_value);

        let triple = TargetTriple::create("x86_64-pc-windows");
        module.set_triple(&triple);

        Self {
            context,
            module,
            builder,
            types,
        }
    }

    fn declare_routine<'a, 'b>(&'a self, name: &'b str) -> FunctionValue<'a> {
        let fn_type = self.types.void_type.fn_type(&[], false);
        self.module.add_function(name, fn_type, None)
    }

    fn declare_routines<'a, 'b>(&'a self, routines: &'b HashMap<String, Routine>) -> Box::<[(&'b Routine, FunctionValue<'a>)]> {
        let functions: Vec<_> = routines.iter()
            .filter(|(_, r)| matches!(r, Routine::Pile { signiture: _, routine: _ }))
            .map(|(_, r)| (r, self.declare_routine(r.signiture().name())))
            .collect();

        functions.into_boxed_slice()
    }

    fn build_routine(&self, fn_value: FunctionValue, tokens: &[Token], signiture: &RoutineSigniture, routines: &HashMap<String, Routine>) -> Result<(), CompilerError> {
        let entry_block = self.context.append_basic_block(fn_value, "entry");

        self.builder.position_at_end(entry_block);

        let mut type_stack: Vec<_> = signiture.inputs().iter().map(|t| {
            match t {
                Type::String => LLVMType::String(LLVMString::Constant),
                Type::I32 => LLVMType::I32,
                Type::Char => LLVMType::Char,
                Type::Bool => LLVMType::Bool,
                Type::Generic { name: _ } => todo!(),
            }
        }).collect();
        let mut block_stack = Vec::new();
        let mut token_index = 0;
        while let Some(token) = tokens.get(token_index) {
            match token {
                Token::Constant(value) => self.push_constant(value, &mut type_stack)?,
                Token::If | Token::While => {
                    type_stack.pop().ok_or(CompilerError::EmptyTypeStack)?;
                },
                Token::Block(Block::Open { close_position: _ }) => {
                    let (_, maybe_open, close) = block_stack.last()
                        .cloned()
                        .unwrap_or((None, None, entry_block));

                    let block_to_append = if let Some(open) = maybe_open {
                        open
                    } else {
                        block_stack.pop();
                        close
                    };

                    let previous_token = &tokens[token_index - 1];
                    let (loop_start, open, close) = match previous_token {
                        Token::While => {
                            self.handle_while_open_block(
                                block_to_append)
                                .map(|(start, open, close)| (Some(start), open, close))
                        },
                        Token::If => {
                            self.handle_if_open_block(
                                block_to_append).map(|(open, close)| (None, open, close))
                        },
                        _ => Err(CompilerError::UnexpectedPreOpenBlockToken),
                    }?;
                    block_stack.push((loop_start, Some(open), close));
                },
                Token::Block(Block::Close { open_position }) => {
                    let (maybe_loop_start, maybe_open, close) = block_stack.pop()
                        .expect("We need blocks in the stack to close a block");
                    let (maybe_loop_start, close) = if maybe_open.is_some() {
                        (maybe_loop_start, close)
                    } else {
                        // this open block has already been closed, go to next block stack entry
                        let (maybe_loop_start, _, close) = block_stack.pop()
                            .expect("We need blocks in the stack to close a block");
                        (maybe_loop_start, close)
                    };

                    block_stack.push((maybe_loop_start, None, close));

                    self.handle_close_block(*open_position, &tokens, maybe_loop_start, close)?;
                    if let Token::While = &tokens[open_position - 1] {
                        type_stack.pop();
                    }
                },
                Token::RoutineCall(routine_name) =>{
                    let Some(routine) = routines.get(routine_name) else {
                        return Err(CompilerError::MissingRoutine(routine_name.to_owned()));
                    };
                    self.call_routine(routine, &mut type_stack)?;
                },
            }
            token_index += 1;
        }

        self.builder.build_return(None);
        
        Ok(())
    }

    fn handle_while_open_block<'a>(&'a self, previous_block: BasicBlock<'a>) -> Result<(BasicBlock<'a>, BasicBlock<'a>, BasicBlock<'a>), CompilerError> {
        let loop_start = self.context.insert_basic_block_after(previous_block, "loop_start");
        self.builder.build_unconditional_branch(loop_start);
        self.builder.position_at_end(loop_start);

        let open_block = self.context.insert_basic_block_after(loop_start, "loop_open");

        let close_block = self.context.insert_basic_block_after(open_block, "loop_close");
        let pop_bool = self.get_function(POP_BOOL)?;

        let bool_value = self.call_int_return(pop_bool, &[], "while_check", POP_BOOL)?;
        self.builder.build_conditional_branch(bool_value, open_block, close_block);

        self.builder.position_at_end(open_block);

        Ok((loop_start, open_block, close_block))
    }

    fn handle_if_open_block<'a>(&'a self, previous_block: BasicBlock<'a>) -> Result<(BasicBlock<'a>, BasicBlock<'a>), CompilerError> {
        let open_block = self.context.insert_basic_block_after(previous_block, "if_open");

        let close_block = self.context.insert_basic_block_after(open_block, "if_close");
        let pop_bool = self.get_function(POP_BOOL)?;

        let bool_value = self.call_int_return(pop_bool, &[], "if_check", "POP_BOOL")?;
        self.builder.build_conditional_branch(bool_value, open_block, close_block);

        self.builder.position_at_end(open_block);

        Ok((open_block, close_block))
    }

    fn handle_close_block(&self,
                    open_position: usize,
                    tokens: &[Token],
                    loop_start: Option<BasicBlock>,
                    previous_close_block: BasicBlock) -> Result<(), CompilerError> {
        match &tokens[open_position - 1] {
            Token::While => {
                self.builder.build_unconditional_branch(
                    loop_start.expect(""));
            },
            Token::If => {
                self.builder.build_unconditional_branch(previous_close_block);
            },
            _ => return Err(CompilerError::UnexpectedPreOpenBlockToken),
        }
        

        self.builder.position_at_end(previous_close_block);

        Ok(())
    }

    fn get_function(&self, name: &str) -> Result<FunctionValue, CompilerError> {
        self.module.get_function(name).ok_or_else(|| CompilerError::MissingRoutine(name.to_owned()))
    }

    fn build_push_int(&self, bytes: u32, name: &str) -> Result<(), CompilerError> {
        let param_type = match bytes {
            1 => self.types.byte_type,
            2 => self.types.i16_type,
            4 => self.types.i32_type,
            8 => self.types.i64_type,
            _ => return Err(CompilerError::InvalidIntByteLength(bytes)),
        };
        let fn_type = self.types.void_type.fn_type(&[param_type.into()], false);

        let fn_value = self.module.add_function(name, fn_type, None);
        let block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(block);

        let param = fn_value.get_first_param();
        let Some(BasicValueEnum::IntValue(int_value)) = param else {
            return Err(CompilerError::InvalidParamType{ expected: "int".to_owned(), found: format!("{:?}", param), fn_name: name.to_owned()});
        };
        let stack_section_type = self.types.byte_type.array_type(bytes);
        let mut stack_section = stack_section_type.const_zero();

        for (i, bits) in (0..bytes).rev().map(|i| i * 8).enumerate() {
            let bits_to_shift = param_type.const_int(bits.into(), false);
            let byte = self.builder.build_right_shift(int_value, bits_to_shift, false, "right_shift");
            let trunc = self.builder.build_int_truncate(byte, self.types.byte_type, "trunc");

            let insert_index: u32 = bytes - (i as u32 + 1);
            let insert_result = self.builder.build_insert_value(stack_section, trunc, insert_index, "inserted_byte");
            let Some(AggregateValueEnum::ArrayValue(insert_result)) = insert_result else {
                return Err(CompilerError::InvalidInsertValueResult { expected: "array".to_owned(), found: format!("{:?}", insert_result)});
            };
            stack_section = insert_result;
        }

        let stack_pointer = self.module.get_global(STACK_POINTER).map(|g| g.as_pointer_value()).ok_or_else(|| CompilerError::MissingGlobal(STACK_POINTER.to_owned()))?;
        let stack_pointer_value = self.builder.build_load(self.types.byte_type, stack_pointer, "stack_pointer");
        let BasicValueEnum::IntValue(stack_pointer_value) = stack_pointer_value else {
            return Err(CompilerError::InvalidLoadType { expected: "int".to_owned(), found: format!("{:?}", stack_pointer_value) });
        };

        let mut stack = self.module.get_global(STACK_NAME).map(|g| g.as_pointer_value()).ok_or_else(|| CompilerError::MissingGlobal(STACK_NAME.to_owned()))?;

        unsafe {
            stack = self.builder.build_gep(stack.get_type(), stack, &[stack_pointer_value], "");
        }

        self.builder.build_store(stack, stack_section);

        let new_stack_pointer = self.builder.build_int_add(stack_pointer_value, self.types.byte_type.const_int(bytes.into(), false), "new_stack_pointer");

        self.builder.build_store(stack_pointer, new_stack_pointer);
        self.builder.build_return(None);

        Ok(())
    }

    fn build_push_str_ptr(&self) -> Result<(), CompilerError> {
        let fn_type = self.types.void_type.fn_type(&[self.types.str_type.into()], false);
        let fn_value = self.module.add_function(PUSH_STR_PTR, fn_type, None);
        let entry_block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry_block);

        let str_ptr = fn_value.get_first_param();
        let Some(BasicValueEnum::PointerValue(str_ptr)) = str_ptr else {
            return Err(CompilerError::InvalidParamType { expected: "ptr".to_owned(), found: format!("{:?}", str_ptr), fn_name: PUSH_STR_PTR.to_owned() });
        };

        let str_ptr_int = self.builder.build_ptr_to_int(str_ptr, self.types.i64_type, "str_ptr_int");

        let push_i64 = self.get_function(PUSH_I64)?;
        self.builder.build_call(push_i64, &[str_ptr_int.into()], "");
        self.builder.build_return(None);

        Ok(())
    }

    fn build_push_bool(&self) -> Result<(), CompilerError> {
        let fn_type = self.types.void_type.fn_type(&[self.types.bool_type.into()], false);
        let fn_value = self.module.add_function(PUSH_BOOL, fn_type, None);
        let entry_block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry_block);

        let bool_value = fn_value.get_first_param();
        let Some(BasicValueEnum::IntValue(bool_value)) = bool_value else {
            return Err(CompilerError::InvalidParamType { expected: "int".to_owned(), found: format!("{:?}", bool_value), fn_name: PUSH_BOOL.to_owned() });
        };

        let push_byte = self.module.get_function(PUSH_BYTE).ok_or_else(|| CompilerError::MissingRoutine(PUSH_BYTE.to_owned()))?;

        let byte = self.builder.build_int_z_extend(bool_value, self.types.byte_type, "bool_extended");

        self.builder.build_call(push_byte, &[byte.into()], "");

        self.builder.build_return(None);

        Ok(())
    }

    fn build_pop_str_ptr(&self) -> Result<(), CompilerError> {
        let fn_type = self.types.str_type.fn_type(&[], false);
        let fn_value = self.module.add_function(POP_STR_PTR, fn_type, None);
        let entry_block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry_block);

        let pop_i64 = self.get_function(POP_I64)?;
        let str_i64 = self.call_int_return(pop_i64, &[], "str_i64", POP_I64)?;

        let str_ptr = self.builder.build_int_to_ptr(str_i64, self.types.str_type, "str_ptr");

        self.builder.build_return(Some(&str_ptr));

        Ok(())
    }

    fn build_pop_bool(&self) -> Result<(), CompilerError> {
        let fn_type = self.types.bool_type.fn_type(&[], false);
        let fn_value = self.module.add_function(POP_BOOL, fn_type, None);
        let entry_block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry_block);

        let pop_byte = self.module.get_function(POP_BYTE).ok_or_else(|| CompilerError::MissingRoutine(POP_BYTE.to_owned()))?;

        let byte = self.builder.build_call(pop_byte, &[], "byte");
        let Some(BasicValueEnum::IntValue(byte)) = byte.try_as_basic_value().left() else {
            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", byte), fn_name: POP_BYTE.to_owned() });
        };

        let bool_value = self.builder.build_int_cast(byte, self.types.bool_type, "bool");

        self.builder.build_return(Some(&bool_value));

        Ok(())
    }
    
    fn build_pop_int(&self, bytes: u32, name: &str) -> Result<(), CompilerError> {
        let int_type = match bytes {
            1 => self.types.byte_type,
            2 => self.types.i16_type,
            4 => self.types.i32_type,
            8 => self.types.i64_type,
            _ => return Err(CompilerError::InvalidIntByteLength(bytes)),
        };

        let fn_type = int_type.fn_type(&[], false);
        let fn_value = self.module.add_function(name, fn_type, None);
        let entry_block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry_block);

        let mut stack_ptr = self.module.get_global(STACK_NAME)
            .map(|g| g.as_pointer_value()).ok_or_else(|| CompilerError::MissingGlobal(STACK_NAME.to_owned()))?;

        let stack_pointer = self.module.get_global(STACK_POINTER)
            .map(|g| g.as_pointer_value()).ok_or_else(|| CompilerError::MissingGlobal(STACK_POINTER.to_owned()))?;

        let stack_pointer_value = self.builder.build_load(self.types.byte_type, stack_pointer, "stack_pointer");
        let BasicValueEnum::IntValue(stack_pointer_value) = stack_pointer_value else {
            return Err(CompilerError::InvalidLoadType { expected: "int".to_owned(), found: format!("{:?}", stack_pointer_value) });
        };

        let new_stack_pointer = self.builder.build_int_sub(stack_pointer_value, self.types.byte_type.const_int(bytes.into(), false), "new_stack_pointer");

        self.builder.build_store(stack_pointer, new_stack_pointer);

        unsafe {
            stack_ptr = self.builder.build_gep(stack_ptr.get_type(), stack_ptr, &[new_stack_pointer], "stack");
        }

        let stack_section_type = self.types.byte_type.array_type(bytes);

        let stack_section = self.builder.build_load(stack_section_type, stack_ptr, "stack");
        let BasicValueEnum::ArrayValue(stack_section) = stack_section else {
            return Err(CompilerError::InvalidLoadType { expected: "array".to_owned(), found: format!("{:?}", stack_section) });
        };

        let mut result_value = int_type.const_zero();

        for index in 0..bytes {
            let bits_to_shift = int_type.const_int((index * 8).into(), false);
            let byte = self.builder.build_extract_value(stack_section, index, "popped_byte");
            let Some(BasicValueEnum::IntValue(byte)) = byte else {
                return Err(CompilerError::InvalidExtractType { expected: "int".to_owned(), found: format!("{:?}", byte) });
            };

            let extended_byte = self.builder.build_int_z_extend(byte, int_type, "extended_byte");
            let shifted_byte = self.builder.build_left_shift(extended_byte, bits_to_shift, "shifted_byte");
                
            result_value = self.builder.build_int_add(result_value, shifted_byte, "result_value");
        }

        self.builder.build_return(Some(&result_value));

        Ok(())
    }

    fn build_print_i32(&self) -> Result<(), CompilerError> {
        let fn_type = self.types.i32_type.fn_type(&[self.types.i32_type.into()], false);
        let fn_value = self.module.add_function(PRINT_I32, fn_type, None);
        let entry = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry);

        let printf = self.module.get_function(PRINTF).ok_or_else(|| CompilerError::MissingRoutine(PRINTF.to_owned()))?;
        
        let print_d_ptr = self.module.get_global(PRINT_D).map(|g| g.as_pointer_value()).ok_or_else(|| CompilerError::MissingGlobal(PRINT_D.to_owned()))?;

        let int_value = fn_value.get_first_param().ok_or_else(|| CompilerError::MissingFirstParam(PRINT_I32.to_owned()))?;
        
        let return_value = self.builder.build_call(printf, &[print_d_ptr.into(), int_value.into()], "ret_value");
        let Some(return_value) = return_value.try_as_basic_value().left() else {
            return Err(CompilerError::MissingReturnValue(PRINTF.to_owned()));
        };

        self.builder.build_return(Some(&return_value));
        Ok(())
    }

    fn build_print_str(&self) -> Result<(), CompilerError> {
        let fn_type = self.types.i32_type.fn_type(&[self.types.str_type.into()], false);
        let fn_value = self.module.add_function(PRINT_STR, fn_type, None);
        let entry = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry);

        let printf = self.module.get_function(PRINTF).ok_or_else(|| CompilerError::MissingRoutine(PRINTF.to_owned()))?;

        let str_ptr = fn_value.get_first_param().ok_or_else(|| CompilerError::MissingFirstParam(PRINT_STR.to_owned()))?;

        let print_s_ptr = self.module.get_global(PRINT_S).map(|g| g.as_pointer_value()).ok_or_else(|| CompilerError::MissingGlobal(PRINT_S.to_owned()))?;
        
        let return_value = self.builder.build_call(printf, &[print_s_ptr.into(), str_ptr.into()], "ret_value");
        let Some(return_value) = return_value.try_as_basic_value().left() else {
            return Err(CompilerError::MissingReturnValue(PRINTF.to_owned()));
        };

        self.builder.build_return(Some(&return_value));
        Ok(())
    }

    fn build_print_bool(&self) -> Result<(), CompilerError> {
        let fn_type = self.types.i32_type.fn_type(&[self.types.byte_type.into()], false);
        let fn_value = self.module.add_function(PRINT_BOOL, fn_type, None);
        let entry = self.context.append_basic_block(fn_value, "entry");
        let is_true_block = self.context.insert_basic_block_after(entry, "is_true");
        let is_false_block = self.context.insert_basic_block_after(entry, "is_false");

        let true_const_string = &self.context.const_string("true".as_ref(), true);
        let true_global_value = self.module.add_global(true_const_string.get_type(), Some(AddressSpace::from(0u16)), "true_str");
        true_global_value.set_linkage(Linkage::Internal);
        true_global_value.set_initializer(true_const_string);

        let false_const_string = &self.context.const_string("false".as_ref(), true);
        let false_global_value = self.module.add_global(false_const_string.get_type(), Some(AddressSpace::from(0u16)), "false_str");
        false_global_value.set_linkage(Linkage::Internal);
        false_global_value.set_initializer(false_const_string);

        let printf = self.module.get_function(PRINTF).ok_or_else(|| CompilerError::MissingRoutine(PRINTF.to_owned()))?;

        self.builder.position_at_end(entry);
        let byte_value = fn_value.get_first_param();
        let Some(BasicValueEnum::IntValue(byte_value)) = byte_value else {
            return Err(CompilerError::InvalidParamType { expected: "int".to_owned(), found: format!("{:?}", byte_value), fn_name: PRINT_BOOL.to_owned() });
        };

        let is_true = self.builder.build_int_cast(byte_value, self.types.bool_type, "is_true");

        self.builder.build_conditional_branch(is_true, is_true_block, is_false_block);

        self.builder.position_at_end(is_false_block);

        let return_value = self.builder.build_call(printf, &[false_global_value.as_pointer_value().into()], "print_result");
        let Some(return_value) = return_value.try_as_basic_value().left() else {
            return Err(CompilerError::MissingReturnValue(PRINTF.to_owned()));
        };

        self.builder.build_return(Some(&return_value));

        self.builder.position_at_end(is_true_block);

        let return_value = self.builder.build_call(printf, &[true_global_value.as_pointer_value().into()], "print_result");
        let Some(return_value) = return_value.try_as_basic_value().left() else {
            return Err(CompilerError::MissingReturnValue(PRINTF.to_owned()));
        };

        self.builder.build_return(Some(&return_value));

        Ok(())
    }

    fn build_print_char(&self) -> Result<(), CompilerError> {
        let fn_type = self.types.i32_type.fn_type(&[self.types.i16_type.array_type(2).into()], false);
        let fn_value = self.module.add_function(PRINT_CHAR, fn_type, None);
        let entry_block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry_block);

        let bytes = fn_value.get_first_param().ok_or_else(|| CompilerError::MissingFirstParam(PRINT_CHAR.to_owned()))?;

        let format_string = &self.context.const_string("%C".as_ref(), true);
        let fmt_global_value = self.module.add_global(format_string.get_type(), Some(AddressSpace::from(0u16)), "print_c");
        fmt_global_value.set_linkage(Linkage::Internal);
        fmt_global_value.set_initializer(format_string);

        let printf = self.module.get_function(PRINTF).ok_or_else(|| CompilerError::MissingRoutine(PRINTF.to_owned()))?;
        let return_value = self.builder.build_call(printf, &[fmt_global_value.as_pointer_value().into(), bytes.into()], "return_value");
        let return_value = return_value.try_as_basic_value().left().ok_or_else(|| CompilerError::MissingReturnValue(PRINTF.to_owned()))?;

        self.builder.build_return(Some(&return_value));

        Ok(())
    }

    fn call_int_return<'a>(&'a self, fn_value: FunctionValue<'a>, args: &[BasicMetadataValueEnum<'a>], name: &str, fn_name: &str) -> Result<IntValue<'a>, CompilerError> {
        let return_value = self.builder.build_call(fn_value, args, name);
        if let Some(BasicValueEnum::IntValue(return_value)) = return_value.try_as_basic_value().left() {
            Ok(return_value)
        } else {
            Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", return_value), fn_name: fn_name.to_owned() })
        }
    }
    
    fn call_ptr_return<'a>(&'a self, fn_value: FunctionValue<'a>, args: &[BasicMetadataValueEnum<'a>], name: &str, fn_name: &str) -> Result<PointerValue<'a>, CompilerError> {
        let return_value = self.builder.build_call(fn_value, args, name);
        if let Some(BasicValueEnum::PointerValue(return_value)) = return_value.try_as_basic_value().left() {
            Ok(return_value)
        } else {
            Err(CompilerError::InvalidReturnValue { expected: "ptr".to_owned(), found: format!("{:?}", return_value), fn_name: fn_name.to_owned() })
        }
    }

    fn load_int<'a, T : BasicType<'a>>(&'a self, pointee_type: T, ptr: PointerValue<'a>, name: &str) -> Result<IntValue<'a>, CompilerError> {
        let load_result = self.builder.build_load(pointee_type, ptr, name);
        if let BasicValueEnum::IntValue(int_value) = load_result {
            Ok(int_value)
        } else {
            Err(CompilerError::InvalidLoadType { expected: "int".to_owned(), found: format!("{:?}", load_result) })
        }
    }

    fn build_str_concat(&self) -> Result<(), CompilerError> {
        let fn_type = self.types.void_type.fn_type(&[], false);
        let fn_value = self.module.add_function(STR_CONCAT, fn_type, None);
        let entry_block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry_block);

        let push_i64 = self.get_function(PUSH_I64)?;
        let push_i32 = self.get_function(PUSH_I32)?;
        let pop_i32 = self.get_function(POP_I32)?;
        let pop_i64 = self.get_function(POP_I64)?;
        let malloc = self.get_function(MALLOC)?;

        let b_len = self.call_int_return(pop_i32, &[], "b_len", POP_I32)?;
        let b_ptr_int = self.call_int_return(pop_i64, &[], "b_ptr_int", POP_I64)?;
        let b_ptr = self.builder.build_int_to_ptr(b_ptr_int, self.types.str_type, "b_ptr");

        let a_len = self.call_int_return(pop_i32, &[], "a_len", POP_I32)?;
        let a_ptr_int = self.call_int_return(pop_i64, &[], "a_ptr_int", POP_I64)?;
        let a_ptr = self.builder.build_int_to_ptr(a_ptr_int, self.types.str_type, "a_ptr");

        let length_i16 = self.builder.build_int_add(a_len, b_len, "length_chars");
        // minus 1 char because both strings will be null terminated, but we only want one null
        let length_16 = self.builder.build_int_sub(length_i16, self.types.i32_type.const_int(1, false), "length_chars");

        let length_bytes = self.builder.build_int_mul(length_i16, self.types.i32_type.const_int(2, false), "length_bytes");

        let new_ptr = self.call_ptr_return(malloc, &[length_bytes.into()], "new_ptr", MALLOC)?;

        let new_ptr_int = self.builder.build_ptr_to_int(new_ptr, self.types.i64_type, "new_ptr_int");

        self.builder.build_call(push_i64, &[new_ptr_int.into()], "");
        self.builder.build_call(push_i32, &[length_16.into()], "");

        let counter_ptr = self.builder.build_alloca(self.types.i32_type, "counter");
        self.builder.build_store(counter_ptr, self.types.i32_type.const_zero());

        let a_i16_to_load = self.builder.build_int_sub(a_len, self.types.i32_type.const_int(1, false), "a_bytes_to_load");

        let loop_a_block = self.context.insert_basic_block_after(entry_block, "loop_a");
        let loop_a_end_block = self.context.insert_basic_block_after(loop_a_block, "loop_a_end");
        self.builder.build_unconditional_branch(loop_a_block);
        self.builder.position_at_end(loop_a_block);

        let current_count = self.load_int(self.types.i32_type, counter_ptr, "current_count")?;

        let next_count = self.builder.build_int_add(current_count, self.types.i32_type.const_int(1, false), "next_count");
        self.builder.build_store(counter_ptr, next_count);

        let a_ptr_to_load;
        unsafe {
            a_ptr_to_load = self.builder.build_gep(self.types.i16_type, a_ptr, &[current_count], "a_ptr_to_load");
        }

        let a_i16 = self.load_int(self.types.i16_type, a_ptr_to_load, "a_byte")?;

        let new_ptr_to_store;
        unsafe {
            new_ptr_to_store = self.builder.build_gep(self.types.i16_type, new_ptr, &[current_count], "new_ptr");
        }
        self.builder.build_store(new_ptr_to_store, a_i16);

        let exit_count = self.builder.build_int_sub(a_i16_to_load, self.types.i32_type.const_int(1, false), "exit_count");
        let is_count = self.builder.build_int_compare(IntPredicate::EQ, current_count, exit_count, "is_count");
        self.builder.build_conditional_branch(is_count, loop_a_end_block, loop_a_block);

        self.builder.build_unconditional_branch(loop_a_end_block);
        self.builder.position_at_end(loop_a_end_block);

        let loop_b_block = self.context.insert_basic_block_after(loop_a_end_block, "loop_b");
        let loop_b_end_block = self.context.insert_basic_block_after(loop_b_block, "loop_b_end");
        self.builder.build_unconditional_branch(loop_b_block);
        self.builder.position_at_end(loop_b_block);

        let current_count = self.load_int(self.types.i32_type, counter_ptr, "current_count")?;

        let next_count = self.builder.build_int_add(current_count, self.types.i32_type.const_int(1, false), "next_count");
        self.builder.build_store(counter_ptr, next_count);

        let b_ptr_to_load;
        let b_offset = self.builder.build_int_sub(current_count, a_i16_to_load, "b_offset");

        unsafe {
            b_ptr_to_load = self.builder.build_gep(self.types.i16_type, b_ptr, &[b_offset], "b_ptr_to_load");
        }

        let b_byte = self.load_int(self.types.i16_type, b_ptr_to_load, "b_byte")?;

        let new_ptr_to_store;
        unsafe {
            new_ptr_to_store = self.builder.build_gep(self.types.i16_type, new_ptr, &[current_count], "new_ptr_to_store");
        }

        self.builder.build_store(new_ptr_to_store, b_byte);

        let exit_count = self.builder.build_int_sub(length_i16, self.types.i32_type.const_int(2, false), "exit_count");
        let is_count = self.builder.build_int_compare(IntPredicate::EQ, current_count, exit_count, "is_count");

        self.builder.build_conditional_branch(is_count, loop_b_end_block, loop_b_block);

        self.builder.position_at_end(loop_b_end_block);

        self.builder.build_return(None);

        Ok(())
    }

    fn build_string_compare(&self) -> Result<(), CompilerError> {
        let fn_type = self.types.void_type.fn_type(&[], false);
        let fn_value = self.module.add_function(STR_COMPARE, fn_type, None);
        let entry_block = self.context.append_basic_block(fn_value, "entry");

        self.builder.position_at_end(entry_block);

        let pop_i32 = self.get_function(POP_I32)?;
        let pop_i64 = self.get_function(POP_I64)?;
        let push_bool = self.get_function(PUSH_BOOL)?;

        let a_len = self.call_int_return(pop_i32, &[], "a_len", POP_I32)?;
        let a_ptr_int = self.call_int_return(pop_i64, &[], "a_ptr_int", POP_I64)?;
 
        let b_len = self.call_int_return(pop_i32, &[], "b_len", POP_I32)?;
        let b_ptr_int = self.call_int_return(pop_i64, &[], "b_ptr_int", POP_I64)?;       

        let lengths_equal = self.builder.build_int_compare(IntPredicate::EQ, a_len, b_len, "lengths_equal");

        let lengths_not_equal_block = self.context.insert_basic_block_after(entry_block, "lengths_not_equal");
        let lengths_equal_block = self.context.insert_basic_block_after(lengths_not_equal_block, "lengths_equal");

        self.builder.build_conditional_branch(lengths_equal, lengths_equal_block, lengths_not_equal_block);

        self.builder.position_at_end(lengths_not_equal_block);
        self.builder.build_call(push_bool, &[self.types.bool_type.const_int(0, false).into()], "");
        self.builder.build_return(None);

        self.builder.position_at_end(lengths_equal_block);

        let a_ptr = self.builder.build_int_to_ptr(a_ptr_int, self.types.str_type, "a_ptr");
        let b_ptr = self.builder.build_int_to_ptr(b_ptr_int, self.types.str_type, "b_ptr");
        let counter = self.builder.build_alloca(self.types.i32_type, "counter_ptr");
        self.builder.build_store(counter, self.types.i32_type.const_zero());

        let loop_block = self.context.insert_basic_block_after(lengths_equal_block, "loop");
        
        self.builder.build_unconditional_branch(loop_block);

        self.builder.position_at_end(loop_block);

        let current_counter = self.load_int(self.types.i32_type, counter, "current_counter")?;
        let next_counter = self.builder.build_int_add(current_counter, self.types.i32_type.const_int(1, false), "next_counter");
        self.builder.build_store(counter, next_counter);

        let a_ptr_to_load;
        let b_ptr_to_load;
        unsafe {
            a_ptr_to_load = self.builder.build_gep(self.types.i16_type, a_ptr, &[current_counter], "a_ptr_to_load");
            b_ptr_to_load = self.builder.build_gep(self.types.i16_type, b_ptr, &[current_counter], "b_ptr_to_load");
        }

        let a_i16 = self.load_int(self.types.i16_type, a_ptr_to_load, "a_i16")?;
        let b_i16 = self.load_int(self.types.i16_type, b_ptr_to_load, "b_i16")?;

        let i16_equal = self.builder.build_int_compare(IntPredicate::EQ, a_i16, b_i16, "i16_equal");

        let not_equal_block = self.context.insert_basic_block_after(loop_block, "not_equal");
        let equal_block = self.context.insert_basic_block_after(not_equal_block, "equal");

        self.builder.build_conditional_branch(i16_equal, equal_block, not_equal_block);

        self.builder.position_at_end(not_equal_block);
        self.builder.build_call(push_bool, &[self.types.bool_type.const_int(0, false).into()], "");

        self.builder.build_return(None);

        self.builder.position_at_end(equal_block);

        let exit_count = self.builder.build_int_sub(a_len, self.types.i32_type.const_int(1, false), "exit_count");
        let at_end = self.builder.build_int_compare(IntPredicate::EQ, exit_count, current_counter, "at_end");

        let loop_end = self.context.insert_basic_block_after(equal_block, "loop_end");

        self.builder.build_conditional_branch(at_end, loop_end, loop_block);
        self.builder.position_at_end(loop_end);

        self.builder.build_call(push_bool, &[self.types.bool_type.const_int(1, false).into()], "");
        self.builder.build_return(None);

        Ok(())
    }

    fn push_constant(&self, value: &Value, type_stack: &mut Vec<LLVMType>) -> Result<(), CompilerError> {
        match value {
            Value::String(str_value) => {
                type_stack.push(LLVMType::String(LLVMString::Constant));

                let utf16_encoded = str_value.encode_utf16();

                let mut int_values: Vec<_> = utf16_encoded.map(|val| {
                    self.types.i16_type.const_int(val.into(), false)
                }).collect();

                let const_zero = self.types.i16_type.const_zero();
                int_values.extend([const_zero].into_iter());

                let len_value = self.types.i32_type.const_int(int_values.len() as u64, false);

                let const_array = &self.types.i16_type.const_array(int_values.into_boxed_slice().as_ref());

                let global_value = self.module.add_global(const_array.get_type(), Some(AddressSpace::from(0u16)), "string");
                global_value.set_linkage(Linkage::Internal);
                global_value.set_initializer(const_array);

                let str_ptr = self.builder.build_pointer_cast(
                    global_value.as_pointer_value(),
                    self.context.i8_type().ptr_type(AddressSpace::from(0u16)),
                    "str_ptr",
                );

                let ptr_int = self.builder.build_ptr_to_int(str_ptr, self.types.i64_type, "ptr_as_int");

                let push_i64 = self.module.get_function(PUSH_I64).ok_or_else(|| CompilerError::MissingRoutine(PUSH_I64.to_owned()))?;

                let push_i32 = self.module.get_function(PUSH_I32).ok_or_else(|| CompilerError::MissingRoutine(PUSH_I32.to_owned()))?;
                
                self.builder.build_call(push_i64, &[ptr_int.into()], "");
                self.builder.build_call(push_i32, &[len_value.into()], "");

                Ok(())
            },
            Value::I32(i32_value) => {
                type_stack.push(LLVMType::I32);

                let push_i32 = self.module.get_function(PUSH_I32).ok_or_else(|| CompilerError::MissingRoutine(PUSH_I32.to_owned()))?;

                let llvm_value = self.types.i32_type.const_int(*i32_value as u64, false);
                self.builder.build_call(push_i32, &[llvm_value.into()], "");

                Ok(())
            },
            Value::Char(char_value) => {
                type_stack.push(LLVMType::Char);

                let mut buffer = [0; 2];
                char_value.encode_utf16(&mut buffer);

                let first_16 = self.types.i16_type.const_int(buffer[0].into(), false);
                let last_16 = self.types.i16_type.const_int(buffer[1].into(), false);

                let push_i16 = self.module.get_function(PUSH_I16).ok_or_else(|| CompilerError::MissingRoutine(PUSH_I16.to_owned()))?;

                self.builder.build_call(push_i16, &[first_16.into()], "");
                self.builder.build_call(push_i16, &[last_16.into()], "");
                
                Ok(())
            },
            Value::Bool(bool_value) => {
                type_stack.push(LLVMType::Bool);

                let bool_value: u64 = (*bool_value).into();

                let llvm_bool = self.types.bool_type.const_int(bool_value, false);
                
                let push_bool = self.module.get_function(PUSH_BOOL).ok_or_else(|| CompilerError::MissingRoutine(PUSH_BOOL.to_owned()))?;

                self.builder.build_call(push_bool, &[llvm_bool.into()], "");

                Ok(())
            },
        }
    }
    fn call_routine(&self, routine: &Routine, type_stack: &mut Vec<LLVMType>) -> Result<(), CompilerError> {
        match routine {
            Routine::Intrinsic { signiture: _, routine } => self.call_intrinsic(routine, type_stack),
            Routine::Pile { signiture, routine: _ } => {
                for _ in signiture.inputs() {
                    type_stack.pop();
                }
                let routine = self.get_function(signiture.name())?;
                self.builder.build_call(routine, &[], "");

                for output in signiture.outputs() {
                    type_stack.push(match output {
                        Type::Generic { name: _ } => panic!("Generic routine should have been specialized"),
                        Type::I32 => LLVMType::I32,
                        Type::Char => LLVMType::Char,
                        Type::Bool => LLVMType::Bool,
                        Type::String => LLVMType::String(LLVMString::Stack),
                    });
                }

                Ok(())
            },
        }
    }

    fn call_intrinsic(&self, routine: &IntrinsicRoutine, type_stack: &mut Vec<LLVMType>) -> Result<(), CompilerError> {

        let pop_bool = self.get_function(POP_BOOL)?;
        let pop_byte = self.get_function(POP_BYTE)?;
        let pop_i16 = self.get_function(POP_I16)?;
        let pop_i32 = self.get_function(POP_I32)?;
        let pop_str_ptr = self.get_function(POP_STR_PTR)?;

        let push_bool = self.get_function(PUSH_BOOL)?;
        let push_i16 = self.get_function(PUSH_I16)?;
        let push_i32 = self.get_function(PUSH_I32)?;
        let push_str_ptr = self.get_function(PUSH_STR_PTR)?;

        let print_str = self.get_function(PRINT_STR)?;
        let print_i32 = self.get_function(PRINT_I32)?;
        let print_bool = self.get_function(PRINT_BOOL)?;
        let print_char = self.get_function(PRINT_CHAR)?;

        let str_concat = self.get_function(STR_CONCAT)?;


        let pop_value = |llvm_type: LLVMType| -> Result<Box<[BasicMetadataValueEnum]>, CompilerError> {
            match llvm_type {
                LLVMType::String(_) => {
                    let str_len = self.call_int_return(pop_i32, &[], "str_len", POP_I32)?;
                    let str_ptr = self.call_ptr_return(pop_str_ptr, &[], "str_ptr", POP_STR_PTR)?;

                    Ok(Box::new([str_len.into(), str_ptr.into()]))
                },
                LLVMType::I32 => {
                    let i32_value = self.call_int_return(pop_i32, &[], "i32_value", POP_I32)?;

                    Ok(Box::new([i32_value.into()]))
                },
                LLVMType::Char => {
                    let first_i16 = self.call_int_return(pop_i16, &[], "first_i16", POP_I16)?;
                    let second_i16 = self.call_int_return(pop_i16, &[], "second_i16", POP_I16)?;

                    Ok(Box::new([first_i16.into(), second_i16.into()]))
                },
                LLVMType::Bool => {
                    let bool_value = self.call_int_return(pop_bool, &[], "bool", POP_BOOL)?;

                    Ok(Box::new([bool_value.into()]))
                }
            }
        };

        let push_value = |value: Box<[BasicMetadataValueEnum]>, llvm_type: LLVMType| -> Result<(), CompilerError> {
            let mut iter = value.iter();
            match llvm_type {
                LLVMType::String(_) => {
                    let first = iter.next().ok_or(CompilerError::InvalidLoadType { expected: "int".to_owned(), found: "None".to_owned() })?;
                    let second = iter.next().ok_or(CompilerError::InvalidLoadType { expected: "ptr".to_owned(), found: "None".to_owned() })?;

                    let str_len = first.into_int_value();
                    let str_ptr = second.into_pointer_value();

                    self.builder.build_call(push_str_ptr, &[str_ptr.into()], "");
                    self.builder.build_call(push_i32, &[str_len.into()], "");

                    Ok(())
                },
                LLVMType::I32 => {
                    let first = iter.next().ok_or(CompilerError::InvalidLoadType { expected: "int".to_owned(), found: "None".to_owned() })?;
                    let i32_value = first.into_int_value();

                    self.builder.build_call(push_i32, &[i32_value.into()], "");
                            
                    Ok(())
                },
                LLVMType::Bool => {
                    let first = iter.next().ok_or(CompilerError::InvalidLoadType { expected: "int".to_owned(), found: "None".to_owned() })?;
                    let bool_value = first.into_int_value();

                    self.builder.build_call(push_bool, &[bool_value.into()], "");
                            
                    Ok(())

                },
                LLVMType::Char => {
                    let first = iter.next().ok_or(CompilerError::InvalidLoadType { expected: "int".to_owned(), found: "None".to_owned() })?;
                    let second = iter.next().ok_or(CompilerError::InvalidLoadType { expected: "ptr".to_owned(), found: "None".to_owned() })?;

                    let first_i16 = first.into_int_value();
                    let second_i16 = second.into_int_value();

                    self.builder.build_call(push_i16, &[second_i16.into()], "");
                    self.builder.build_call(push_i16, &[first_i16.into()], "");

                    Ok(())
                },
            }
        };

        match routine {
            IntrinsicRoutine::Print => {
                let top_type = type_stack.pop().expect("Type stack should not be empty");

                match top_type {
                    LLVMType::String(string_type) => {
                        let _ = self.builder.build_call(pop_i32, &[], "discard");
                        let str_ptr = self.call_ptr_return(pop_str_ptr, &[], "str_ptr", POP_STR_PTR)?;

                        self.builder.build_call(print_str, &[str_ptr.into()], "print_result");

                        if let LLVMString::Stack = string_type {
                            // todo: figure out if we can free memory (might have been cloned);
                            // let free_memory = self.get_function(FREE)?;
                            // self.builder.build_call(free_memory, &[str_ptr.into()], "");
                        }
                    },
                    LLVMType::I32 => {
                        let int_value = self.builder.build_call(pop_i32, &[], "int_value").try_as_basic_value().left();
                        let Some(BasicValueEnum::IntValue(int_value)) = int_value else {
                            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", int_value), fn_name: POP_I32.to_owned() });
                        };

                        self.builder.build_call(print_i32, &[int_value.into()], "print_result");
                    }
                    LLVMType::Bool => {
                        let byte_value = self.builder.build_call(pop_byte, &[], "byte_value").try_as_basic_value().left();
                        let Some(BasicValueEnum::IntValue(byte_value)) = byte_value else {
                            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", byte_value), fn_name: POP_BYTE.to_owned() });
                        };

                        self.builder.build_call(print_bool, &[byte_value.into()], "print_result");
                    },
                    LLVMType::Char => {
                        let last_16 = self.builder.build_call(pop_i16, &[], "last_16").try_as_basic_value().left();
                        let Some(BasicValueEnum::IntValue(last_16)) = last_16 else {
                            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", last_16), fn_name: POP_I16.to_owned() });
                        };
                        let first_16 = self.builder.build_call(pop_i16, &[], "first_16").try_as_basic_value().left();
                        let Some(BasicValueEnum::IntValue(first_16)) = first_16 else {
                            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", first_16), fn_name: POP_I16.to_owned() });
                        };

                        let array_value = self.types.i16_type.array_type(2).const_zero();
                        let array_value = self.builder.build_insert_value(array_value, first_16, 0, "char_bytes").ok_or_else(
                            || CompilerError::InvalidInsertValueResult { expected: "array".to_owned(), found: "None".to_owned() })?;
                        let array_value = self.builder.build_insert_value(array_value, last_16, 1, "char_bytes");
                        let Some(AggregateValueEnum::ArrayValue(array_value)) = array_value else {
                            return Err(CompilerError::InvalidInsertValueResult { expected: "array".to_owned(), found: format!("{:?}", array_value) });
                        };

                        self.builder.build_call(print_char, &[array_value.into()], "print_result");
                    }
                }

                Ok(())
            },
            IntrinsicRoutine::PrintLine => {
                let Some(puts) = self.module.get_function(PRINTF) else {
                    return Err(CompilerError::MissingRoutine(PRINTF.to_owned()));
                };
                let new_line = self.module.get_global("new_line").unwrap();
                self.builder.build_call(puts, &[new_line.as_pointer_value().into()], "print_result");
                Ok(())
            },
            IntrinsicRoutine::AddI32 => {
                let top_type = type_stack.pop();
                let second_type = type_stack.pop();
                if !matches!((top_type, second_type), (Some(LLVMType::I32), Some(LLVMType::I32))) {
                    return Err(CompilerError::InvalidTypeStackTypes { expected: Box::new([LLVMType::I32, LLVMType::I32]), found: Box::new([top_type, second_type]) });
                }

                let a = self.builder.build_call(pop_i32, &[], "a").try_as_basic_value().left();
                let Some(BasicValueEnum::IntValue(a)) = a else {
                    return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", a), fn_name: POP_I32.to_owned() });
                };

                let b = self.builder.build_call(pop_i32, &[], "b").try_as_basic_value().left();
                let Some(BasicValueEnum::IntValue(b)) = b else {
                    return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", b), fn_name: POP_I32.to_owned() });
                };

                let sum = self.builder.build_int_add(a, b, "sum");
                self.builder.build_call(push_i32, &[sum.into()], "");

                type_stack.push(LLVMType::I32);
                Ok(())
            },
            IntrinsicRoutine::MinusI32 => {
                let top_type = type_stack.pop();
                let second_type = type_stack.pop();
                if !matches!((top_type, second_type), (Some(LLVMType::I32), Some(LLVMType::I32))) {
                    return Err(CompilerError::InvalidTypeStackTypes { expected: Box::new([LLVMType::I32, LLVMType::I32]), found: Box::new([top_type, second_type]) });
                }

                let a = self.builder.build_call(pop_i32, &[], "a").try_as_basic_value().left();
                let Some(BasicValueEnum::IntValue(a)) = a else {
                    return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", a), fn_name: POP_I32.to_owned() });
                };

                let b = self.builder.build_call(pop_i32, &[], "b").try_as_basic_value().left();
                let Some(BasicValueEnum::IntValue(b)) = b else {
                    return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", b), fn_name: POP_I32.to_owned() });
                };

                let difference = self.builder.build_int_sub(a, b, "difference");
                self.builder.build_call(push_i32, &[difference.into()], "");

                type_stack.push(LLVMType::I32);

                Ok(())
            },
            IntrinsicRoutine::Not => {
                let top_type = type_stack.pop();
                if !matches!(top_type, Some(LLVMType::Bool)) {
                    return Err(CompilerError::InvalidTypeStackTypes { expected: Box::new([LLVMType::Bool]), found: Box::new([top_type]) });
                }

                let a = self.builder.build_call(pop_bool, &[], "a").try_as_basic_value().left();
                let Some(BasicValueEnum::IntValue(a)) = a else {
                    return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", a), fn_name: POP_BOOL.to_owned() });
                };

                let const_true = self.types.bool_type.const_int(1, false);
                let inverted = self.builder.build_int_sub(const_true, a, "inverted");

                self.builder.build_call(push_bool, &[inverted.into()], "");
                type_stack.push(LLVMType::Bool);

                Ok(())
            },
            IntrinsicRoutine::Eq => {
                let Some(top_type) = type_stack.pop() else {
                    return Err(CompilerError::EmptyTypeStack);
                };
                let second_type = type_stack.pop();
                match (top_type, second_type) {
                    (LLVMType::String(_), Some(LLVMType::String(_))) => (),
                    _ if second_type != Some(top_type) => {
                        return Err(CompilerError::InvalidTypeStackTypes { expected: Box::new([top_type, top_type]), found: Box::new([Some(top_type), second_type]) });
                    },
                    _ => ()
                }
                 
                match top_type {
                    LLVMType::I32 => {
                        let a = self.builder.build_call(pop_i32, &[], "a").try_as_basic_value().left();
                        let Some(BasicValueEnum::IntValue(a)) = a else {
                            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", a), fn_name: POP_I32.to_owned() });
                        };
                        let b = self.builder.build_call(pop_i32, &[], "b").try_as_basic_value().left();
                        let Some(BasicValueEnum::IntValue(b)) = b else {
                            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", b), fn_name: POP_I32.to_owned() });
                        };

                        let eq = self.builder.build_int_compare(IntPredicate::EQ, a, b, "eq");
                        self.builder.build_call(push_bool, &[eq.into()], "");
                    },
                    LLVMType::Bool => {
                        let a = self.builder.build_call(pop_bool, &[], "a").try_as_basic_value().left();
                        let Some(BasicValueEnum::IntValue(a)) = a else {
                            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", a), fn_name: POP_BOOL.to_owned() });
                        };
                        let b = self.builder.build_call(pop_bool, &[], "b").try_as_basic_value().left();
                        let Some(BasicValueEnum::IntValue(b)) = b else {
                            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", b), fn_name: POP_BOOL.to_owned() });
                        };

                        let eq = self.builder.build_int_compare(IntPredicate::EQ, a, b, "eq");
                        self.builder.build_call(push_bool, &[eq.into()], "");

                    },
                    LLVMType::Char => {
                        let a1 = self.builder.build_call(pop_i16, &[], "a1").try_as_basic_value().left();
                        let Some(BasicValueEnum::IntValue(a1)) = a1 else {
                            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", a1), fn_name: POP_I16.to_owned() });
                        };
                        
                        let a2 = self.builder.build_call(pop_i16, &[], "a2").try_as_basic_value().left();
                        let Some(BasicValueEnum::IntValue(a2)) = a2 else {
                            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", a2), fn_name: POP_I16.to_owned() });
                        };

                        let b1 = self.builder.build_call(pop_i16, &[], "b1").try_as_basic_value().left();
                        let Some(BasicValueEnum::IntValue(b1)) = b1 else {
                            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", b1), fn_name: POP_I16.to_owned() });
                        };

                        let b2 = self.builder.build_call(pop_i16, &[], "b2").try_as_basic_value().left();
                        let Some(BasicValueEnum::IntValue(b2)) = b2 else {
                            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", b2), fn_name: POP_I16.to_owned() });
                        };

                        let eq1 = self.builder.build_int_compare(IntPredicate::EQ, a1, b1, "eq1");
                        let eq2 = self.builder.build_int_compare(IntPredicate::EQ, a2, b2, "eq2");
                        let eq = self.builder.build_and(eq1, eq2, "eq");
                        self.builder.build_call(push_bool, &[eq.into()], "eq");
                    },
                    LLVMType::String(_) => {
                        let str_compare = self.get_function(STR_COMPARE)?;
                        self.builder.build_call(str_compare, &[], "");
                    }
                }

                type_stack.push(LLVMType::Bool);

                Ok(())
            },
            IntrinsicRoutine::Swap => {
                let top_type = type_stack.pop().ok_or(CompilerError::EmptyTypeStack)?;
                let second_type = type_stack.pop().ok_or(CompilerError::EmptyTypeStack)?;

                let top_value = pop_value(top_type)?;
                let second_value = pop_value(second_type)?;

                push_value(top_value, top_type)?;
                push_value(second_value, second_type)?;

                type_stack.push(top_type);
                type_stack.push(second_type);

                Ok(())
            },
            IntrinsicRoutine::Mod => {
                let top_type = type_stack.pop().ok_or(CompilerError::EmptyTypeStack)?;
                let second_type = type_stack.pop().ok_or(CompilerError::EmptyTypeStack)?;

                if !matches!((top_type, second_type), (LLVMType::I32, LLVMType::I32)) {
                    return Err(CompilerError::InvalidTypeStackTypes { expected: Box::new([LLVMType::I32, LLVMType::I32]), found: Box::new([Some(top_type), Some(second_type)]) });
                }

                let top_int = self.call_int_return(pop_i32, &[], "top_int", POP_I32)?;
                let second_int = self.call_int_return(pop_i32, &[], "second_int", POP_I32)?;

                let remainer = self.builder.build_int_signed_rem(top_int, second_int, "remainder");

                self.builder.build_call(push_i32, &[remainer.into()], "");
                type_stack.push(LLVMType::I32);

                Ok(())
            },
            IntrinsicRoutine::Drop => {
                let top_type = type_stack.pop().ok_or(CompilerError::EmptyTypeStack)?;

                _ = pop_value(top_type)?;

                Ok(())
            },
            IntrinsicRoutine::Clone => {
                let top_type = type_stack.pop().ok_or(CompilerError::EmptyTypeStack)?;

                let top_value = pop_value(top_type)?;
                push_value(top_value.clone(), top_type)?;
                push_value(top_value, top_type)?;

                type_stack.push(top_type);
                type_stack.push(top_type);

                Ok(())

            },
            IntrinsicRoutine::CloneOver => {
                let top_type = type_stack.pop().ok_or(CompilerError::EmptyTypeStack)?;
                let second_type = type_stack.pop().ok_or(CompilerError::EmptyTypeStack)?;

                let top_value = pop_value(top_type)?;
                let second_value = pop_value(second_type)?;

                push_value(second_value.clone(), second_type)?;
                push_value(top_value, top_type)?;
                push_value(second_value.clone(), second_type)?;

                type_stack.push(second_type);
                type_stack.push(top_type);
                type_stack.push(second_type);

                Ok(())
            },
            IntrinsicRoutine::GreaterThan => {
                let top_type = type_stack.pop().ok_or(CompilerError::EmptyTypeStack)?;
                let second_type = type_stack.pop().ok_or(CompilerError::EmptyTypeStack)?;

                if !matches!((top_type, second_type), (LLVMType::I32, LLVMType::I32)) {
                    return Err(CompilerError::InvalidTypeStackTypes { expected: Box::new([LLVMType::I32, LLVMType::I32]), found: Box::new([Some(top_type), Some(second_type)]) });
                }

                let top_int = self.call_int_return(pop_i32, &[], "top_int", POP_I32)?;
                let second_int = self.call_int_return(pop_i32, &[], "second_int", POP_I32)?;

                let greater_than = self.builder.build_int_compare(IntPredicate::SGT, top_int, second_int, "greater_than");
                self.builder.build_call(push_bool, &[greater_than.into()], "");

                type_stack.push(LLVMType::Bool);

                Ok(())
            },
            IntrinsicRoutine::StringConcat => {
                let top_type = type_stack.pop();
                let second_type = type_stack.pop();

                match (top_type, second_type) {
                    (Some(LLVMType::String(top_str_type)), Some(LLVMType::String(second_str_type))) => Ok((top_str_type, second_str_type)),
                    (Some(_), Some(_)) => Err(CompilerError::InvalidTypeStackTypes {
                        expected: Box::new([LLVMType::String(LLVMString::Constant), LLVMType::String(LLVMString::Constant)]),
                        found: Box::new([top_type, second_type]),
                    }),
                    _ => Err(CompilerError::EmptyTypeStack),
                }?;

                self.builder.build_call(str_concat, &[], STR_CONCAT);

                type_stack.push(LLVMType::String(LLVMString::Stack));

                Ok(())
            }
        }
    }
}
