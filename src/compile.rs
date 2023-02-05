use inkwell::{OptimizationLevel, IntPredicate};
use inkwell::builder::Builder;
use inkwell::context::{Context};
use inkwell::types::{IntType, VoidType, PointerType, ArrayType};
use inkwell::module::{Module, Linkage};
use inkwell::AddressSpace;
use inkwell::values::{AnyValue, AsValueRef, BasicValueEnum, BasicValue, PointerValue, ArrayValue, IntValue, AggregateValueEnum, FunctionValue};
use crate::{Type, Token};
use crate::{PileProgram, Value, routines::{Routine, IntrinsicRoutine}};
use std::collections::VecDeque;


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

const PUSH_BOOL: &str = "push_bool";
const PUSH_BYTE: &str = "push_byte";
const PUSH_I16: &str = "push_i16";
const PUSH_I32: &str = "push_i32";
const PUSH_I64: &str = "push_i64";

const POP_BOOL: &str = "pop_bool";
const POP_BYTE: &str = "pop_byte";
const POP_I16: &str = "pop_i16";
const POP_I32: &str = "pop_i32";
const POP_I64: &str = "pop_i64";

const PRINT_STR: &str = "print_str";
const PRINT_I32: &str = "print_i32";
const PRINT_BOOL: &str = "print_bool";
const PRINT_CHAR: &str = "print_char";

const PRINTF: &str = "printf";
const PRINT_D: &str = "print_d";

impl PileProgram {
    pub(crate) fn compile(&self, file_name: &str) -> Result<(), CompilerError> {

        let slash_index = file_name.chars().rev().position(|c| c == '/' || c == '\\').map(|index| file_name.len() - 1 - index);
        let file_part = if let Some(slash_index) = slash_index {
            &file_name[(slash_index + 1)..]
        } else {
            file_name
        };
        
        let context = Context::create();
        let mut code_gen = CodeGen::new(file_part, &context);

        code_gen.build_push_int(2, PUSH_I16)?;
        code_gen.build_push_int(4, PUSH_I32)?;
        code_gen.build_push_int(8, PUSH_I64)?;
        code_gen.build_push_int(1, PUSH_BYTE)?;
        code_gen.build_push_bool()?;

        code_gen.build_pop_int(2, POP_I16)?;
        code_gen.build_pop_int(4, POP_I32)?;
        code_gen.build_pop_int(8, POP_I64)?;
        code_gen.build_pop_int(1, POP_BYTE)?;
        code_gen.build_pop_bool()?;

        code_gen.build_print_i32()?;
        code_gen.build_print_str()?;
        code_gen.build_print_bool()?;
        code_gen.build_print_char()?;

        let mut type_stack = Vec::new();

        code_gen.start_top_level_statements();
        for token in self.tokens.iter() {
            match token {
                Token::Constant(value) => code_gen.push_constant(value, &mut type_stack)?,
                Token::If => todo!(),
                Token::While => todo!(),
                Token::Block(block) => todo!(),
                Token::RoutineCall(routine_name) =>{
                    let Some(routine) = self.routines.get(routine_name) else {
                        return Err(CompilerError::MissingRoutine(routine_name.to_owned()));
                    };
                    code_gen.call_routine(routine, &mut type_stack)?
                },
            }
        }
        code_gen.end_top_level_statements();

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
    global_strings: Vec<String>,
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

        Self {
            context,
            module,
            builder,
            types,
            global_strings: Vec::new(),
        }
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
        
        let return_value = self.builder.build_call(printf, &[str_ptr.into()], "ret_value");
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

        let return_value = self.builder.build_call(printf, &[false_global_value.as_pointer_value().into()], "");
        let Some(return_value) = return_value.try_as_basic_value().left() else {
            return Err(CompilerError::MissingReturnValue(PRINTF.to_owned()));
        };

        self.builder.build_return(Some(&return_value));

        self.builder.position_at_end(is_true_block);

        let return_value = self.builder.build_call(printf, &[true_global_value.as_pointer_value().into()], "");
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
        let fmt_global_value = self.module.add_global(format_string.get_type(), Some(AddressSpace::from(0u16)), "true_str");
        fmt_global_value.set_linkage(Linkage::Internal);
        fmt_global_value.set_initializer(format_string);

        let printf = self.module.get_function(PRINTF).ok_or_else(|| CompilerError::MissingRoutine(PRINTF.to_owned()))?;
        let return_value = self.builder.build_call(printf, &[fmt_global_value.as_pointer_value().into(), bytes.into()], "return_value");
        let return_value = return_value.try_as_basic_value().left().ok_or_else(|| CompilerError::MissingReturnValue(PRINTF.to_owned()))?;

        self.builder.build_return(Some(&return_value));

        Ok(())
    }

    fn push_constant(&mut self, value: &Value, type_stack: &mut Vec<LLVMType>) -> Result<(), CompilerError> {
        match value {
            Value::String(str_value) => {
                type_stack.push(LLVMType::String(LLVMString::Constant));
                let global_name = format!("string_{}", self.global_strings.len());
                let const_string = &self.context.const_string(str_value.as_ref(), true);
                let global_value = self.module.add_global(const_string.get_type(), Some(AddressSpace::from(0u16)), &global_name);
                global_value.set_linkage(Linkage::Internal);
                global_value.set_initializer(const_string);

                let str_ptr = self.builder.build_pointer_cast(
                    global_value.as_pointer_value(),
                    self.context.i8_type().ptr_type(AddressSpace::from(0u16)),
                    "str_ptr",
                );
                self.global_strings.push(global_name);

                let ptr_int = self.builder.build_ptr_to_int(str_ptr, self.types.i64_type, "ptr_as_int");
                let push_i64 = self.module.get_function(PUSH_I64).ok_or_else(|| CompilerError::MissingRoutine(PUSH_I64.to_owned()))?;
                
                self.builder.build_call(push_i64, &[ptr_int.into()], "");
                
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
            Routine::Intrinsic { signiture, routine } => self.call_intrinsic(routine, type_stack),
            Routine::Pile { signiture, routine } => todo!(),
        }
    }

    fn call_intrinsic(&self, routine: &IntrinsicRoutine, type_stack: &mut Vec<LLVMType>) -> Result<(), CompilerError> {

        fn get_function<'a>(code_gen: &'a CodeGen, name: &'static str) -> Result<FunctionValue<'a>, CompilerError> {
            code_gen.module.get_function(name).ok_or_else(|| CompilerError::MissingRoutine(name.to_owned()))
        }

        let pop_bool = get_function(self, POP_BOOL)?;
        let pop_byte = get_function(self, POP_BYTE)?;
        let pop_i16 = get_function(self, POP_I16)?;
        let pop_i64 = get_function(self, POP_I64)?;
        let pop_i32 = get_function(self, POP_I32)?;

        let push_bool = get_function(self, PUSH_BOOL)?;
        let push_byte = get_function(self, PUSH_BYTE)?;
        let push_i32 = get_function(self, PUSH_I32)?;

        let print_str = get_function(self, PRINT_STR)?;
        let print_i32 = get_function(self, PRINT_I32)?;
        let print_bool = get_function(self, PRINT_BOOL)?;
        let print_char = get_function(self, PRINT_CHAR)?;
        match routine {
            IntrinsicRoutine::Print => {
                let top_type = type_stack.pop().expect("Type stack should not be empty");

                match top_type {
                    LLVMType::String(LLVMString::Constant) => {
                        let str_ptr_int = self.builder.build_call(pop_i64, &[], "str_ptr").try_as_basic_value().left();
                        let Some(BasicValueEnum::IntValue(str_ptr_int)) = str_ptr_int else {
                            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", str_ptr_int), fn_name: POP_I64.to_owned() });
                        };

                        let str_ptr = self.builder.build_int_to_ptr(str_ptr_int, self.types.str_type, "str_ptr");

                        self.builder.build_call(print_str, &[str_ptr.into()], "");
                    },
                    LLVMType::String(LLVMString::Stack) => {
                        todo!();
                    },
                    LLVMType::I32 => {
                        let int_value = self.builder.build_call(pop_i32, &[], "int_value").try_as_basic_value().left();
                        let Some(BasicValueEnum::IntValue(int_value)) = int_value else {
                            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", int_value), fn_name: POP_I32.to_owned() });
                        };

                        self.builder.build_call(print_i32, &[int_value.into()], "");
                    }
                    LLVMType::Bool => {
                        let byte_value = self.builder.build_call(pop_byte, &[], "byte_value").try_as_basic_value().left();
                        let Some(BasicValueEnum::IntValue(byte_value)) = byte_value else {
                            return Err(CompilerError::InvalidReturnValue { expected: "int".to_owned(), found: format!("{:?}", byte_value), fn_name: POP_BYTE.to_owned() });
                        };

                        self.builder.build_call(print_bool, &[byte_value.into()], "");
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
                            return Err(CompilerError::InvalidInsertValueResult { expected: "array".to_owned(), found: format!("{:?}", array_value).to_owned() });
                        };

                        self.builder.build_call(print_char, &[array_value.into()], "");
                    }
                }

                Ok(())
            },
            IntrinsicRoutine::PrintLine => {
                let Some(puts) = self.module.get_function(PRINTF) else {
                    return Err(CompilerError::MissingRoutine(PRINTF.to_owned()));
                };
                let new_line = self.module.get_global("new_line").unwrap();
                self.builder.build_call(puts, &[new_line.as_pointer_value().into()], "");
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
                    LLVMType::String(_) => todo!("string comparison")
                }

                type_stack.push(LLVMType::Bool);

                Ok(())
            },
            IntrinsicRoutine::Swap => todo!(),
            IntrinsicRoutine::Mod => todo!(),
            IntrinsicRoutine::Drop => todo!(),
            IntrinsicRoutine::Clone => todo!(),
            IntrinsicRoutine::CloneOver => todo!(),
            IntrinsicRoutine::GreaterThan => todo!(),
            IntrinsicRoutine::StringConcat => todo!(),
        }
    }

    fn start_top_level_statements(&self) {
        let main_fn_type = self.types.void_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);
        let basic_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(basic_block);
    }

    fn end_top_level_statements(&self)  {
        self.builder.build_return(None);
    }

    
}
