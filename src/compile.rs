use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::{Context};
use inkwell::types::{IntType, VoidType, PointerType, ArrayType};
use inkwell::module::{Module, Linkage};
use inkwell::AddressSpace;
use inkwell::values::{AnyValue, AsValueRef, BasicValueEnum, BasicValue, PointerValue, ArrayValue, IntValue, AggregateValueEnum};
use crate::{Type, Token};
use crate::{PileProgram, Value, routines::{Routine, IntrinsicRoutine}};
use std::collections::VecDeque;


#[derive(Debug)]
pub(crate) enum CompilerError{
    Something
}

enum LLVMType {
    ConstString,
    String,
    I32,
    Char,
    Bool
}

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
        let mut stack_pointer = 0;
        let mut type_stack = Vec::new();

        code_gen.start_top_level_statements();
        for token in self.tokens.iter() {
            match token {
                Token::Constant(value) => code_gen.push_constant(value, &mut stack_pointer, &mut type_stack)?,
                Token::If => todo!(),
                Token::While => todo!(),
                Token::Block(block) => todo!(),
                Token::RoutineCall(routine_name) =>{
                    let Some(routine) = self.routines.get(routine_name) else {
                        return Err(CompilerError::Something);
                    };
                    code_gen.call_routine(routine, &mut stack_pointer, &mut type_stack)?
                },
            }
        }
        code_gen.end_top_level_statements();

        let ll_file = &file_name[..(file_name.len() - 3)];
        code_gen.module.print_to_file(format!("{}.ll", ll_file)).map_err(|_| CompilerError::Something)?;
        Ok(())
    }
}

type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    types: LLVMValueTypes<'ctx>,
    global_strings: Vec<String>,
}

const STACK_NAME: &str = "pile_stack";
const STACK_SIZE: u32 = 1024u32;

struct LLVMValueTypes<'a> {
    byte_type: IntType<'a>,
    i32_type: IntType<'a>,
    i64_type: IntType<'a>,
    void_type: VoidType<'a>,
    generic_address_space: AddressSpace,
    str_type: PointerType<'a>,
    stack_type: ArrayType<'a>,
}


impl<'ctx> CodeGen<'ctx> {
    fn new(module_name: &str, context: &'ctx Context) -> Self {
        let generic_address_space = AddressSpace::from(0u16);
        let char_type = context.i8_type();
        let str_type = char_type.ptr_type(generic_address_space);
        let byte_type = context.i8_type();
        let stack_type = byte_type.array_type(STACK_SIZE);
        let types = LLVMValueTypes {
            byte_type,
            i32_type: context.i32_type(),
            i64_type: context.i64_type(),
            void_type: context.void_type(),
            generic_address_space,
            str_type,
            stack_type,
        };

        let builder = context.create_builder();
        let module = context.create_module(module_name);

        let stack = module.add_global(stack_type, Some(generic_address_space), STACK_NAME);

        let zero_byte = types.byte_type.const_zero();
        let empty_array: Vec<_> = (0..STACK_SIZE).into_iter().map(|_| zero_byte).collect();

        stack.set_linkage(Linkage::Internal);
        stack.set_initializer(&types.byte_type.const_array(&empty_array));
        

        let printf_type = types.i32_type.fn_type(&[types.str_type.into()], true);
        let printf = module.add_function("printf", printf_type, Some(Linkage::External));
        module.add_function("_putw", types.i64_type.fn_type(&[types.i64_type.into()], true), Some(Linkage::External));

        let new_line_value = context.const_string("\r\n".as_ref(), true);
        let new_line = module.add_global(new_line_value.get_type(), Some(types.generic_address_space), "new_line");
        new_line.set_linkage(Linkage::Internal);
        new_line.set_initializer(&new_line_value);

        let print_d_value = context.const_string("%d".as_ref(), true);
        let print_d = module.add_global(print_d_value.get_type(), Some(types.generic_address_space), "print_d");
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

    fn push_llvm_int<'a>(&'a self, int_value: IntValue<'a>, bytes: u64, mut stack: ArrayValue<'a>, stack_pointer: &mut u32) {
        let stack_pointer_value = self.module.get_global(STACK_NAME).unwrap().as_pointer_value();
                let puts = self.module.get_function("printf").unwrap();
                let global = self.module.get_global("print_d").unwrap();
                let pointer_value = global.as_pointer_value();
                let new_line = self.module.get_global("new_line").unwrap();

        for bits in (0..bytes).rev().map(|i| i * 8) {
            let bits_to_shift = self.types.i32_type.const_int(bits, false);
            let byte = self.builder.build_right_shift(int_value, bits_to_shift, false, "right_shift");
            let trunc = self.builder.build_int_truncate(byte, self.types.byte_type, "trunc");
            let Some(AggregateValueEnum::ArrayValue(insert_result)) = self.builder.build_insert_value(stack, trunc, *stack_pointer, "inserted_byte") else {
                todo!("return err");
            };
            stack = insert_result;
            *stack_pointer += 1;
        }

        self.builder.build_store(stack_pointer_value, stack);
    }

    fn push_llvm_ptr(&self, ptr_value: PointerValue, stack: ArrayValue, stack_pointer: &mut u32) {
        let ptr_int = ptr_value.const_to_int(self.types.i32_type);
                
        self.push_llvm_int(ptr_int, 8, stack, stack_pointer);
    }

    fn push_llvm_i32(&self, i32_value: IntValue, stack: ArrayValue, stack_pointer: &mut u32) {
        self.push_llvm_int(i32_value, 4, stack, stack_pointer);
    }

    fn pop_llvm_i32<'a, 'b>(&'a self, stack: ArrayValue<'a>, stack_pointer: &'b mut u32) -> Result<IntValue, CompilerError> {
        self.pop_llvm_int(4, stack, stack_pointer)
    }

    fn pop_llvm_ptr<'a>(&'a self, ptr_type: PointerType<'a>, stack: ArrayValue<'a>, stack_pointer: &mut u32) -> Result<PointerValue, CompilerError> {
        let ptr_value = self.pop_llvm_int(8, stack, stack_pointer)?;

        let ptr_value = self.builder.build_int_to_ptr(ptr_value, ptr_type, "stack_pointer");
        Ok(ptr_value)
    }


    fn pop_llvm_int<'a, 'b>(&'a self, value_bytes: u64, stack: ArrayValue<'a>, stack_pointer: &'b mut u32) -> Result<IntValue, CompilerError> {
        let mut bytes = VecDeque::with_capacity(4);

        for _ in 0..value_bytes {
            *stack_pointer -= 1;
            let byte = self.builder.build_extract_value(stack, *stack_pointer, "popped_bit");
            let Some(BasicValueEnum::IntValue(byte)) = byte else {
                return Err(CompilerError::Something);
            };
                
            bytes.push_back(byte);
        }
        let mut int_value = bytes.pop_back().unwrap();
        let int_type = match value_bytes {
            4 => self.types.i32_type,
            8 => self.types.i64_type,
            _ => panic!(),
        };
        let const_8 = int_type.const_int(8, false);
        int_value = self.builder.build_int_z_extend(int_value, int_type, "ext_i16");

        for next_byte in bytes.into_iter().rev() {
            int_value = self.builder.build_left_shift(int_value, const_8, "left_shift");
            let next_byte = self.builder.build_int_z_extend(next_byte, int_type, "extend_next_byte");
            int_value = self.builder.build_int_add(int_value, next_byte, "add");
        }

        Ok(int_value)

    }

    fn push_constant(&mut self, value: &Value, stack_pointer: &mut u32, type_stack: &mut Vec<LLVMType>) -> Result<(), CompilerError> {
        match value {
            Value::String(str_value) => {
                type_stack.push(LLVMType::ConstString);
                let global_name = format!("string_{}", self.global_strings.len());
                let const_string = &self.context.const_string(str_value.as_ref(), true);
                let str_type = const_string.get_type();
                let global_value = self.module.add_global(str_type, Some(AddressSpace::from(0u16)), &global_name);
                global_value.set_linkage(Linkage::Internal);
                let chars: &[u8] = str_value.as_ref();
                global_value.set_initializer(const_string);

                let str_ptr = self.builder.build_pointer_cast(
                    global_value.as_pointer_value(),
                    self.context.i8_type().ptr_type(AddressSpace::from(0u16)),
                    "str_ptr",
                );
                self.global_strings.push(global_name);
                let stack = self.module.get_global(STACK_NAME).ok_or(CompilerError::Something)?;
                let stack_array = self.builder.build_load(self.types.stack_type, stack.as_pointer_value(), "stack_load");
                let BasicValueEnum::ArrayValue(stack_array) = stack_array else {
                    return Err(CompilerError::Something)
                };

                self.push_llvm_ptr(str_ptr, stack_array, stack_pointer);
                
                Ok(())
            },
            Value::I32(i32_value) => {
                type_stack.push(LLVMType::I32);
                let stack = self.module.get_global(STACK_NAME).ok_or(CompilerError::Something)?;
                let stack_array = self.builder.build_load(self.types.stack_type, stack.as_pointer_value(), "stack_load");
                let BasicValueEnum::ArrayValue(stack_array) = stack_array else {
                    return Err(CompilerError::Something);
                };

                self.push_llvm_i32(self.types.i32_type.const_int(*i32_value as u64, false), stack_array, stack_pointer);

                Ok(())
            },
            Value::Char(char_value) => todo!(),
            Value::Bool(bool_value) => todo!(),
        }
    }
    fn call_routine(&self, routine: &Routine, stack_pointer: &mut u32, type_stack: &mut Vec<LLVMType>) -> Result<(), CompilerError> {
        match routine {
            Routine::Intrinsic { signiture, routine } => self.call_intrinsic(routine, stack_pointer, type_stack),
            Routine::Pile { signiture, routine } => todo!(),
        }
    }

    fn call_intrinsic(&self, routine: &IntrinsicRoutine, stack_pointer: &mut u32, type_stack: &mut Vec<LLVMType>) -> Result<(), CompilerError> {
        match routine {
            IntrinsicRoutine::Print => {
                let top_type = type_stack.pop().expect("Type stack should not be empty");

                let stack = self.module.get_global(STACK_NAME).ok_or(CompilerError::Something)?;
                let stack_array = self.builder.build_load(self.types.stack_type, stack.as_pointer_value(), "stack_load");
                let BasicValueEnum::ArrayValue(array_value) = stack_array else {
                    return Err(CompilerError::Something);
                };
                
                let Some(puts) = self.module.get_function("printf") else {
                    return Err(CompilerError::Something);
                };

                match top_type {
                    LLVMType::ConstString => {
                        let ptr_type = self.types.byte_type.array_type(1).ptr_type(self.types.generic_address_space);
                        let str_ptr = self.pop_llvm_ptr(ptr_type, array_value, stack_pointer)?;
                        self.builder.build_call(puts, &[str_ptr.into()], "");
                    },
                    LLVMType::String => {
                        todo!();
                    },
                    LLVMType::I32 => {
                        let i32_value = self.pop_llvm_i32(array_value, stack_pointer)?;
                        let print_d = self.module.get_global("print_d").unwrap();
                        self.builder.build_call(puts, &[print_d.as_pointer_value().into(), i32_value.into()], "");
                    }
                    LLVMType::Bool => {
                        todo!()
                    },
                    LLVMType::Char => {
                        todo!()
                    }
                }

                Ok(())
            },
            IntrinsicRoutine::PrintLine => {
                let Some(puts) = self.module.get_function("printf") else {
                    return Err(CompilerError::Something);
                };
                let new_line = self.module.get_global("new_line").unwrap();
                self.builder.build_call(puts, &[new_line.as_pointer_value().into()], "");
                Ok(())
            }
            _ => Ok(())
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
