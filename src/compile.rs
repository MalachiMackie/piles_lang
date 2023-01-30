use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::{Module, Linkage};
use inkwell::AddressSpace;
use crate::PileProgram;

#[derive(Debug)]
pub(crate) enum CompilerError{
    Something
}

impl PileProgram {
    pub(crate) fn compile(&self, file_name: &str) -> Result<(), CompilerError> {

        let slash_index = file_name.chars().rev().position(|c| c == '/' || c == '\\').map(|index| file_name.len() - 1 - index);

        let file_part = if let Some(slash_index) = slash_index {
            println!("{}", slash_index);
            &file_name[(slash_index + 1)..]
        } else {
            file_name
        };
        
        let context = Context::create();
        let module = context.create_module(file_part);
        let code_gen = CodeGen {
            context: &context,
            module,
            builder: context.create_builder()
        };


        code_gen.compile()?;

        let ll_file = &file_name[..(file_name.len() - 3)];
        println!("{}", ll_file);

        code_gen.module.print_to_file(format!("{}.ll", ll_file)).map_err(|_| CompilerError::Something)?;

        println!("{}", code_gen.module.print_to_string());
        Ok(())
    }
}

type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    fn compile(&self) -> Result<(), CompilerError> {
        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();
        let str_type = self.context.i8_type().ptr_type(AddressSpace::from(0u16));
        let void_type = self.context.void_type();
        let printf_type = i32_type.fn_type(&[str_type.into()], true);

        let function_type = void_type.fn_type(&[], false);

        let function = self.module.add_function("main", function_type, None);

        // `printf` is same to `puts`
        let printf = self.module.add_function("puts", printf_type, Some(Linkage::External));

        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        let global_name = "my_global";
        let string = "Hello World";

        let ty = self.context.i8_type().array_type(string.len() as u32);
        let gv = self.module.add_global(ty, Some(AddressSpace::from(0u16)), global_name);
        gv.set_linkage(Linkage::Internal);
        gv.set_initializer(&self.context.const_string(string.as_ref(), false));

        let pointer_value = self.builder.build_pointer_cast(
            gv.as_pointer_value(),
            self.context.i8_type().ptr_type(AddressSpace::from(0u16)),
            global_name,
        );

        self.builder.build_call(printf, &[pointer_value.into()], "");
        self.builder.build_return(None);
        Ok(())
    }
}
