; ModuleID = 'recursive_routine.pi'
source_filename = "recursive_routine.pi"

@my_global = internal global [11 x i8] c"Hello World"

define void @main() {
entry:
  %0 = call i32 (ptr, ...) @puts(ptr @my_global)
  ret void
}

declare i32 @puts(ptr, ...)
