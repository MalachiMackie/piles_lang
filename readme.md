# Piles

A type safe stack based programming language made for fun

Each "statement" will either push a value on the stack, or pop a value off the stack.

# Features

## Types

The following types are implement:
- 32 bit integers
- char
- string
- bool

## Routines

A routine is called with the syntax: `!my_routine`
A routine is defined beginning with a double bang, the routine name, a pipe separator, which types the routine takes as inputs from the stack, and which types it will push back on the stack.

Example:
```
!!clone_and_print_line | i32 -> i32 {
	!clone !print !println
}
```

# Examples

examples are in `./piles_tests/`

a pile program can be run either interpreted, or compiled

## Interpreted

`cargo run interpret <pile_program>.pi`

## Compilation
Currently, compilation requires llvm and gcc external tools. Also the msvcrt library is needed as a linked library

To compile:
1. `cargo run compile <pile_program>.pi`
1. `llc <pile_program>.ll`
1. `gcc <pile_program>.s -lmsvcrt -o <pile_program>.exe`
1. `./<pile_program>.exe`
