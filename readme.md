# compiling
1. `piles.exe compile ./pile_examples/if.pi` will output if.ll
1. `llc if.ll` will output if.s
1. `gcc if.s -o if.exe` will output if.exe that you can run
