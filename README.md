# GOBURIN

Goburin is a special-purpose family of programming-languages and toolchains to build and mantain software.

# TODO
- [ ] Self-hosted
  - [ ] Add support for: 
      - [ ] string literals
      - [ ] system calls (to read, write and so on)
      - [ ] reading command line arguments (to get the path of the file to parse) 
  - [ ] Implement the compiler in Goburin

## Links
Linux Syscall Table -> (https://filippo.io/linux-syscall-table/)
(https://www.youtube.com/watch?v=8QP2fDBIxjM&list=PLpM-Dvs8t0VbMZA7wW9aR3EtBqe2kinu4)
(https://norasandler.com/2017/11/29/Write-a-Compiler.html)
(http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf)

## FUTURE
- [ ] Frontend
  - [ ] Lexer (takes source string and returns array of tokens)
  - [ ] Parser (takes array of tokens and produces an AbstractSyntaxTree)
  - [ ] Semantic Analysis
  - [ ] Type Checking
- [ ] Interpreter (takes an AST and executes it)
- [ ] Generate Rust (takes an AST and produces Rust code)
- [ ] Byte-code Generator (takes an AST and produces Byte-code)
- [ ] VM (takes Byte-code and executes it)
- [ ] Fast Custom x64 Backend (for fast iteration and development)
- [ ] Link with LLVM Backend 
- [ ] Custom Backend?
  - [ ] Intermediate Language (takes an AST and generates a low-level IR)
  - [ ] Code Generator (takes the IR and produces machine code)
- [ ] Middleend? 
  - [ ] Intermediate Representation
  - [ ] Optimizations
    - [ ] Dead-code elimination
    - [ ] Reachability analysis
    - [ ] Constant propagation
    - [ ] Loop vectorization / unrolling
