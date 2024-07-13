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

* [Introduction to Compilers and Language Design](https://www3.nd.edu/~dthain/compilerbook/compilerbook.pdf)
* https://www.youtube.com/watch?v=8QP2fDBIxjM&list=PLpM-Dvs8t0VbMZA7wW9aR3EtBqe2kinu4
* https://norasandler.com/2017/11/29/Write-a-Compiler.html
* http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf
* https://boxbase.org/entries/2018/mar/5/hindley-milner/
* https://www.labri.fr/perso/casteran/CoqArt/Tsinghua/C5.pdf
* https://github.com/wh5a/Algorithm-W-Step-By-Step
* http://steshaw.org/hm/hindley-milner.pdf
* https://course.ccs.neu.edu/cs4410sp19/lec_type-inference_notes.html

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

# Low Goburin

## Hello World
```cpp
let std = import std

pub main : () = (
    std.println("Hello, {}!", "world")
)
```

## Comments
Goburin supports single-line comments only. Doc comments will be added in the future.

```cpp
let println = import std.println

pub main : () = (
    // Comments start with "//" and end at the next end of line
    // println("Hello?") <-- comments are ignored and not executed 
    
    println("Hello, {}!", "world")
)
```

## Values
Goburin, at the moment, only supports bools and un/signed 8/32 and "word-sized" integers.

```cpp
pub main : () = (
    let a: bool,  // true or false
    let b: i8,    // signed 8-bit integer
    let c: u8,    // unsigned 8-bit integer
    let d: i32,   // signed 32-bit integer
    let e: u32,   // unsigned 32-bit integer
    let f: iptr,  // signed sized integer
    let g: uptr,  // unsigned sized integer
)
```

## Bug Fixes Links

[Do NOT use _exit!!!](https://stackoverflow.com/questions/38379553/using-printf-in-assembly-leads-to-empty-output-when-piping-but-works-on-the-ter)
