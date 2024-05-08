# GOBURIN
My personal hobby "goblin" programming language. Meant to be simple and moddable.

# TODO
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

# TinyGob
TinyGob will be the first language we code an interpreter for. Here is what the language will support in the beginning:
- Numerical variables
- Basic arithmetic
- If statements
- While loops
- Print text and numbers
- Input numbers
- Labels and goto
- Comments

The base structure of TiniGob will be Lexer -> Parser -> Emitter.

