# Goburin

# Primitives

- `:`: start a word definition
- `;`: end a word definition
- `+`: add two integers
- `-`: subtract two integers
- `*`: multiply two integers
- `/`: divide two integers
- `@`: read the value of a cell (8 bytes) from memory
- `!`: write a value to a cell (8 bytes) in memory
- `0=`: put true (1) on the stack if current top of the stack is equal to zero, otherwise put false (0).
- `ps@`: get the current value of the parameter stack pointer
- `ps!`: write a new address to the parameter stack pointer
- `rs@`: get the current value of the return stack pointer
- `rs!`: write a new address to the parameter stack pointer
- `mem`: give the pointer to the start of user memory
- `?ret`: return current word if the top of stack is true (non zero)
- `?exit`: exit current word if the top of stack is true (non zero)

## Future
- `and`: bitwise AND two integers
- `lsr`: logical right shift an integer
- `syscall0`-`syscall6`: perform a Linux system call with 0 to 6 arguments.

# Two Stacks

- `rbp` and `rsp`: hold the data stack and the address stack
- `r15`: holds the pointer to the memory block

# Info Links

- [fasm Documentation](https://flatassembler.net/docs.php)
- [x86_64 Cheat Sheet](https://web.stanford.edu/class/cs107/resources/x86-64-reference.pdf)
- [Linux Syscalls 1](https://chromium.googlesource.com/chromiumos/docs/+/master/constants/syscalls.md)
- [Linux Syscalls 2](https://blog.rchapman.org/posts/Linux_System_Call_Table_for_x86_64/)
- [ELF Format Cheat Sheet](https://gist.github.com/x0nu11byt3/bcb35c3de461e5fb66173071a2379779)
- [ELF64 Object File Specification](https://irix7.com/techpubs/007-4658-001.pdf)
- [Writing a Linux ELF Binary by Hand](https://www.youtube.com/watch?v=JM9jX2aqkog)
- [Crafting executables from raw bytes - Kay Lack](https://www.youtube.com/watch?v=cX5tQJhuNeY) <- life saver!
- [Online x86 Assembler](https://defuse.ca/online-x86-assembler.htm) <- life saver!
- ...

# Useful Tools

- `fasm`
- `nm`
- `hexyl`
- `just`
- `watchexe`
- `gdb`
- `gf`
- ...

# Inspiration

- [Starforth v1](https://elektito.com/2023/07/08/starforth-2/)
- [Starforth v1 Source Code](https://git.sr.ht/~elektito/starforth/tree/ec7a4a14baa5faa79a7e3d65c9f68ef0734fc62b)
- [Forget C - Assembly is All You Need](https://www.youtube.com/watch?v=hzjBdIJ9Ycs)
- ...

# Other Links

- [Ratfactor Forth Article](https://ratfactor.com/forth/the_programming_language_that_writes_itself.html)
- [Hacker News of Ratfactor Forth Article](https://news.ycombinator.com/item?id=45639250)
