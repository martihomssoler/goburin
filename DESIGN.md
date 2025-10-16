# Goburin

# Primitives

All cells, aka variables, are 64-bit wide, aka quad-word.

- `push` / `pop`: cell to/from the stack
- `r>` / `>r`: cell to/from the return-stack
- `.`: return current dictionary pointer
- `,`: store cell at current dictionary pointer an increment the pointer
- `load` / `store` cell from/to memory address

# Info Links

- [fasm Documentation](https://flatassembler.net/docs.php)
- [x86_64 Cheat Sheet](https://web.stanford.edu/class/cs107/resources/x86-64-reference.pdf)
- [Linux Syscalls 1](https://chromium.googlesource.com/chromiumos/docs/+/master/constants/syscalls.md)
- [Linux Syscalls 2](https://blog.rchapman.org/posts/Linux_System_Call_Table_for_x86_64/)
- [ELF Format Cheat Sheet](https://gist.github.com/x0nu11byt3/bcb35c3de461e5fb66173071a2379779)
- [ELF64 Object File Specification](https://irix7.com/techpubs/007-4658-001.pdf)
- [Writing a Linux ELF Binary by Hand](https://www.youtube.com/watch?v=JM9jX2aqkog)
- [Crafting executables from raw bytes - Kay Lack](https://www.youtube.com/watch?v=cX5tQJhuNeY) <- life saver!
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
