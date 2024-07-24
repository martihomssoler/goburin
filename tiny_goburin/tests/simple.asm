format ELF64

section '.text' executable
public _start
extrn printf
extrn exit

_start:
mov rcx, 2
mov rdi, rcx
call exit

section '.data' writeable

fmt_int: db "%d", 0
fmt_char: db "%c", 0
fmt_string: db "%s", 0
