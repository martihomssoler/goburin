format ELF64

STDIN  = 0
STDOUT = 1
STDERR = 2

SYS_WRITE = 1
SYS_OPEN  = 2
SYS_CLOSE = 3
SYS_EXIT  = 60

O_RDONLY = 00h
O_WRONLY = 01h
O_RDWR	 = 02h
O_CREAT	 = 40h

macro syscall1 value, arg1
{
        mov rax, value
        mov rdi, arg1
        syscall        
}

macro syscall3 value, arg1, arg2, arg3
{
        mov rax, value
        mov rdi, arg1
        mov rsi, arg2
        mov rdx, arg3
        syscall        
}

macro exit value
{
        syscall1 SYS_EXIT, value
}

macro write fd, buf, len
{
        syscall3 SYS_WRITE, fd, buf, len
}

macro open filename, flags, mode
{
        syscall3 SYS_OPEN, filename, flags, mode
}

section '.text' executable
public _start
_start:
        ; rax <- O_WRONLY | O_CREAT
        mov rbx, O_WRONLY
        or rbx, O_CREAT
        
        open output_path, rbx, 777o
        cmp rax, 0 ; check for error
        jl .failed_open

        mov rdx, rax
        write rdx, input_path, input_path_len
        cmp rax, 0 ; check for error
        jl .failed_write
        
        exit 0
        ret

.failed_open:
        push rax
        write STDERR, failed_open_msg, failed_open_msg_len
        pop rax
        exit rax
        ret

.failed_write: 
        push rax
        write STDERR, failed_write_msg, failed_write_msg_len
        pop rax
        exit rax
        ret

section '.rodata' writable
input_path: file "src/goburin_forth.forth"
input_path_len = $ - input_path

output_path: db "build/goburin_forth", 0

failed_open_msg: db "Failed to open file", 0
failed_open_msg_len = $ - failed_open_msg
failed_write_msg: db "Failed to write file", 0
failed_write_msg_len = $ - failed_write_msg
