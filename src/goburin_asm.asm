format ELF64

;;; constants

        ;;; FILE DESCRIPTORS
        STDIN     equ 0
        STDOUT    equ 1
        STDERR    equ 2

        ;;; SYSCALLS NUMBERS
        SYS_WRITE equ 1
        SYS_OPEN  equ 2
        SYS_CLOSE equ 3
        SYS_EXIT  equ 60

        ;; FILE PERMISIONS
        O_RDONLY  equ 000h
        O_WRONLY  equ 001h
        O_RDWR	  equ 002h
        O_CREAT	  equ 040h
        O_TRUNC	  equ 200h

;;; macros

macro syscall1 value, arg1 {
        mov rax, value
        mov rdi, arg1
        syscall        
}

macro syscall3 value, arg1, arg2, arg3 {
        mov rax, value
        mov rdi, arg1
        mov rsi, arg2 
        mov rdx, arg3 
        syscall  
}

macro exit value { syscall1 SYS_EXIT, value }
macro write fd, buf, len { syscall3 SYS_WRITE, fd, buf, len }
macro open fd, flags, mode { syscall3 SYS_OPEN, fd, flags, mode }

;;; CODE SECTION

section '.text' executable
public _start

_start:
; open output file
        mov rbx, O_WRONLY
        or rbx, O_CREAT
        or rbx, O_TRUNC
        open output, rbx, 777o
        cmp rax, 0
        jl .failed_open
        mov [output_fd], rax
.loop:
        call read_token
        cmp rax, 1 ; 1 is the return when EOF is read
        je .exit
        call compile
        jmp .loop
.exit:
        exit 0
;;; FAIL STATES
.failed_open:
        push rax
        write STDERR, failed_open_msg, failed_open_msg_len
        pop rax
        exit rax
.failed_write: 
        push rax
        write STDERR, failed_write_msg, failed_write_msg_len
        pop rax
        exit rax
.failed_string_len_limit:
        push rax
        write STDERR, failed_string_len_limit_msg, failed_string_len_limit_msg_len
        pop rax
        exit rax
        

; Advance the `input_idx` until a `white_space` character is encountered
; writting any non-`whitespace` char in `string`
;
; ### destroys
; eax, ebx, rdi, rsi
; 
; ### pseudo-code ###
; loop
;   skip_whitespaces()
;   char = input[input_idx]
;   input_idx += 1
;   if char is [null, new_line, space, ...] return
;
;   string[string_idx] = char
;   string_idx += 1
; end_loop
; ###
read_token:
; reset `string_idx`
        mov [string_idx], dword 0
.read_char:
; read char from `input`
        mov rdi, input
        mov rsi, [input_idx]
        movzx ebx, byte [rdi + rsi]
; advance index
        inc rsi
        mov [input_idx], rsi
; check if EOF
        cmp [input_idx], dword input_len
        jge .eof
; check for `whitespaces`
        cmp ebx, 32 ; is char an ASCII 'control' char + space ?
        jle .whitespace
; char is valid
; write char to `string`
        mov rdi, string
        mov rsi, [string_idx]
        mov [rdi + rsi], ebx
; advance index
        inc rsi
        mov [string_idx], rsi
; check if > STRING_LEN
        cmp [string_idx], dword string_len
        jge _start.failed_string_len_limit
        jmp .read_char
.whitespace:
        cmp [string_idx], dword 0
        je .read_char
        mov eax, 0
        ret
.eof:
        mov eax, 1
        ret

;        
compile:
        write [output_fd], string, [string_idx] 
        ret

;;; DATA SECTION

section '.rodata' writable

input: file "src/goburin_forth.forth"
input_len = $ - input
input_idx: dq 0

output: db "build/goburin_forth", 0
output_fd: dq 0

string: times 32 db 0
string_len = $ - string
string_idx: dq 0

;;; FAIL MESSAGES
failed_open_msg: db "Failed to open file", 10, 0
failed_open_msg_len = $ - failed_open_msg

failed_write_msg: db "Failed to write file", 10, 0
failed_write_msg_len = $ - failed_write_msg

failed_string_len_limit_msg: db "Token exceeded string limit of 32", 10, 0
failed_string_len_limit_msg_len = $ - failed_string_len_limit_msg


