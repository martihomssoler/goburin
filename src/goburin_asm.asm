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

        ;;; FILE PERMISIONS
        O_RDONLY  equ 000h
        O_WRONLY  equ 001h
        O_RDWR	  equ 002h
        O_CREAT	  equ 040h
        O_TRUNC	  equ 200h

        ;;; EXIT CODES
        SUCCESS      equ 0
        CREAT_ERROR  equ 1
        WRITE_ERROR  equ 2
        STRLEN_ERROR equ 3
        UNKNOWN_WORD equ 4

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

macro codegen size {
        mov rbx, size
        call output_scratch
}

macro tail_codegen size {
        mov rbx, size
        jmp output_scratch
}

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
        exit SUCCESS
.failed_open:
        write STDERR, failed_open_msg, failed_open_msg_len
        exit CREAT_ERROR
.failed_write: 
        write STDERR, failed_write_msg, failed_write_msg_len
        exit WRITE_ERROR
.failed_string_len_limit:
        write STDERR, failed_string_len_limit_msg, failed_string_len_limit_msg_len
        exit STRLEN_ERROR
        

; Advance the `input_idx` until a `white_space` character is encountered
; writting any non-`whitespace` char in `string`
;
; ### pre
; destroys:
;   - rax, rbx, rdi, rsi
;
; ### post
;   - rdi = pointer to the string
;   - rsi = length of the token found
; destroys:
;   - rax, rbx, rcx, rdi, rsi
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
        mov rsi, 0
.read_char:
; read char from `input`
        mov rdi, input
        mov rsi, [input_idx]
        movzx rbx, byte [rdi + rsi]
; advance index
        inc rsi
        mov [input_idx], rsi
; check if EOF
        cmp [input_idx], dword input_len
        jge .eof
; check for `whitespaces`
        cmp rbx, " "; is char an ASCII 'control' char + space, aka less than space=32
        jle .whitespace
        ; TODO: skip comments
; char is valid
; write char to `string`
        mov rdi, string
        mov rsi, [string_idx]
        mov [rdi + rsi], rbx
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
        mov rax, 0
        jmp .return
.eof:
        mov rax, 1
.return: 
        mov rdi, string
        mov rsi, [string_idx]
        ret

;
; ### pre
; expects: 
;   - rdi = pointer to the string
;   - rsi = length of the token found
;
; ### post
; destroys:
;   - rax, rbx, rcx, rdi, rsi
compile:

; number
; we parse the number from right to left
.try_number:
        mov rbx, 0 ; current number
        mov rcx, 1 ; current multiplier
        ; TODO: check for other bases other than 10 
.number_loop:
        movzx rax, byte [rdi + rsi - 1]
.check_sign:
        cmp rsi, 1 ; last character?
        jne .no_sign
        cmp rax, "-"
        jne .check_plus
        neg rbx
        jmp .number_found
.check_plus:
        cmp rax, "+"
        je .number_found
.no_sign:
; check if number is in base 16, aka hex number
        cmp byte [base], 16
        jne .not_hex
; check if 'a' <= char <= `f`, we parse the char as a hex
        cmp rax, 'a'
        jl .not_hex
        cmp rax, 'f'
        jg .not_hex
        sub rax, 'a' - 10 ; we get the char offset, 'a' => 0, .., 'f' => 6
        jmp .hex
.not_hex:
; check if '0' <= char <= `9`, otherwise we have an unparsable character
        cmp rax, '0'
        jl .number_not_found
        cmp rax, '9'
        jg .number_not_found
        sub rax, '0' ; we get the number offset, '0' => 0, .., '9' => 9
.hex:
; the digit should be in rax
        mul rcx                ; rax * rcx => digit * multiplier
        add rbx, rax           ; add digit to current number
        mov rax, rcx           ; move digit to rax
        movzx rcx, byte [base] ; overwrite rax with the base
        mul rcx                ; rax * rcx => multiplier * base
        mov rcx, rax
        dec edx
        jz .number_found      ; end of token
        jmp .number_loop
.number_found:
; generate code in `scratch` buffer
        mov byte [scratch+0], 68h ; push dword instruction (1)
        mov [scratch+1], rbx      ; the number (4)
        tail_codegen 5            ; super important the tail call to return to the parent function
.number_not_found:
        exit UNKNOWN_WORD

        write [output_fd], string, [string_idx]
        write [output_fd], newline, 1
        ret

;; output the contents of `scratch`
; ### pre
; expects: 
;   - rbx = size of `scratch`
; 
; ### post
; destroys:
;   - rax, rbx
output_scratch:
        write [output_fd], scratch, rbx
        cmp rax, 0
        jge .success
; write error message
        write STDERR, failed_write_scratch_msg, failed_write_scratch_msg_len
        write STDERR, grave_accent, 1
        write STDERR, scratch, rbx
        write STDERR, grave_accent, 1
        write STDERR, newline, 1
.success:
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

scratch: times 128 db 0

; string `constants`
newline: db 10
grave_accent: db 96

; others
base: db 16

;;; FAIL MESSAGES
failed_open_msg: db "Failed to open file", 10, 0
failed_open_msg_len = $ - failed_open_msg

failed_write_msg: db "Failed to write file", 10, 0
failed_write_msg_len = $ - failed_write_msg

failed_string_len_limit_msg: db "Token exceeded string limit of 32", 10, 0
failed_string_len_limit_msg_len = $ - failed_string_len_limit_msg

failed_write_scratch_msg: db "Failed to write `scratch` buffer to file", 10, 0
failed_write_scratch_msg_len = $ - failed_write_scratch_msg


