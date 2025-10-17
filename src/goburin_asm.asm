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
        SYS_LSEEK equ 8
        SYS_MMAP  equ 9
        SYS_EXIT  equ 60

        ;;; FILE PERMISIONS
        O_RDONLY  equ 000h
        O_WRONLY  equ 001h
        O_RDWR	  equ 002h
        O_CREAT	  equ 040h
        O_TRUNC	  equ 200h

        ;;; EXIT CODES
        SUCCESS            equ 0
        CREAT_ERROR        equ 1
        WRITE_ERROR        equ 2
        SEEK_ERROR         equ 3
        LSEEK_ERROR        equ 4
        STRLEN_ERROR       equ 5
        UNKNOWN_WORD_ERROR equ 6
        NOMAIN_ERROR       equ 7

        ;;; OTHERS
        ENTRY_ADDR  equ 0x00_00_40_00_00
        DATA_ADDR   equ 0x00_00_60_00_00
;;; macros

macro syscall1 value, arg1 {
        mov rdi, arg1
        mov rax, value
        syscall        
}

macro syscall3 value, arg1, arg2, arg3 {
        mov rdx, arg3 
        mov rsi, arg2 
        mov rdi, arg1
        mov rax, value
        syscall  
}

macro write fd, buf, len { syscall3 SYS_WRITE, fd, buf, len }
macro open fd, flags, mode { syscall3 SYS_OPEN, fd, flags, mode }
macro lseek fd, offset, whence { syscall3 SYS_LSEEK, fd, offset, whence }
macro exit value { syscall1 SYS_EXIT, value }

macro codegen size {
        mov rbx, size
        call output_scratch
}

macro tail_codegen size {
        mov rbx, size
        jmp output_scratch
}

macro debug_token token, token_len {
if 1
        push rdi
        push rsi
        push rbx
        push rcx
        mov rbx, token
        mov rcx, token_len
        write STDOUT, rbx, rcx
        write STDOUT, newline, 1
        pop rcx
        pop rbx
        pop rsi
        pop rdi
else 
end if
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
        test rax, rax
        jz .failed_open
        mov [output_fd], rax

; write elf header
; we need to add padding because we hardcoded that our `starting address` is in C0h
; we should be now at B0h, so we just need to add 16 bytes
        write [output_fd], elf_header, elf_header_len + program_header_len + data_header_len + 16
        test rax, rax
        jz .failed_write
; TODO: entry_point patching
; compilation loop
.loop:
        call read_token
        cmp rax, 1 ; 1 is the return when EOF is read
        jne .compile_token
        mov rax, [string_idx]
        test rax, rax
        jz .epilogue; only exit if `string_idx` == 0
.compile_token:
        call compile
        jmp .loop
; end loop
.epilogue:

.patch_header:

; seek 'filesz' in `output`
        mov rbx, offset_filesz 
        lseek [output_fd], rbx, 0 
        test rax, rax
        jz .failed_lseek

; patch `filesz`
        mov rax, [current_offset]
        add rax, elf_header_len + program_header_len + data_header_len + 16 - 1
        mov [scratch], qword rax
        codegen 8                     ; write to `filesz` field
        codegen 8                     ; write the same value to `memsz` field since the fd head moved
        sub qword [current_offset], 8 ; the two "codegen 4" calls have increased `current_offset`
                                      ; by 8 but did not generate any code

; seek 'entry' in `output`
        mov rbx, offset_entry 
        lseek [output_fd], rbx, 0 
        test rax, rax
        jz .failed_lseek

; patch `entry`
        mov rax, 0x4000C0
        mov qword [scratch], 0x4000C0
        codegen 8

        exit SUCCESS
.failed_open:
        write STDERR, failed_open_msg, failed_open_msg_len
        exit CREAT_ERROR
.failed_write: 
        write STDERR, failed_write_msg, failed_write_msg_len
        exit WRITE_ERROR
.failed_lseek:
        write STDERR, failed_lseek_msg, failed_lseek_msg_len
        exit LSEEK_ERROR
.failed_string_len_limit:
        write STDERR, failed_string_len_limit_msg, failed_string_len_limit_msg_len
        exit STRLEN_ERROR
.failed_finding_main:
        write STDERR, failed_finding_main_msg, failed_finding_main_msg_len
        exit NOMAIN_ERROR
        

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
public compile
compile:
        debug_token string, [string_idx]

.try_builtins:
; rbx <- current builtin index
        mov rbx, 0
.builtins_loop:
        mov al, byte [builtins + rbx]         ; builtins[curr_word] == ...
        cmp al, 0                             ; end of builtins?
        je .try_number
; rcx <- current index for word comparison
        mov rcx, 0
.builtins_token_loop:
; al <- current byte to be compared with read token
        mov al, byte [builtins + rbx + rcx] ; builtins[curr_word][char] == ...
        cmp al, byte [rdi + rcx]              ; ... token[char]
        jne .builtins_not_found
; the first char matches the builtin word
        inc rcx
; check if char index is equal to token index (rsi)
        cmp rcx, rsi
        je .builtins_found
; this means the token is exactly the builtin word we want
; NOTE: builtin words cannot have substrings, so they need to be unique!
        jmp .builtins_token_loop
        
.builtins_not_found:
        add rbx, 16                     ; 8 bytes for the word + 8 bytes for the address
        jmp .builtins_loop

.builtins_found:
        mov rax, [builtins + rbx + 8]   ; we want to get the address of the builtin word
        jmp qword rax

; number
; we parse the number from right to left
.try_number:
        mov rbx, 0 ; current number
        mov rcx, 1 ; current multiplier
; check for an `h` at the right most character
        mov byte [base], 10
        movzx rax, byte [rdi + rsi - 1]
        cmp rax, 'h' ; number is not in hex
        jne .number_loop
        mov byte [base], 16
        dec rsi
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
        dec rsi
        jz .number_found      ; end of token
        jmp .number_loop
.number_found:
; generate code in `scratch` buffer
        mov byte [scratch+0], 68h  ; push dword instruction (1)
        mov [scratch+1], qword rbx ; the number (4)
        tail_codegen 5             ; (1 + 4) super important the tail call to return to the parent function

.number_not_found:
        exit UNKNOWN_WORD_ERROR

;; output the contents of `scratch`
; ### pre
; expects: 
;   - rbx = size of `scratch` to write
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
        write STDERR, quote, 1
        write STDERR, scratch, rbx
        write STDERR, quote, 1
        write STDERR, newline, 1
.success:
        add qword [current_offset], rbx
        ret

;;; COMPILE BUILTINS

;;;
compile_plus:
        mov byte [scratch+0], 0x5b   ; pop rbx
        mov byte [scratch+1], 0x58   ; pop rax
        mov byte [scratch+2], 0x48   ; add ...
        mov word [scratch+3], 0xd801 ; ... rax, rbx
        mov byte [scratch+5], 0x50   ; push rax
        tail_codegen 6

compile_minus:
        mov byte [scratch+0], 0x5b   ; pop rbx
        mov byte [scratch+1], 0x58   ; pop rax
        mov byte [scratch+2], 0x48   ; sub ...
        mov word [scratch+3], 0xd829 ; ... rax, rbx
        mov byte [scratch+5], 0x50   ; push rax
        tail_codegen 6

compile_multiply:
        mov byte [scratch+0], 0x58   ; pop rax
        mov byte [scratch+1], 0x5a   ; pop rdx
        mov byte [scratch+2], 0x48   ; mul ...
        mov word [scratch+3], 0xe2f7 ; ... rdx (into rax)
        mov byte [scratch+5], 0x50   ; push rax
        tail_codegen 6

compile_divide:
        mov byte [scratch+0], 0x48   ; xor ...
        mov word [scratch+1], 0xd231 ; ... rdx,rdx
        mov byte [scratch+3], 0x5b   ; pop    rbx
        mov byte [scratch+4], 0x58   ; pop    rax
        mov byte [scratch+5], 0x48   ; div ...
        mov word [scratch+6], 0xf3f7 ; ... rbx
        mov byte [scratch+8], 0x50   ; push   rax
        tail_codegen 9

compile_ret:
        mov byte [scratch+0], 0xC3   ; ret
        tail_codegen 1

compile_exit:
        ; 58 48 89 c7 48 c7 c0 3c 00 00 00 0f 05 c3
        mov byte [scratch+0],  0x58           ; pop rax
        mov word [scratch+1],  0x8948         ; ... ??
        mov word [scratch+3],  0x48C7         ; mov rdi, rax
        mov dword [scratch+5], 0x003cC0C7     ; mov rax, SYS_EXIT
        mov word [scratch+9],  0x0000         ; ... padding
        mov word [scratch+11], 0x050F         ; syscall        
        mov byte [scratch+13], 0xC3           ; ret
        tail_codegen 14

;;; DATA SECTION
;;; -- WRITABLE
section '.data' writable

input_idx: dq 0
output_fd: dq 0

string: rb 32
string_len = $ - string
string_idx: dq 0

scratch: rb 128 
current_offset: dq 0
base: db 16

;;; -- READ ONLY
section '.rodata'

input: file "src/goburin_forth.forth"
input_len = $ - input

output: db "build/goburin_forth", 0

; string `constants`
newline: db 10
quote: db 39

;;; -- FAIL MESSAGES
failed_open_msg: db "Failed to open file", 10, 0
failed_open_msg_len = $ - failed_open_msg

failed_write_msg: db "Failed to write file", 10, 0
failed_write_msg_len = $ - failed_write_msg

failed_string_len_limit_msg: db "Token exceeded string limit of 32", 10, 0
failed_string_len_limit_msg_len = $ - failed_string_len_limit_msg

failed_write_scratch_msg: db "Failed to write `scratch` buffer to file", 10, 0
failed_write_scratch_msg_len = $ - failed_write_scratch_msg

failed_lseek_msg: db "Failed to seek in ELF64 header", 10, 0
failed_lseek_msg_len = $ - failed_lseek_msg

failed_finding_main_msg: db "Failed to find `main` function", 10, 0
failed_finding_main_msg_len = $ - failed_finding_main_msg

; w => 2
; d => 4
; q => 8
;;; ELF64 Header
elf_header:
        db 7fh, 45h, 4ch, 46h, 02h, 01h, 01h, 00h ; e_ident     => Magic number and other info
        db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h
        dw 02h                                    ; (w) e_type      => Object file type 
        dw 3eh                                    ; (w) e_machine   => Architecture 
        dd 01h                                    ; (d) e_version   => Object file version 
        dq 00h                                    ; (q) e_entry     => Entry point virtual address       -- TO BE PATCHED
        ;  ^-- (24 bytes offset)
        dq program_header - elf_header            ; (q) e_phoff     => Program header table file offset 
        dq 00h                                    ; (q) e_shoff     => Section header table file offset 
        dd 00h                                    ; (d) e_flags     => Processor-specific flags 
        dw elf_header_len                         ; (w) e_ehsize    => ELF header size in bytes 
        dw program_header_len                     ; (w) e_phentsize => Program header table entry size 
        dw 02h                                    ; (w) e_phnum     => Program header table entry count (2 entries) 
        dw 00h                                    ; (w) e_shentsize => Section header table entry size 
        dw 00h                                    ; (w) e_shnum     => Section header table entry count 
        dw 00h                                    ; (w) e_shstrndx  => Section header string table index 
elf_header_len = $ - elf_header

program_header:
        dd 01h                                    ; (d) p_type      => Segment type (PT_LOAD)
        dd 05h                                    ; (d) p_flags     => Segment flags (READ + EXEC)
        dq 00h                                    ; (q) p_offset    => Segment file offset
        dq ENTRY_ADDR                             ; (q) p_vaddr     => Segment virtual address
        dq 00h                                    ; (q) p_paddr     => Segment physical address
        dq 00h                                    ; (q) p_filesz    => Segment size in file              -- TO BE PATCHED
        ;  ^-- (32 bytes offset)
        dq 00h                                    ; (q) p_memsz     => Segment size in memory            -- TO BE PATCHED
        ;  ^-- (40 bytes offset)
        dq 0x1000                                 ; (q) p_align     => Segment alignment
program_header_len = $ - program_header

data_header:
        dd 01h                                    ; (d) p_type      => Segment type (PT_LOAD)
        dd 04h                                    ; (d) p_flags     => Segment flags (READ)
        dq 00h                                    ; (q) p_offset    => Segment file offset
        dq DATA_ADDR                              ; (q) p_vaddr     => Segment virtual address
        dq 00h                                    ; (q) p_paddr     => Segment physical address
        dq 0100h                                  ; (q) p_filesz    => Segment size in file              -- TO BE PATCHED
        ;  ^-- (32 bytes offset)
        dq 0100h                                  ; (q) p_memsz     => Segment size in memory            -- TO BE PATCHED
        ;  ^-- (40 bytes offset)
        dq 0x1000                                 ; (q) p_align     => Segment alignment
data_header_len = $ - data_header

; we need to padd by 16 bytes because we hardcoded that our `starting address`
padding:
        db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h
        db 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h

offset_entry  = 24
offset_filesz = 32 + elf_header_len

;;; -- BUILTINS
; every builtin consists of:
; - 64 bits for the "keyword"
; - 64 bits to the routine that compiles it
; 
; NOTE: builtin words cannot have substrings, so they need to be unique!
; Cannot have two words, one being a substring of another like "str" and "string"!
builtins:
; size 1
        dq "+"
        dq compile_plus
        dq "-"
        dq compile_minus
        dq "*"
        dq compile_multiply
        dq "/"
        dq compile_divide
; size 3
        dq "ret" 
        dq compile_ret
; size 4
        dq "exit" 
        dq compile_exit

; end of built-ins
        db 0

