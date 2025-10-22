format ELF64

;;; constants

        ;;; FILE DESCRIPTORS
        STDIN     equ 0
        STDOUT    equ 1
        STDERR    equ 2

        ;;; SYSCALL NUMBERS
        SYS_READ  equ 0
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
        READ_ERROR         equ 2
        WRITE_ERROR        equ 3
        SEEK_ERROR         equ 4
        LSEEK_ERROR        equ 5
        STRLEN_ERROR       equ 6
        UNKNOWN_WORD_ERROR equ 7
        NOMAIN_ERROR       equ 8

        ;;; OTHERS
        ENTRY_ADDR  equ 0x00_00_40_00_00
        DATA_ADDR   equ 0x00_00_60_00_00

        MAX_DICT_ENTRIES equ 255
        MAX_TOKEN_SIZE   equ 32
        DATA_STACK_LEN   equ 2048
        MEMORY_BLOCK_LEN equ 8192

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

macro read fd, buf, count { syscall3 SYS_READ, fd, buf, count }
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
if 0
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
public compile_token
public read_token
public read_stdin
public add_dict
public find_dict

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
; compilation loop
.loop:
        call read_token
        cmp rax, 1 ; 1 is returned when we read EOF 
        jne .compile
        mov rax, [string_idx]
        test rax, rax
        jz .epilogue; only exit if `string_idx` == 0
.compile:
        call compile_token
        jmp .loop
; end loop
.epilogue:
; we try to find the main function and back-patch it
        mov rdi, main_name
        mov rsi, main_name_len
        call find_dict
        cmp rdx, -1
        jne .main_found
        jmp .failed_finding_main
.main_found:
        sub rdx, [current_offset]
        sub rdx, 5                          ; jmp instruction is 5 bytes 
        push rdx                            ; save rdx
        mov byte  [scratch + 0], 0xe9       ; jmp ...
        mov dword [scratch + 1], edx        ; ... addresses are 8 bytes but jumping w/ 4 byte offset
        codegen 5

.patch_header:
; seek 'filesz' in `output`
        mov rbx, offset_filesz 
        lseek [output_fd], rbx, 0 
        test rax, rax
        jz .failed_lseek

; patch `filesz`
        mov rax, [current_offset]
        add rax, elf_header_len + program_header_len + data_header_len + 16
        mov [scratch], qword rax
        codegen 8                      ; write to `filesz` field
        codegen 8                      ; write the same value to `memsz` field since the fd head moved
        sub byte [current_offset], 16  ; the two "codegen 8" calls have increased `current_offset`
                                       ; by 16 but did not generate any code

; seek 'entry' in `output`
        mov rbx, offset_entry 
        lseek [output_fd], rbx, 0 
        test rax, rax
        jz .failed_lseek

; patch `entry`
        pop rdx                        ; restore rdx
        mov rax, 0x400000
        add rax, elf_header_len + program_header_len + data_header_len + 16
        add rax, [current_offset]
        add rax, rdx
        mov qword [scratch], rax
        codegen 8

        exit SUCCESS
.failed_open:
        write STDERR, failed_open_msg, failed_open_msg_len
        exit CREAT_ERROR
.failed_read:
        write STDERR, failed_read_msg, failed_read_msg_len
        exit READ_ERROR
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
        

; Advance the `string_idx` until a `white_space` character is encountered
; writting any non-`whitespace` char in `string`
;
; ### post
;   - rdi = pointer to the string
;   - rsi = length of the token found
; destroys:
;   - rax, rbx, rcx, rdi, rsi
;
read_token:
; reset `string_idx`
        mov [string_idx], dword 0
        mov rsi, 0
.read_char:
        call read_stdin       ; rbx contains the byte
        jz .eof               ; equal 0, means EOF
; advance index
        inc rsi
; check for `whitespaces`
        cmp rbx, 0x20; is char an ASCII 'control' char + space, aka less than 0x20 => 32
        jle .whitespace
; check for '\' comment
        cmp rbx, "\"
        je .comment
        cmp rbx, "("
        je .parenthesis
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
.comment:
; keep advancing the index until we get EOF or a newline
        call read_stdin
        jz .eof
; check if newline
        cmp rbx, 10 ; newline value is 10
        je .continue
        jmp .comment
.parenthesis:
; keep advancing the index until we get EOF or a ")"
        call read_stdin
        jz .eof
; check if ")"
        cmp rbx, ")" ; newline value is 10
        je .continue
        jmp .parenthesis
.continue:
; we jump back to the beginning of this function, resetting the string
        jmp read_token
.eof:
        mov rax, 1
.return: 
        mov rdi, string
        mov rsi, [string_idx]
        ret

; get a character from stdin.
; returns ascii code in rbx. sets carry on eof.
;
; ### post
; destroys:
;   - rax, rbx, rdi, rsi
read_stdin:
; read char from STDIN
        read STDIN, input_buf, 1
        test rax, rax
        jb _start.failed_read  ; below 0, means error
        jnz .get_byte          ; 0 means eof
        stc                    ; set carry flag
        ret
.get_byte:
        movzx rbx, byte [input_buf]
        clc                    ; clear carry flag
        ret

; ### pre
; expects: 
;   - rdi = pointer to the string
;   - rsi = length of the token found
;
; ### post
; destroys:
;   - rax, rbx, rcx, rdi, rsi
compile_token:
        debug_token string, [string_idx]
.try_dict:
        call find_dict                        ; rdx = should have address
        cmp rdx, -1                           ; word not found in dictionary
        je .try_builtins
; add prelude for function calling
        mov byte [scratch+0], 0x48            ; xchg ...
        mov word [scratch+1], 0xe587          ; ... rbp, rsp
        mov byte [scratch+3], 0xe8            ; call
        mov rax, [current_offset]
        add rax, 8                            ; 8 is the size of the xchg and call instructions
                                              ; (call will be relative to next instruction)
        sub rdx, rax                          ; this will result in a relative jump to the word code
        mov dword [scratch+4], edx            ; sub-routine address
        mov byte [scratch+8], 0x48           ; xchg ...
        mov word [scratch+9], 0xe587         ; ... rbp, rsp
        tail_codegen 11
.try_builtins:
; rbx <- current builtin index
        xor rax, rax
        mov rbx, 0
.builtins_loop:
        mov al, byte [builtins + rbx]         ; builtins[curr_word] == ...
        cmp al, 0                             ; end of builtins?
        je .try_char_literals
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
        jmp .builtins_token_loop
        
.builtins_not_found:
        add rbx, 16                     ; 8 bytes for the word + 8 bytes for the address
        jmp .builtins_loop

.builtins_found:
        mov rax, [builtins + rbx + 8]   ; we want to get the address of the builtin word
        jmp qword rax

.try_char_literals:
        mov rcx, 3      ; size of a char literal 
        cmp rcx, rsi
        jne .try_number
        cmp byte [rdi], "'"
        jne .try_number
        cmp byte [rdi+2], "'"
        jne .try_number
        movzx rbx, byte [rdi+1]
        jmp .number_found

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

; find a dictionary entry
;
; ### pre
; expects:
;   - rdi = pointer to the token name
;   - rsi = length of the token
;  
; ### post
; destroys:
;   - rax, rcx, rdx
;
; returns:
;   - rdx = address of the entry found or -1 if not found
find_dict:
        push rsi                    ; save token length
        mov rdx, dict
        mov rcx, [dict_len]
.dict_loop:
        pop rsi                     ; we need to do this pop/push dance to restore ...
        push rsi                    ; ... the token length
        test rcx, rcx
        jz .not_found               ; we decrement `rcx`, if 0 then not found
; compare size
        movzx rbx, byte [rdx]       ; first data in an entry is the length
        cmp rbx, rsi
        jne .not_equal
; compare string
.cmp_string_loop:
        test rsi, rsi
        jz .found
        mov bl, byte [rdi + rsi - 1]
        cmp bl, byte [rdx + 1 + rsi - 1] ; (rdx + 1) is the dict entry name
        jne .not_equal
        dec rsi
        jmp .cmp_string_loop
.not_equal:
        add rdx, dict_entry_len    ; save current entry
        dec rcx
        jmp .dict_loop
.not_found:
        pop rsi                    ; restore token length
        mov rdx, -1
        ret
.found:
        pop rsi                    ; restore token length
        inc rdx                    ; step over the token len (1 byte)
        add rdx, MAX_TOKEN_SIZE    ; step over the token name (MAX_TOKEN_SIZE bytes)
        mov rdx, [rdx]             ; we want the content, aka the pointer value
        ret

; add an entry to the dictionary
;
; ### pre
; expects:
;   - rdx = address of the token in memory
;   - rdi = pointer to the token name
;   - rsi = length of the token
;  
; ### post
; destroys:
;   - rax, rbx, rcx, rdx, rdi, rsi
add_dict:
        push rsi
        push rdi
        push rdx
; change some registers so we can use `rep movsb` and others later on
        mov rcx, rsi             ; rci should contain token length
        mov rsi, rdi             ; rsi should contain token ptr
; 
        lea rdi, [dict]          ; rdi should contain the dict addr
        mov rax, [dict_len]
        mov rbx, dict_entry_len
        mul rbx                  ; rax = offset of new token multiple of `dict_entry_len`
        add rdi, rax             ; rdi = points to an empty entry, to be filled with the `token` info
; write token name_len 
        mov [rdi + 0], cl
        inc rdi
; write token name
        push rcx
        rep movsb                ; Copy token name, using RSI (ptr) + RCX (len)  for source
        pop rcx
        add rdi, MAX_TOKEN_SIZE
        sub rdi, rcx             ; substract the amount of bytes copied by `rep movsb`
; write code_ptr
        pop rdx
        mov qword [rdi], rdx
; increment the number of dict entries
        inc qword [dict_len]
        pop rdi
        pop rsi
        ret

; output the contents of `scratch`
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

compile_colon:
        call read_token
        debug_token string, [string_idx]
        mov rdx, qword [current_offset]
        call add_dict
; check if token has length 4 and is "main"
        mov rbx, rsi
        cmp rbx, 4
        jne .not_main
        mov bl, byte [rdi + 0]
        cmp bl, byte [main_name + 0]
        jne .not_main
        mov bl, byte [rdi + 1]
        cmp bl, byte [main_name + 1]
        jne .not_main
        mov bl, byte [rdi + 2]
        cmp bl, byte [main_name + 2]
        jne .not_main
        mov bl, byte [rdi + 3]
        cmp bl, byte [main_name + 3]
        jne .not_main
        jmp .is_main
.not_main:
; swap rbp <=> rsp, this way the stack pointer points to the start of the `data` stack
; and we are prepared to execute code and push data into it
        mov byte [scratch+0], 0x48   ; xchg ...
        mov word [scratch+1], 0xe587 ; ... rbp, rsp (function preamble)
        tail_codegen 3
.is_main:
; instead of the usual `function` preamble, we need to create the `data` stack and the
; memory stack
; NOTE: data stack
        mov rax, DATA_STACK_LEN
        call mmap_codegen
; lea rbp, [rax+0x800] => 48 8d a8 00 08 00 00
        mov byte  [scratch+0], 0x48
        mov word  [scratch+1], 0xa88d
        mov dword [scratch+3], DATA_STACK_LEN
        codegen 7
; NOTE: memory block hold in `r15`
        mov byte  [scratch+0], 0x55    ; push rbp
        codegen 1
        mov rax, MEMORY_BLOCK_LEN
        call mmap_codegen

        mov byte  [scratch+0], 0x49    ; mov ...
        mov word  [scratch+1], 0xc789  ; ... r15, rax
        codegen 3

        mov byte  [scratch+0], 0x5d    ; pop rbp
        codegen 1
        ret

mmap_codegen:
        push rax ; save rax because it gets destroyed by `codegen`
        ; SYS_MMAP
        ; mov    rax,0x9
        ; 48 c7 c0 09 00 00 00
        mov byte  [scratch+0], 0x48
        mov word  [scratch+1], 0xc0c7
        mov dword [scratch+3], SYS_MMAP
        codegen 7

        ; Address
        ; xor    rdi,rdi
        ; 48 31 ff
        mov byte  [scratch+0], 0x48
        mov word  [scratch+1], 0xff31
        codegen 3

        pop rax ; restore rax
        ; Size (passed in `rax`)
        ; mov    rsi, `rax at comptime`
        ; 48 c7 c6 ?? ?? ?? ??
        mov byte  [scratch+0], 0x48
        mov word  [scratch+1], 0xc6c7
        mov dword [scratch+3], eax
        codegen 7

        ; Protection => 3 ( PROT_READ | PROT_WRITE )
        ; mov    rdx,0x3
        ; 48 c7 c2 03 00 00 00
        mov byte  [scratch+0], 0x48
        mov word  [scratch+1], 0xc2c7
        mov dword [scratch+3], 3
        codegen 7

        ; Flags => 0x22 ( MAP_PRIVATE | MAP_ANONYMOUS )
        ; mov    r10,0x22
        ; 49 c7 c2 22 00 00 00
        mov byte  [scratch+0], 0x49
        mov word  [scratch+1], 0xc2c7
        mov dword [scratch+3], 0x22
        codegen 7

        ; fd => -1 (anonymous)
        ; mov    r8,0xffffffffffffffff
        ; 49 c7 c0 ff ff ff ff
        mov byte  [scratch+0], 0x49
        mov word  [scratch+1], 0xc0c7
        mov dword [scratch+3], -1
        codegen 7

        ; offset => 0
        ; xor    r9,r9
        ; 4d 31 c9
        mov byte  [scratch+0], 0x4d
        mov word  [scratch+1], 0xc931
        codegen 3

        ; syscall
        ; 0f 05
        mov word  [scratch+0], 0x050f
        codegen 2

        ret
        
; size 1
compile_semicolon:
; swap rbp <=> rsp, this way the stack pointer points to the start of the `address` stack
; and we are prepared to jump out of the function
        mov byte [scratch+0], 0x48   ; xchg ...
        mov word [scratch+1], 0xe587 ; ... rbp, rsp (function preamble)
        mov byte [scratch+3], 0xc3   ; ret
        tail_codegen 4
 
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

compile_fetch:
        mov byte  [scratch+0],  0x58     ; pop rax
        mov byte  [scratch+1],  0xff     ; push ...
        mov byte  [scratch+2],  0x30   ; ... [rax]
        tail_codegen 3

compile_store:
        mov byte  [scratch+0], 0x58      ; pop rax
        mov byte  [scratch+1], 0x5b      ; pop rbx
        mov byte  [scratch+2], 0x48      ; mov ...
        mov word  [scratch+3], 0x1889    ; ... [rax], rbx
        tail_codegen 5

; size 2
compile_less_zero:
        mov byte  [scratch+0],  0x58     ; pop rax
        mov byte  [scratch+1],  0x48     ; test ...
        mov word  [scratch+2],  0xc085   ; ... rax, rax
        mov word  [scratch+4],  0x890f   ; jns ...
        mov dword [scratch+6],  7        ; ... +7 (after jmp)
        mov word  [scratch+10], 0x016a   ; push 1 (true)
        mov byte  [scratch+12], 0xe9     ; jmp ...
        mov dword [scratch+13], 2        ; ... +2 (after push 0)
        mov word  [scratch+17], 0x006a   ; push 0 (false)
        tail_codegen 19
        
; size 3
compile_ps_fetch:
        mov byte  [scratch+0],  0x54     ; push rsp
        tail_codegen 1

compile_ps_store:
        mov byte  [scratch+0],  0x5c     ; pop rsp
        tail_codegen 1

compile_rs_fetch:
        mov byte  [scratch+0],  0x55     ; push rbp
        tail_codegen 1

compile_rs_store:
        mov byte  [scratch+0],  0x5d     ; pop rbp
        tail_codegen 1

compile_mem:
        mov word [scratch+0],  0x5741    ; push r15
        tail_codegen 2

; size 4
compile_qret:
        mov byte  [scratch+0], 0x58      ; pop rax
        mov byte  [scratch+1], 0x48      ; test ...
        mov word  [scratch+2], 0xc085    ; ... rax, rax
        mov word  [scratch+4], 0x840f    ; jz ...
        mov dword [scratch+6], 4         ; ... +4 (after ret)
        mov byte  [scratch+10], 0x48     ; xchg ...
        mov word  [scratch+11], 0xe587   ; ... rbp, rsp (function preamble)
        mov byte  [scratch+13], 0xC3     ; ret
        tail_codegen 14

; size 5
compile_qexit:
        mov byte  [scratch+0], 0x58      ; pop rax
        mov byte  [scratch+1], 0x48      ; test ...
        mov word  [scratch+2], 0xc085    ; ... rax, rax
        mov word  [scratch+4], 0x840f    ; jz ...
        mov dword [scratch+6], 14        ; ... +14 (after exit)
; exit call
        mov byte  [scratch+10], 0x58     ; pop rax
        mov byte  [scratch+11], 0x48     ; mov rdi ...
        mov word  [scratch+12], 0xc789   ; ... rax
        mov byte  [scratch+14], 0x48     ; mov ...
        mov word  [scratch+15], 0xc0c7   ; ... rax ...
        mov dword [scratch+17], SYS_EXIT ; ... SYS_EXIT
        mov word  [scratch+21], 0x050f   ; syscall        
        mov byte  [scratch+23], 0xc3     ; ret
        tail_codegen 24

; size 8
compile_syscall1:
        mov byte [scratch+0], 0x58     ; pop rax
        mov byte [scratch+1], 0x5f     ; pop rdi
        mov word [scratch+2], 0x050f   ; syscall
        mov byte [scratch+4], 0x50     ; push rax
        tail_codegen 5

compile_syscall2:
        mov byte [scratch+0], 0x58     ; pop rax
        mov byte [scratch+1], 0x5e     ; pop rsi
        mov byte [scratch+2], 0x5f     ; pop rdi
        mov word [scratch+3], 0x050f   ; syscall
        mov byte [scratch+5], 0x50     ; push rax
        tail_codegen 6

compile_syscall3:
        mov byte [scratch+0], 0x58     ; pop rax
        mov byte [scratch+1], 0x5a     ; pop rdx
        mov byte [scratch+2], 0x5e     ; pop rsi
        mov byte [scratch+3], 0x5f     ; pop rdi
        mov word [scratch+4], 0x050f   ; syscall
        mov byte [scratch+6], 0x50     ; push rax
        tail_codegen 7

compile_syscall4:
        mov byte [scratch+0], 0x58     ; pop rax
        mov word [scratch+1], 0x5a41   ; pop r10
        mov byte [scratch+3], 0x5a     ; pop rdx
        mov byte [scratch+4], 0x5e     ; pop rsi
        mov byte [scratch+5], 0x5f     ; pop rdi
        mov word [scratch+6], 0x050f   ; syscall
        mov byte [scratch+8], 0x50     ; push rax
        tail_codegen 9

compile_syscall5:
        mov byte [scratch+0],  0x58    ; pop rax
        mov word [scratch+1],  0x5841  ; pop r8
        mov word [scratch+3],  0x5a41  ; pop r10
        mov byte [scratch+5],  0x5a    ; pop rdx
        mov byte [scratch+6],  0x5e    ; pop rsi
        mov byte [scratch+7],  0x5f    ; pop rdi
        mov word [scratch+8],  0x050f  ; syscall
        mov byte [scratch+10], 0x50    ; push rax
        tail_codegen 11

compile_syscall6:
        mov byte [scratch+0],  0x58    ; pop rax
        mov word [scratch+1],  0x5941  ; pop r9
        mov word [scratch+3],  0x5841  ; pop r8
        mov word [scratch+5],  0x5a41  ; pop r10
        mov byte [scratch+7],  0x5a    ; pop rdx
        mov byte [scratch+8],  0x5e    ; pop rsi
        mov byte [scratch+9],  0x5f    ; pop rdi
        mov word [scratch+10], 0x050f  ; syscall
        mov byte [scratch+12], 0x50    ; push rax
        tail_codegen 13
 
;;; DATA SECTION
;;; -- WRITABLE
section '.data' writable

input_buf: rb 1
output_fd: dq 0

string: rb MAX_TOKEN_SIZE
string_len = $ - string
string_idx: dq 0

scratch: rb 128 
current_offset: dq 0
base: db 16

dict: rb dict_entry_len * MAX_DICT_ENTRIES
dict_len: rq 1

; only used to calculate the len
; left for documentation purposes
dict_entry:
        name_len rb 1
        name rb MAX_TOKEN_SIZE
        code_ptr rq 1
dict_entry_len = $ - dict_entry

;;; -- READ ONLY
section '.rodata'

output: db "build/goburin_forth", 0
main_name: db "main"
main_name_len = $ - main_name

; string `constants`
newline: db 10
quote: db 39

;;; -- FAIL MESSAGES
failed_open_msg: db "Failed to open file", 10, 0
failed_open_msg_len = $ - failed_open_msg

failed_read_msg: db "Failed to read file", 10, 0
failed_read_msg_len = $ - failed_read_msg

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
builtins:
; size 1
        dq ":"
        dq compile_colon
        dq ";"
        dq compile_semicolon
        dq "+"
        dq compile_plus
        dq "-"
        dq compile_minus
        dq "*"
        dq compile_multiply
        dq "/"
        dq compile_divide
        dq "@"
        dq compile_fetch
        dq "!"
        dq compile_store
; size 2
        dq "<0"
        dq compile_less_zero
; size 3
        dq "ps@"
        dq compile_ps_fetch
        dq "ps!"
        dq compile_ps_store
        dq "rs@"
        dq compile_rs_fetch
        dq "rs!"
        dq compile_rs_store
        dq "mem"
        dq compile_mem
; size 4
        dq "?ret" 
        dq compile_qret
; size 5
        dq "?exit" 
        dq compile_qexit
; size 8
        dq "syscall1"
        dq compile_syscall1
        dq "syscall2"
        dq compile_syscall2
        dq "syscall3"
        dq compile_syscall3
        dq "syscall4"
        dq compile_syscall4
        dq "syscall5"
        dq compile_syscall5
        dq "syscall6"
        dq compile_syscall6

; end of built-ins
        db 0
