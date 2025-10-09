format ELF64 executable

_start:
        mov rax, 60   ; return ...
        mov rdi, 0    ; ... 0
        syscall       
        ret            
