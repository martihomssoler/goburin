( CONSTANTS )
   \ SYSCALL NUMBERS
   : SYS_READ   0 ;
   : SYS_WRITE  1 ;
   : SYS_OPEN   2 ;
   : SYS_CLOSE  3 ;
   : SYS_LSEEK  8 ;
   : SYS_MMAP   9 ;
   : SYS_EXIT  60 ;
   
   \ EXIT CODES
   : SUCCESS            0 ; 
   : CREATE_ERROR       1 ; 
   : READ_ERROR         2 ; 
   : WRITE_ERROR        3 ; 
   : SEEK_ERROR         4 ; 
   : LSEEK_ERROR        5 ; 
   : STRLEN_ERROR       6 ; 
   : UNKNOWN_WORD_ERROR 7 ; 
   : NOMAIN_ERROR       8 ; 

   \ OTHERS
   : FALSE 0 ;
   : TRUE 1 ;
   : CELL 8 ;

( )

( HELPERS )
   : cells CELL * ;
   
   : cp mem ;
   : dp mem CELL + ;
   : here cp @ ;
   : dict dp @ ;
   
   : x mem 2 cells + ;             \ first stack variable
   : y mem 3 cells + ;             \ second stack variable
   : z mem 4 cells + ;             \ third stack variable
   
   : dup ps@ @ ;                   \ get address of top of stack, push the value at address
   : swap x ! y ! x @ y @ ;        \ get addres of 1st + 2nd cell, store in mem, reverse order
   : drop ps@ CELL + ps! ;         \ get address of top of stack, get second value, override the current stack
   
   : over ( x y -- x y x ) x ! y ! x @ y @ x @ ;
   : tuck ( x y -- y x y ) x ! y ! y @ x @ y @ ;
   : rot ( x y z -- y z x ) x ! y ! z ! y @ z @ x @ ;
   : -rot ( x y z -- z x y ) x ! y ! z ! z @ x @ y @ ;
   : 2dup ( x y -- x y x y ) x ! y ! x @ y @ x @ y @ ;
( )

( DATA )
   : current_offset mem 5 cells + ;
   : output_fd mem 6 cells + ;
   : output_buf mem 7 cells + ;
   : str_ptr mem 8 cells + ;
( )


: str_buf str_ptr CELL + ;

: exit ( x -- ) SYS_EXIT syscall1 ;
: write ( fd ptr len -- err? )
	SYS_WRITE syscall3 dup <0 WRITE_ERROR swap ?exit drop
;

: open ( fd flags permisions -- err? )
	SYS_OPEN syscall3 dup <0 CREATE_ERROR swap ?exit drop
;

\ writes the byte to the output_buf and then to the output_fd 
: write_byte  ( byte -- ) output_buf ! output_fd @ output_buf 1 write ;
\ emit the leat significant byte of n
: emit1 ( n -- ) write_byte current_offset @ 1 + current_offset ! ;

: output_open ( -- )
   \ no need to set str_buf length since we need a null terminated str_buf here
   'b' str_buf !      'u' str_buf 1  + ! 'i' str_buf 2  + ! 'l' str_buf 3  + ! 'd' str_buf 4  + !
   '/' str_buf 5  + ! 'g' str_buf 6  + ! 'o' str_buf 7  + ! 'b' str_buf 8  + ! 'u' str_buf 9  + !
   'r' str_buf 10 + ! 'i' str_buf 11 + ! 'n' str_buf 12 + ! '_' str_buf 13 + ! 'f' str_buf 14 + !
   'o' str_buf 15 + ! 'r' str_buf 16 + ! 't' str_buf 17 + ! 'h' str_buf 18 + ! '2' str_buf 19 + !

   0  str_buf 20 + !
   
   \ 241h => (O_WRONLY | O_CREAT | O_TRUNC) flags
   \ 1ffh => 777 permisions (bc they are in octal...)
   str_buf 241h 1ffh open 
   output_fd !
;

: init ( -- )
   \ clear the dictionary
   0 dp !
   \ reset the current_offset
   0 current_offset !
   \ set the "here" pointer (aka start of memory) after the string buffer
   str_buf 32 cells + cp !
   \ open output file and save the fd
   output_open
;

: write_headers ( -- )
\ elf_header
   \ magic number => 7fh, 45h, 4ch, 46h
   7fh emit1 45h emit1 4ch emit1 46h emit1
   \ e_indent => 02h, 01h, 01h, 00h 
   02h emit1 01h emit1 01h emit1 00h emit1
   \ padding => 00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h     
   00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1

   \  e_type (w => 2 bytes)
   02h emit1 00h emit1
   \  e_machine (w => 2 bytes)
   3eh emit1 00h emit1
   \  e_version (d => 4 bytes)
   01h emit1 00h emit1 00h emit1 00h emit1
   \  e_entry (q => 8 bytes)                                  <<== TO BE PATCHED   
   00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1

   \  e_phoff (q => 8 bytes) can be harcoded
	40h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1
   \  e_shoff (q => 8 bytes)
   00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1
   \  e_flags (d => 4 bytes)
   00h emit1 00h emit1 00h emit1 00h emit1
   \  e_ehsize (w => 2 bytes)  can be harcoded
   40h emit1 00h emit1
   \  e_phentsize (w => 2 bytes) can be harcoded
   38h emit1 00h emit1
   \  e_phnum 2 entries (w => 2 bytes)
   02h emit1 00h emit1
   \  e_shentsize (w => 2 bytes)
   00h emit1 00h emit1
   \  e_shnum (w => 2 bytes)
   00h emit1 00h emit1
   \  e_shstrndx (w => 2 bytes)
   00h emit1 00h emit1
\ program_header
   \ p_type (d => 4 bytes)   
   01h emit1 00h emit1 00h emit1 00h emit1
   \ p_flags (d => 4 bytes)  
   05h emit1 00h emit1 00h emit1 00h emit1
   \ p_offset (q => 8 bytes)  
   00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1
   \ p_vaddr (q => 8 bytes) ENTRY_ADDR 400000h
   00h emit1 00h emit1 40h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1
   \ p_paddr (q => 8 bytes)  
   00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1
   \ p_filesz (q => 8 bytes)                                  <<== TO BE PATCHED   
   00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1
   \ p_memsz (q => 8 bytes)                                   <<== TO BE PATCHED   
   00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1
   \ p_align (q => 8 bytes) 0x1000
   00h emit1 10h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1 00h emit1

   \ reset current_offset, because the headers are not part of the code "offset"
   0 current_offset !
;

: main ( -- ) init write_headers 0 exit ;
