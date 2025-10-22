: false 0 ;
: true 1 ;

: cell 8 ;
: cells cell * ;

: cp mem ;
: dp mem cell + ;

: here cp @ ;
: dict dp @ ;

: x mem 2 cells + ;             \ first stack variable
: y mem 3 cells + ;             \ second stack variable
: z mem 4 cells + ;             \ third stack variable

: dup ps@ @ ;                   \ get address of top of stack, push the value at address
: swap x ! y ! x @ y @ ;        \ get addres of 1st + 2nd cell, store in mem, reverse order
: drop ps@ cell + ps! ;         \ get address of top of stack, get second value, override the current stack

: main ( -- ) 0 ;
