: exit ( x -- ) 60 syscall1 ;

: result
   34 35 +
;

: threes
   3 3 3
;

: main
   threes * *
   result +
   exit
;
