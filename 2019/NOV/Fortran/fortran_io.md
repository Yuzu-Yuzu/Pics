<!-- TOC -->

- [Console I/O](#console-io)
    - [Function READ*](#function-read)
    - [Function PRINT*](#function-print)
- [File I/O](#file-io)
    - [Function OPEN & READ](#function-open--read)
    - [Function WRITE](#function-write)

<!-- /TOC -->
# Console I/O
## Function READ*
```fortran
! Behaviour similar to cin
READ*, A
READ*, B, C
READ*, D
```
## Function PRINT*
```fortran
! Behaviour similar to cout
PRINT*, "The square root of", 2, 'is', SQRT( 2.0 )
```
# File I/O
## Function OPEN & READ
```fortran
OPEN( 1, FILE = 'DATA' )        ! connects number 1 to the file 'DATA'
READ(1, *) A, B, C              ! reads from file 'DATA' 
                                ! no comma after ')'
PRINT*, A, B, C
END
```
## Function WRITE
```fortran
OPEN( 2, FILE = 'prn' )
WRITE(2, *) 'This is in the file'
PRINT*, 'This is on the screen'
END    
```

