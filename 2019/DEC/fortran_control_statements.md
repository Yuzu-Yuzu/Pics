# Fortran Controll Statements
<!-- TOC -->

- [DO Loop](#do-loop)
- [DO-EXIT Loop](#do-exit-loop)
- [IF-THEN-ELSE Statement](#if-then-else-statement)

<!-- /TOC -->
# DO Loop
```fortran
! general format of DO loop

DO I = J, K, STEP        ! STEP is by default 1
    ! code here
    END DO



! print_alpha.f90
! prints letters from a to z

DO I = 97, 122
    print*, ACHAR( I )
    END DO
    END



! print_alpha_reverse.f90
! prints letters from z to a

DO I = 122, 97, -1
    print*, ACHAR( I )
    END DO
    END
```
# DO-EXIT Loop
```fortran
INTEGER K, N
N = 0
DO
    N = N + 1
    K = SELECTED_INT_KIND( N )
    IF( K == -1 ) EXIT
    PRINT*, N, K
    END DO
END
```
# DO-WHILE Loop
```fortran
DO WHILE (logical-expr)
    block
    END DO
```
# IF-THEN-ELSE Statement
```fortran
IF (condition) THEN
    ! code block 1
ELSE                ! ELSE is optional
    ! code block 2
END IF


! final_mark.f90
! Final mark for course based on class record and exams

PROGRAM Final_Mark
    IMPLICIT NONE
    ! The implicit none statement is used to inhibit a very old feature of Fortran that by default treats all variables that start with the letters i, j, k, l, m and n as integers and all other variables as real arguments. Implicit None should always be used. It prevents potential confusion in variable types, and makes detection of typographic errors easier. 
    
    REAL CRM                ! Class record mark
    REAL ExmAvg             ! average of two exam papers
    REAL Final              ! final mark
    REAL P1                 ! mark for first paper
    REAL P2                 ! mark for second paper
    INTEGER Stu             ! student counter

    OPEN( 1, FILE = 'MARKS' )
    PRINT*, 'CRM Exam Avg Final Mark'
    PRINT*                  ! prints a blank line  
    DO Stu = 1, 3
        READ( 1, * ) CRM, P1, P2
        ExmAvg = (P1 + P2) / 2.0
        IF (ExmAvg > CRM) THEN
            Final = ExmAvg
        ELSE
            Final = (P1 + P2 + CRM) / 3.0
        END IF
        IF (Final >= 50) THEN
            PRINT*, CRM, ExmAvg, Final, 'PASS'
        ELSE
            PRINT*, CRM, ExmAvg, Final, 'FAIL'
        END IF
    END DO
END
```
A shorter form if IF construct:
```fortran
IF (condition) statement
! can be used when the code block inside IF has only 1 statement
```
