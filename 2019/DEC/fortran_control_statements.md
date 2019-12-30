# Fortran Controll Statements
<!-- TOC -->

- [DO Loop](#do-loop)
- [DO-EXIT Loop](#do-exit-loop)
- [DO-WHILE Loop](#do-while-loop)
- [IF-THEN-ELSE Statement](#if-then-else-statement)
- [SELECT-CASE Statement](#select-case-statement)
- [GOTO Statement](#goto-statement)

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
# SELECT-CASE Statement
```fortran
CHARACTER CH
DO
    READ*, CH
    PRINT*, ICHAR( CH )
    IF (CH == '@') EXIT
    IF (CH >= 'A' .and. CH <= 'Z'.or. CH >= 'a' .and. CH <= 'z') THEN
        SELECT CASE (CH)
        CASE ('A', 'E', 'I', 'O', 'U', 'a', 'e', 'i', 'o', 'u')
            PRINT*, 'Vowel'
        CASE DEFAULT
            PRINT*, 'Consonant'
        END SELECT
    ELSE
        PRINT*, 'Something else'
    END IF
END DO
```
Colon may be used to specify a range of values.
```fortran
SELECT CASE ( INT(Final) )
CASE (75:)
    PRINT*, Name, CRM, ExmAvg, Final, '1'
    Firsts = Firsts + 1
CASE (70:74)
    PRINT*, Name, CRM, ExmAvg, Final, '2+'
    UpSeconds = UpSeconds + 1
CASE (60:69)
    PRINT*, Name, CRM, ExmAvg, Final, '2-'
    LowSeconds = LowSeconds + 1
CASE (50:59)
    PRINT*, Name, CRM, ExmAvg, Final, '3'
    Thirds = Thirds + 1
CASE DEFAULT
    PRINT*, Name, CRM, ExmAvg, Final, 'F'
    Fails = Fails + 1
END SELECT
```
# GOTO Statement
label is a number in the range 1–99999 preceding a statement on the same line.
```fortran
! Using IF-THEN-ELSE
IF (L1) THEN
    I = 1
    J = 2
ELSE IF (L2) THEN
    I = 2
    J = 3
ELSE
    I = 3
    J = 4
END IF



! Using GOTO
IF (.NOT.L1) GOTO 10
I = 1
J = 2
GOTO 30
10 IF (.NOT.L2) GOTO 20
I = 2
J = 3
GOTO 30
20 I = 3
J = 4
30 CONTINUE ! Dummy statement - does nothing
```
