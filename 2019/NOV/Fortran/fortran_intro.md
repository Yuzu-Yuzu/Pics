# Fortran: Introduction
<!-- TOC -->

- [Setup Fortran Environment](#setup-fortran-environment)
- [My First Fortran Program: Hello](#my-first-fortran-program-hello)
- [Compiling a Fortran Program](#compiling-a-fortran-program)
- [Program Layout: Program Money](#program-layout-program-money)
- [Names and Variables](#names-and-variables)

<!-- /TOC -->
# Setup Fortran Environment
```bash
# First check if the environment is setup
$ gcc -v                        # checks for gcc
$ gfortran -v                   # checks for gfortran
# if either one returns an error
$ sudo apt install gcc          # install gcc
$ sudo apt install gfortran     # install gfortran
```
Recommended IDE: **VS Code** with the **Modern Fortran** extension by Migual Carvajal.
- Grammar Highlighting
- Auto-completion
# My First Fortran Program: Hello
```fortran
! hello.f90

! A line beginning with '!' is a comment
! This line is a comment
CHARACTER NAME*20
PRINT*, 'What is your name?'    ! print* is like cout
READ*, NAME                     ! read* is like cin
PRINT*, 'Hi there, ', NAME     
END                             ! end of program
```
# Compiling a Fortran Program
```bash
# Similar to gcc
$ gfortran -o hello hello.f90
$ ./hello
```
# Program Layout: Program Money
```fortran
! money.f90

! Fortran is case-insensitive
! No semicolon at the end of statements
program money                            ! start of program
    real balance, interest, rate         ! variable declaration

    balance = 1000                       ! assignment
    rate = 0.09
    interest = rate * balance            
    balance = balance + interest         
    print*, 'new balance: ', balance
    end program money                    ! end of program

```
The general structure of a simple Fortran program is as follows (items in square brackets are optional):
```fortran
[program program_name]
    [statement]
    [statement]
end [program [program name]]
```
The following code will also compile.</br>
```fortran
! money_alternative.f90

! Actually, the only compulsory statement in Fortran is "end"
program money                            ! this line optional
    real balance, interest, rate         

    balance = 1000; rate = 0.09          ! Statements in a line separated by;
    interest = rate * &                  ! separating a statement 
    balance                              ! into multiple lines using &
    balance = balance + interest         
    print*, 'new balance: ', balance
    end                                  ! can be replaced by "end program"
```
# Names and Variables
A Fortran identifier must satisfy the following:
- Starts with a letter
- Contains only letters, digits, and underscores.
- Has maximum length of 31

Identifiers are **case-insensitive**.</br>
i.e. "rate" is the same as "RATE", "rAtE", etc.
