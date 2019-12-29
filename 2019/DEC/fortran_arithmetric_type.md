<!-- TOC -->

- [Numeric Expressions](#numeric-expressions)
  - [Exponent](#exponent)
  - [Integer Division](#integer-division)
  - [Float Point Division](#float-point-division)
- [Operator Precedence](#operator-precedence)
- [Datatypes](#datatypes)
  - [Integer](#integer)
  - [Real](#real)
  - [Complex](#complex)
  - [Logical](#logical)
  - [Character](#character)
- [Named Constants](#named-constants)

<!-- /TOC -->
# Numeric Expressions
## Exponent
> $$S=VT-\frac12gT^2$$
```fortran
! Fortran code:
    S = V * T - G / 2 * T ** 2    ! ** stands for exponent
```
> $$C=\frac{\sqrt{A^2+B^2}}{2A}$$
```fortran
! Fortran code:
    C = (A ** 2 + B ** 2) ** 0.5 / (2 * A)
! alternatively:
    C = sqrt( A ** 2 + B ** 2 ) / (2 * A)
```
## Integer Division
```
10 / 3 evaluates to 3
19 / 4 evaluates to 4
4 / 5 evaluates to 0 (which could cause an unwanted division by zero)
- 8 / 3 evaluates to -2
3 * 10 / 3 evaluates to 10
10 / 3 * 3 evaluates to 9
```
## Float Point Division
```
10 / 3.0 evaluates to 3.33333
4. / 5 evaluates to 0.8
2 ** (- 2) evaluates to 0 (?)
```
# Operator Precedence
```
Operator  Precedence       Meaning                   Example
**        1                Exponentiation            2 ** 4 (=16)
*         2                Multiplication            2 * A
/         3                Division                  B / DELTA
+         3                Addition or unary plus    A + 6.9
```
# Datatypes
## Integer
- Has three kinds that specifies the "size" of int.
- Kinds 1, 2, 3 under FTN90.
- May be 2, 4, 8 under a different compiler.
- Function SELECTED_INT_KIND(N): returns the kind parameter value for teh kind that will be able to represent all integers in the range $-10^N$ to $10^N$.
- Function HUGE(X) returns the largest value represented by its argument (integer or real).

```fortran
! select_int_kind

! k6 is the kind that holds from -10^6 to 10^6
integer, parameter :: k6 = selected_int_kind(6)

! count has the kind stated above
integer (k6) count

! alternatively
integer (kind = k6) sum
```

```fortran
big = huge(i)
small = big + 1
print*, 'default kind: ', kind(i)
print*, 'largest:      ', big
print*, 'smallest:     ', small
```
## Real 
- Two kinds 1 and 2. 2 is double precision.
- Function SELECTED_REAL_KIND similar to SELECTED_INTEGER_KIND
## Complex 
```fortran
COMPLEX, PARAMETER :: i = (0, 1)
COMPLEX X, Y
X = (1, 1)
Y = (1, -1)
PRINT*, CONJG(X), i * X * Y
```
## Logical 
- 2 literal constants: .true and .false
- Logical operators: .not., .and., .or., .eqv., .neqv.

## Character
- Two kinds ASCII and GREEK
```fortran
CHARACTER LETTER        
! declares LETTER to be a character variable of length 1
CHARACTER (Len = 15) Name
! Name can hold a string of up 15 characters

CHARACTER Name*15
! alternative definition

Name = 'J. Soap'
! initialization with string literal


CHARACTER (LEN = 10, KIND = GREEK) Greek_Word
```
# Named Constants
```fortran
REAL, PARAMETER :: G = 9.8
! declares G as a named constant, or parameter
! G may not be changed later in the program

REAL, PARAMETER :: Pi = 3.141593
INTEGER, PARAMETER :: Two = 2
REAL, PARAMETER :: OneOver2Pi = 1 / (2 * Pi)
REAL, PARAMETER :: PiSquared = Pi ** Two
! all parameters are evaluated at complie-time
```

