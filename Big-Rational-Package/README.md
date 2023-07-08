
# Context-free grammar of Rational Numbers (EBNF Grammar)

RationalNumber ::= RationalNumber1 | RationalNumber2 | RationalNumber3.

RationalNumber1 ::= BigInt.
BigInt ::= MINUS PosNum | PosNum.
PosNum ::= DIGIT | DIGIT PosNum.

RationalNumber2 ::= BigInt DIV Deno.
Deno ::= POSDIGIT PosNum | POSDIGIT.

RationalNumber3 ::= SIGN Int DECI Int LPAREN Recur RPAREN.
Int ::= "" | DIGIT Int.
Recur ::= DIGIT Recur | DIGIT.

DIGIT ::= “0” | POSDIGIT.
POSDIGIT ::= “1” | “2” | “3” | “4” | “5” | “6” | “7” | “8” | “9”.

LPAREN ::= "(".
RPAREN ::=")".
DIV ::= "/".
DECI ::= ".".
SIGN ::= "+" | "~" | "".
MINUS ::= "~". 

# Context-free grammar of Rational Numbers' Expression (EBNF Grammar)

Expression ::= RationalExpression 
RationalExpression ::= RationalExpression ADDOP Term | Term .
Term ::= Term MULTOP Term2 | Term2 .
Term2 ::= Term2 DIVOP Factor | Factor .
Factor ::= RationalNumber .
ADDOP ::= “+” | “−” .
MULTOP ::= “∗” .
DIVOP ::= "/" .

# Design Decisions
- In order to be able to deal with rationals involving large integers, we design a signature BIGINT for integers of type bigint and a structure BigInt: BIGINT which implements all the integer functions involving integers of unbounded magnitude. We set bigint to be of type "int list * int", i.e. list of all digits in the number in reverse ordered followed by 0/1 corresponding to positive/negative sign respectively. 

- Implemented bigint functions include 
    - toString (bigint to String)
    - toBigInt (String ti bigint)
    - add sub, mul, bigdiv (binary operators)
    - lt, eq (comparison operators)
    - bigabs (absolute)
    - gcd (greatest common divisor)
    - neg (negation)

    These functions were found to be enough for implementing rational number expressions.

- For the purpose of evaluation, we have considered rational numbers to be of the form "int * bigint * bigint * bigint" = (S,I,N,R) where 
        S = 0 for Positive rationals in decimal form
        S = 1 for Negative rationals in decimal form
        S = 2 for rationals in fractional normal form (p/q) with p = I and q = N > 0, R = 0 by convention

        N = bigint(-1) if N is empty in decimal form
        I = bigint(-1) if I is empty in decimal form

# Acknowledgements
Referred to code taught in class by Prof. Subodh V. Sharma in Semester II, 2020-21 
