structure AST =
struct

type num =int

datatype binop = Add | Sub | Mul | Div
datatype typ = INT


datatype Prog = Prog of RationalExp

and RationalExp =  AddExp of RationalExp*AddOp*Term1
                            | SubExp of RationalExp*SubOp*Term1
                            | NegExp of num
                            | FracExp of DivOp*num
                            | DeciExp1 of num*num*num
                            | DeciExp2 of num*num*num
                            | DeciExp3 of num*num*num
                            | NumExp of num 

and Term1 = MultExp of RationalExp*MultOp*Term2
and Term2 = DivExp of RationalExp*DivOp*RationalExp
                            

datatype value = IntVal of int


end






