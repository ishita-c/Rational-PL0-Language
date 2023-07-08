%%
(* required declarations *)

%name Rational

%term    PLUS | MINUS | MULT | DIV | DECI | UNARYMINUS
      | LPAREN | RPAREN | EOF | NUM of int


%nonterm Prog of AST.Prog | RationalNumber of AST.RationalNumber | DivOp of AST.DivOp 
        | SubOp of AST.SubOp | AddOp of AST.AddOp | MultOp of AST.MultOp | Type of AST.typ
           
%pos int 

(*optional declarations *)

%eop EOF
%noshift EOF

(* %header  *)

%left PLUS MINUS 
%left DIV MULT
%nonassoc LPAREN RPAREN 



(* %right *)
%start Prog

%verbose

%%
Prog: Expression    (AST.Prog(RationalNumber))

Expression :            RationalExpression 
RationalExpression :    RationalExpression PLUS Term        (AST.AddExp(RationalExpression, AST.PLUS, Term))
                        | RationalExpression MINUS Term     (AST.SubExp(RationalExpression, AST.MINUS, Term))
                        | Term 
Term :                  Term MULTOP Term2                   (AST.MultExp(RationalExpression, AST.MULT, Term))
                        | Term2 
Term2 :                 Term2 DIVOP Factor                  (AST.DivExp(RationalExpression, AST.DIV, Term))
                        | Factor 
Factor :                RationalNumber 

RationalNumber :        RationalNumber1 
                        | RationalNumber2 
                        | RationalNumber3

RationalNumber1 :       BigInt
BigInt :                UNARYMINUS NUM                (AST.NegExp(NUM))
                        | NUM                         (AST.NumExp(NUM))

RationalNumber2 :       BigInt DIV NUM                (AST.FracExp(AST.DIV, NUM))

RationalNumber3 :       PLUS NUM DECI NUM LPAREN NUM RPAREN             (AST.DeciExp1(NUM1, NUM2, NUM3))
                        | NUM DECI NUM LPAREN NUM RPAREN                (AST.DeciExp2(NUM1, NUM2, NUM3))
                        | UNARYMINUS NUM DECI NUM LPAREN NUM RPAREN     (AST.DeciExp3(NUM1, NUM2, NUM3))





Type: INT (AST.INT) 
      | LPAREN Type RPAREN (Type)






            
            
         

