
# Context-free grammar (EBNF of the Rational-PL0 language)

Program: Block (AST.PROG(Block))

Block: DeclarationSeq CommandSeq (AST.BLK(DeclarationSeq,CommandSeq))
        | CommandSeq (AST.BlockCSeq(CommandSeq))

DeclarationSeq: VarDecls ProcDecls (AST.DecSeq1(VarDecls, ProcDecls))
                | ProcDecls (AST.DecSeq2(ProcDecls))
                | VarDecls (AST.DecSeq3(VarDecls))

VarDecls: RatVarDecls IntVarDecls BoolVarDecls (AST.VarDecls1(RatVarDecls, IntVarDecls, BoolVarDecls))
        | RatVarDecls IntVarDecls (AST.VarDecls2(RatVarDecls, IntVarDecls))
        | RatVarDecls BoolVarDecls (AST.VarDecls3(RatVarDecls, BoolVarDecls))
        | IntVarDecls BoolVarDecls (AST.VarDecls4(IntVarDecls, BoolVarDecls))
        | RatVarDecls (AST.VarDecls5(RatVarDecls))
        | IntVarDecls (AST.VarDecls6(IntVarDecls))
        | BoolVarDecls (AST.VarDecls7(BoolVarDecls))

RatVarDecls: RATIONALNUM IdentifierList SEMICOLON (AST.RatVarDecs(IdentifierList))

IntVarDecls: INTEGERNUM IdentifierList SEMICOLON (AST.IntVarDecs(IdentifierList))

BoolVarDecls: BOOLEANNUM IdentifierList SEMICOLON (AST.BoolVarDecs(IdentifierList))

IdentifierList: ID (AST.IdList1(ID))
                | ID COMMA IdentifierList (AST.IdList2(ID, IdentifierList))

ProcDecls: ProcDeclsSeq SEMICOLON (AST.ProcDecs(ProcDeclsSeq))

ProcDeclsSeq: ProcDef (AST.ProcDecsSeq1(ProcDef))
            | ProcDeclsSeq SEMICOLON ProcDef (AST.ProcDecsSeq2(ProcDeclsSeq, ProcDef))

ProcDef: PROCEDURE ID Block (AST.ProcDefType(ID, Block))

CommandSeq: LCURLY RCURLY (AST.EmptyCSeq)
        | LCURLY CommandSeqNoBraces RCURLY (AST.CmdSeqExpSeq(CommandSeqNoBraces))

CommandSeqNoBraces: Command SEMICOLON (AST.CmdSeqNoBraces(Command))
                | CommandSeqNoBraces SEMICOLON Command (AST.CmdSeqNoBraces2(CommandSeqNoBraces, Command))

Command: AssignmentCmd (AST.CmdAssignmentCmd(AssignmentCmd))
        | CallCmd (AST.CmdCallCmd(CallCmd))
        | ReadCmd (AST.CmdReadCmd(ReadCmd))
        | PrintCmd (AST.CmdPrintCmd(PrintCmd))
        | ConditionalCmd (AST.CmdConditionalCmd(ConditionalCmd))
        | WhileCmd (AST.CmdWhileCmd(WhileCmd))

AssignmentCmd: ID ASSIGN Expression (AST.AssignmentCmdType(ID, Expression))

CallCmd: CALL ID (AST.CallCmdType(ID))

ReadCmd: READ LPAREN ID RPAREN (AST.ReadCmdType(ID))

PrintCmd: PRINT LPAREN Expression RPAREN (AST.PrintCmdType(Expression))

ConditionalCmd: IF Expression THEN CommandSeq ELSE CommandSeq FI (AST.ConditionalCmdType(Expression, CommandSeq1, CommandSeq2))

WhileCmd: WHILE Expression DO CommandSeq OD (AST.WhileCmdType(Expression, CommandSeq))

Expression: MAKERAT LPAREN Expression RPAREN        (AST.MakeRatExp(Expression))
            | RAT LPAREN Expression RPAREN             (AST.RatExp(Expression))
            | SHOWRAT LPAREN Expression RPAREN         (AST.ShowRatExp(Expression))
            | SHOWDECI LPAREN Expression RPAREN        (AST.ShowDeciExp(Expression))
            | FROMDECI LPAREN Expression RPAREN        (AST.FromDeciExp(Expression))
            | TODECI LPAREN Expression RPAREN          (AST.ToDeciExp(Expression)) 
            | Expression ORELSE Expression     (AST.BoolExp(Expression1, AST.ORELSE, Expression2))
            | Expression ANDALSO Expression    (AST.BoolExp(Expression1, AST.ANDALSO, Expression2))
            | Expression PLUS Expression   (AST.AddIntExp(Expression1, AST.PLUS, Expression2))
            | Expression INTMINUS Expression  (AST.AddIntExp(Expression1, AST.INTMINUS, Expression2))
            | PLUS Expression   (AST.PosExp(Expression))
            | NEGATE Expression   (AST.NegExp(Expression))
            | Expression INTDIV Expression    (AST.MultExp(Expression1, AST.INTDIV, Expression2))
            | Expression MOD Expression    (AST.MultExp(Expression1, AST.MOD, Expression2))
            | Expression INTMULT Expression   (AST.MultExp(Expression1, AST.INTMULT, Expression2))
            | Expression RATPLUS Expression   (AST.AddRatExp(Expression1, AST.RATPLUS, Expression2))
            | Expression RATMINUS Expression  (AST.AddRatExp(Expression1, AST.RATMINUS, Expression2))
            | Expression RATMULT Expression   (AST.MultRatExp(Expression1, AST.RATMULT, Expression2))
            | Expression RATDIV Expression    (AST.MultRatExp(Expression1, AST.RATDIV, Expression2))
            | LPAREN Expression RPAREN     (AST.LRparenExp(Expression))
            | LCURLY Expression RCURLY     (AST.LRcurlyExp(Expression))
            | Expression LESSTHAN Expression     (AST.RelExp(Expression1, AST.LESSTHAN, Expression2))
            | Expression LESSTHANOREQUAL Expression    (AST.RelExp(Expression1, AST.LESSTHANOREQUAL, Expression2))
            | Expression EQUAL Expression     (AST.RelExp(Expression1, AST.EQUAL, Expression2))
            | Expression GREATERTHAN Expression     (AST.RelExp(Expression1, AST.GREATERTHAN, Expression2))
            | Expression GREATERTHANOREQUAL Expression    (AST.RelExp(Expression1, AST.GREATERTHANOREQUAL, Expression2))
            | Expression NOTEQUAL Expression    (AST.RelExp(Expression1, AST.NOTEQUAL, Expression2))
            | NOT Expression (AST.NotExp(Expression))
            | INVERSE Expression (AST.InvExp(Expression))
            | TRUE (AST.TrueExp)
            | FALSE (AST.FalseExp)
            (*  | NUM (AST.NumExp(NUM))  *)
            | RationalExpression (AST.RationalExp(RationalExpression))
            | ID (AST.IdExp(ID))

RationalExpression: NUM DECI NUM LPAREN NUM RPAREN (AST.RationalExpression1(NUM1, NUM2, NUM3))
                | NUM DECI LPAREN NUM RPAREN (AST.RationalExpression2(NUM1, NUM2))

NUM ::= Digit{Digit}

ID ::= Letter{Letter|Digit}
Letter ::= Uppercase | Lowercase

UpperCase ::= “A” | “B” | “C” | “D” | “E” | “F” | “G” | “H” | “I” | “J” | “K” | “L” | “M” | “N” | “O” | “P” | “Q” | “R” | “S” | “T” | “U” | “V ” | “W” | “X” | “Y ” | “Z”
LowerCase ::= “a” | “b” | “c” | “d” | “e” | “f” | “g” | “h” | “i” | “j” | “k” | “l” | “m” | “n” | “o” | “p” | “q” | “r” | “s” | “t” | “u” | “v” | “w” | “x” | “y” | “z”

Digit ::= “0” | “1” | “2” | “3” | “4” | “5” | “6” | “7” | “8” | “9”

# Syntax-directed translation

This is mentioned in the grammar itself.

# Function Call

A function named `generateAST("filename")` is used that generates the AST inside the file.

A function named `interpret("input_filename", "output_filename")` is used that outputs the evaluation of the file "input_filename".

# Acknowledgements

Referred code of Prof. Subodh Sharma from 2nd Semester 2020-21.

# Evaluation Instructions

make
generateAST("input.rat");
evaluate(it);

OR

make
interpret("input.rat", "out.txt");
