%%
(* required declarations *)

(* Here VAR is for ->"var"<- VariableList *)

%name PL0

%term   RATIONALNUM | INTEGERNUM | BOOLEANNUM |
        TRUE | FALSE | VAR | IF | THEN | ELSE | FI |
        WHILE | DO | OD | PROCEDURE | PRINT | READ | CALL |
        DECI |
        INVERSE | RATPLUS | RATMULT | RATMINUS | RATDIV |
        MAKERAT | RAT | SHOWRAT | SHOWDECI | FROMDECI |
        TODECI | INTMINUS | INTMULT | INTDIV |
        MOD | NEGATE | PLUS | NOT | ANDALSO | ORELSE |
        LESSTHAN | GREATERTHAN | EQUAL | LESSTHANOREQUAL |
        GREATERTHANOREQUAL | NOTEQUAL | ASSIGN |
        LPAREN | RPAREN | LCURLY | RCURLY | SEMICOLON |
        COMMA | LCOMMENT | RCOMMENT | ID of string |
        NUM of string | EOF


%nonterm Program of AST.Program
        | Block of AST.Block
        | DeclarationSeq of AST.DeclarationSeq
        | VarDecls of AST.VarDecls
        | RatVarDecls of AST.RatVarDecls
        | IntVarDecls of AST.IntVarDecls
        | BoolVarDecls of AST.BoolVarDecls
        | IdentifierList of AST.IdentifierList
        | ProcDecls of AST.ProcDecls
        | ProcDeclsSeq of AST.ProcDeclsSeq
        | ProcDef of AST.ProcDef
        | CommandSeq of AST.CommandSeq
        | CommandSeqNoBraces of AST.CommandSeqNoBraces
        | Command of AST.Command
        | AssignmentCmd of AST.AssignmentCmd
        | CallCmd of AST.CallCmd
        | ReadCmd of AST.ReadCmd
        | PrintCmd of AST.PrintCmd
        | ConditionalCmd of AST.ConditionalCmd
        | WhileCmd of AST.WhileCmd
        | Expression of AST.Expression
        | RationalExpression of AST.RationalExpression

%pos int 

(*optional declarations *)

%eop EOF
%noshift EOF

(* %header  *)

%nonassoc IF THEN ELSE FI WHILE DO READ CALL PRINT RAT SHOWRAT SHOWDECI FROMDECI TODECI MAKERAT INVERSE
%left ANDALSO ORELSE EQUAL NOTEQUAL LESSTHAN GREATERTHAN LESSTHANOREQUAL GREATERTHANOREQUAL
%right ASSIGN
%right NOT NEGATE
%left PLUS INTMINUS RATPLUS RATMINUS
%left INTDIV INTMULT RATDIV RATMULT
%left MOD

(* %right *)
(* %nonassoc*)

%start Program

%verbose

%%

(* program, ID, Block, Declaration, DeclarationSeq, CommandSeq, ExpressionSeq, Expression *)

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

(* RatExp -> NUM DECI NUM LPAREN NUM RPAREN *)
(* RatExp -> NUM DECI LPAREN NUM RPAREN *)

(* 100000000000000000000000000000000000000000.123(4) *)


(*

(* program, ID, Block, Declaration, DeclarationSeq,
CommandSeq, ExpressionSeq, Expression *)


program: ProgramString ID DOUBLECOLON Block (AST.PROG(ID,Block))

Block: DeclarationSeq CommandSeq (AST.BLK(DeclarationSeq,CommandSeq))
        | CommandSeq (AST.BlockCSeq(CommandSeq))

(*
Block: Declaration Block(AST.BLK(Block, Declaration)) 
    | Declaration (AST.BLK(Declaration))
    | LCURLY CommandSeq RCURLY (AST.BLK(LCURLY, MultipleCommands, RCURLY))
    | CommandSeq Expression SEMICOLON (AST.BLK(MultipleCommands, Expression, SEMICOLON))
    | Expression SEMICOLON(AST.BLK(Expression, SEMICOLON))
    | LCURLY RCURLY (AST.BLK(LCURLY, RCURLY))
*)

CommandSeq: LCURLY ExpressionSeq RCURLY (AST.CmdSeqExpSeq(ExpressionSeq))
        | LCURLY RCURLY (AST.EmptyCSeq)

ExpressionSeq: Expression SEMICOLON ExpressionSeq(AST.ESeqExpExpSeq(Expression, ExpressionSeq))
        | Expression SEMICOLON (AST.ESeqExp(Expression))

DeclarationSeq: DeclarationSeq Declaration (AST.DECSEQ(Declaration, DeclarationSeq))
    | Declaration (AST.DecSeqDec(Declaration))

Declaration: VAR VariableList COLON Type SEMICOLON (AST.VarListDEC(VariableList,Type))

VariableList: ID RemVariable (AST.VarList(ID, RemVariable))
                | ID (AST.SingleVar(ID))

RemVariable: COMMA ID RemVariable (AST.RemVar(ID, RemVariable))
                | COMMA ID (AST.SingleRemVar(ID))

(*
Declaration: VAR ID Declaration COLON Type SEMICOLON (AST.VarDecType(Declaration, Type))
            | ID Declaration (AST.IdDec(ID, Declaration))
            | COMMA ID Declaration (AST.CIdDec(ID, Declaration))
            (* | Expression ()prakank *)
*)            

Expression: Expression ASSIGN Expression (AST.AssignExp(Expression1, Expression2))
            | READ Expression (AST.ReadExp(Expression))
            | WRITE Expression (AST.WriteExp(Expression))
            | IF Expression THEN CommandSeq ELSE CommandSeq ENDIF (AST.IteExp(Expression, CommandSeq1, CommandSeq2))
            | WHILE Expression DO CommandSeq ENDWH (AST.WhExp(Expression, CommandSeq))
            | Expression OR Expression     (AST.BoolExp(Expression1, AST.OR, Expression2))
            | Expression AND Expression    (AST.BoolExp(Expression1, AST.AND, Expression2))
            | Expression PLUS Expression   (AST.AddExp(Expression1, AST.PLUS, Expression2))
            | Expression MINUS Expression  (AST.AddExp(Expression1, AST.MINUS, Expression2))
            | Expression MULT Expression   (AST.MultExp(Expression1, AST.MULT, Expression2))
            | Expression DIV Expression    (AST.MultExp(Expression1, AST.DIV, Expression2))
            | Expression MOD Expression    (AST.MultExp(Expression1, AST.MOD, Expression2))
            | LPAREN Expression RPAREN     (AST.LRparenExp(Expression))
            | Expression LT Expression     (AST.RelExp(Expression1, AST.LT, Expression2))
            | Expression LEQ Expression    (AST.RelExp(Expression1, AST.LEQ, Expression2))
            | Expression EQ Expression     (AST.RelExp(Expression1, AST.EQ, Expression2))
            | Expression GT Expression     (AST.RelExp(Expression1, AST.GT, Expression2))
            | Expression GEQ Expression    (AST.RelExp(Expression1, AST.GEQ, Expression2))
            | Expression NEQ Expression    (AST.RelExp(Expression1, AST.NEQ, Expression2))
            | NOT Expression (AST.NotExp(Expression))
            | TRUE (AST.TrueExp)
            | FALSE (AST.FalseExp)
            | NUM (AST.NumExp(NUM))
            | ID (AST.IdExp(ID))

Type: INT (AST.INT)
| BOOL (AST.BOOL)

*)