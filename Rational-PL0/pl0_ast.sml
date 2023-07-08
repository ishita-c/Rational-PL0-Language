open BigInt
structure Rational = Rational(BigInt)
open Rational

structure AST =
struct

type Letter = string
type Digit = int
type id = string
type num = string
type ProgramString = string

datatype Type = INT | BOOL

datatype Program = PROG of Block
and Block = BLK of DeclarationSeq*CommandSeq
            | BlockCSeq of CommandSeq
and CommandSeq = CmdSeqExpSeq of CommandSeqNoBraces
                | EmptyCSeq
and CommandSeqNoBraces = CmdSeqNoBraces of Command
                    | CmdSeqNoBraces2 of CommandSeqNoBraces*Command
and DeclarationSeq = DecSeq1 of VarDecls*ProcDecls
                | DecSeq2 of ProcDecls
                | DecSeq3 of VarDecls
and VarDecls = VarDecls1 of RatVarDecls * IntVarDecls * BoolVarDecls
            | VarDecls2 of RatVarDecls * IntVarDecls
            | VarDecls3 of RatVarDecls * BoolVarDecls
            | VarDecls4 of IntVarDecls * BoolVarDecls
            | VarDecls5 of RatVarDecls
            | VarDecls6 of IntVarDecls
            | VarDecls7 of BoolVarDecls
and RatVarDecls = RatVarDecs of IdentifierList
and IntVarDecls = IntVarDecs of IdentifierList
and BoolVarDecls = BoolVarDecs of IdentifierList
and IdentifierList = IdList1 of id
                | IdList2 of id * IdentifierList
and ProcDecls = ProcDecs of ProcDeclsSeq
and ProcDeclsSeq = ProcDecsSeq1 of ProcDef
                | ProcDecsSeq2 of ProcDeclsSeq*ProcDef
and ProcDef = ProcDefType of id*Block
and Command = CmdAssignmentCmd of AssignmentCmd
            | CmdWhileCmd of WhileCmd
            | CmdCallCmd of CallCmd
            | CmdReadCmd of ReadCmd
            | CmdPrintCmd of PrintCmd
            | CmdConditionalCmd of ConditionalCmd
and AssignmentCmd = AssignmentCmdType of id*Expression
and WhileCmd = WhileCmdType of Expression*CommandSeq
and CallCmd = CallCmdType of id
and ReadCmd = ReadCmdType of id
and PrintCmd = PrintCmdType of Expression
and ConditionalCmd = ConditionalCmdType of Expression*CommandSeq*CommandSeq
and Expression = MakeRatExp of Expression
                | RatExp of Expression
                | ShowRatExp of Expression
                | ShowDeciExp of Expression
                | FromDeciExp of Expression
                | ToDeciExp of Expression
                | BoolExp of Expression*BoolOp*Expression
                | AddIntExp of Expression*AddOp*Expression
                | PosExp of Expression
                | NegExp of Expression
                | MultExp of Expression*MultOp*Expression
                | AddRatExp of Expression*RatAddOp*Expression
                | MultRatExp of Expression*RatMultOp*Expression
                | LRparenExp of Expression
                | LRcurlyExp of Expression
                | RelExp of Expression*RelOp*Expression
                | NotExp of Expression
                | InvExp of Expression
                | TrueExp
                | FalseExp
                (* | NumExp of num *)
                | RationalExp of RationalExpression
                | IdExp of id
and RationalExpression = RationalExpression1 of num*num*num
                        | RationalExpression2 of num*num
and BoolOp = ORELSE | ANDALSO
and AddOp = PLUS | INTMINUS
and MultOp = INTDIV | MOD | INTMULT
and RatAddOp = RATPLUS | RATMINUS
and RatMultOp = RATMULT | RATDIV
and RelOp = LESSTHAN | LESSTHANOREQUAL
            | EQUAL | GREATERTHAN
            | GREATERTHANOREQUAL | NOTEQUAL

datatype value = RatVal of Rational.rational
				| BoolVal of bool

type environment = (id*string*value) list
type environment_proc = (id*value) list

(* val gbEnv:environment ref = ref [] *)

end
