open BigInt
structure Rational = Rational(BigInt)

structure Evaluator = 
struct
open AST

val brokenTypes = Fail "Error in evaluation!"

val gbEnv:environment ref = ref []
val gbEnvProc:environment_proc ref = ref []

(* and helper_detailed (nil) = ()
| helper_detailed ((h1,h2,h3)::nil : (id * Type * value) list) = 
(case h3 of
	IntVal x => print ( (h1) ^ "->Type:INT, Value:" ^ Int.toString(x) ^ "\n")
	| BoolVal x => print ( (h1) ^ "->Type:BOOL, Value:" ^ Bool.toString(x) ^ "\n")
)
| helper_detailed ((h1,h2,h3)::tl : (id * Type * value) list) = 
(
	case h3 of
	IntVal x => (print ( (h1) ^ "->Type:INT, Value:" ^ Int.toString(x) ^ "\n");helper_detailed(tl))
	| BoolVal x => (print ( (h1) ^ "->Type:BOOL, Value:" ^ Bool.toString(x) ^ "\n");helper_detailed(tl))
) *)



(* (print ( "->"^ (#1 (hd)) ^ "," ^  "," ^ "<-"); helper_detailed (tl)); *)

(* and print_memory(env:environment) = (
	let val x = ref env in (
		print ("\n");
		helper_detailed (!x);
		print ("\n")
	)
	end
); *)


(* and AddRemVarList(vars: RemVariable, varType) *)

fun updateVarid(i:id,v:value,env:environment) =
		let
		val newEnv =
				List.map(fn (x, t, vOld) => if (x=i) then 
					(
						(x,t,v)
						(* case v of 
						IntVal iVal => (print("Found match - " ^ x ^ ", New Val:" ^ Int.toString(iVal));(x,t,iVal) )
						| BoolVal bVal => (print("Found match - " ^ x ^ ", New Val:" ^ Bool.toString(bVal));(x,t,bVal)) *)
					) 
					else (x,t,vOld) 
				) env;
		in
			(
				(* newEnv;
				print("Debugging\n");
				print_memory(newEnv); *)
				gbEnv := newEnv
			)
		end


and AddVarid(i: id, t: string)=
	case t of
	"int" => gbEnv := (i, t, RatVal(Rational.rat(BigInt.zero)) ) :: !gbEnv
	| "bool" => gbEnv := (i, t, BoolVal false) :: !gbEnv
	| "rat" => gbEnv := (i, t, RatVal(Rational.rat(BigInt.zero)) ) :: !gbEnv

and AddVarList(vars : IdentifierList, varType : string)=
	case vars of 
		IdList2(i1,i2) => (AddVarid(i1,varType);AddVarList(i2,varType))
		| IdList1 i => AddVarid(i,varType)

and envAddHelperVar(d: VarDecls)=
	case d of
		VarDecls1 (r, i, b) =>
		(
			case r of
				RatVarDecs il => AddVarList(il, "rat");
			case i of
				IntVarDecs il => AddVarList(il, "int");
			case b of
				BoolVarDecs il => AddVarList(il, "bool")
		)
		| VarDecls2 (r, i) =>
		(
			case r of
				RatVarDecs il => AddVarList(il, "rat");
			case i of
				IntVarDecs il => AddVarList(il, "int")
		)
		| VarDecls3 (r, b) =>
		(
			case r of
				RatVarDecs il => AddVarList(il, "rat");
			case b of
				BoolVarDecs il => AddVarList(il, "bool")
		)
		| VarDecls4 (i, b) =>
		(
			case i of
				IntVarDecs il => AddVarList(il, "int");
			case b of
				BoolVarDecs il => AddVarList(il, "bool")
		)
		| VarDecls5 (r) =>
		(
			case r of
				RatVarDecs il => AddVarList(il, "rat")
		)
		| VarDecls6 (i) =>
		(
			case i of
				IntVarDecs il => AddVarList(il, "int")
		)
		| VarDecls7 (b) => (
			case b of
				BoolVarDecs il => AddVarList(il, "bool")
		)


and AddDeclId(declId: ProcDef)=
	case declId of
		ProcDefType (id1, blk2) => (
			let
				val v = intereval(blk2)
			in
				gbEnvProc := (id1, v) :: !gbEnvProc
			end
		)
(* end; *)


and AddDeclList(declSeq: ProcDeclsSeq)=
	case declSeq of 
		ProcDecsSeq1 pdef => AddDeclId(pdef)
		| ProcDecsSeq2 (pdefSeq, pdef) => (
			AddDeclList(pdefSeq);
			AddDeclId(pdef)
		)

and envAddHelperDecl(d: ProcDecls)=
	case d of
		ProcDecs pds => (
			case pds of
				ProcDecsSeq1 pdef => AddDeclId(pdef)
				| ProcDecsSeq2 (pdefSeq, pdef) => (
					AddDeclList(pdefSeq);
					AddDeclId(pdef)
				)
		)


and envAdd(ds: DeclarationSeq)=
	case ds of
		DecSeq1 (vd1,pd2) => (envAddHelperVar(vd1);envAddHelperDecl(pd2))
		| DecSeq2 pd1 => envAddHelperDecl(pd1)
		| DecSeq3 vd1 => envAddHelperVar(vd1)

and envLookup (var:id,env:environment):value=
    case List.find(fn (x, _, _) => x = var) env of
	       SOME (x, t, v)   => (
					 case v of 
					 RatVal i => RatVal i
					 | BoolVal b => BoolVal b
				 )
				 | NONE => raise Fail ("Environment lookup error for "^ var^" in evaluation ")

and executeExp(e:Expression):value=
	case e of
		MakeRatExp e => executeMakeRatExp(e)
		| RatExp e => executeRatExp(e)
		| ShowRatExp e => executeShowRatExp(e)
		| ShowDeciExp e => executeShowDeciExp(e)
		| FromDeciExp e => executeFromDeciExp(e)
		| ToDeciExp e => executeToDeciExp(e)
		| BoolExp (e1,bo,e2) => executeBoolExp(e1,bo,e2)
		| AddIntExp (e1,ao,e2) => executeAddIntExp(e1,ao,e2)
		| PosExp e => executeToRationalExp(0, e)
		| NegExp e => executeToRationalExp(1, e)
		| MultExp (e1,mo,e2) => executeMultExp(e1,mo,e2)
		| AddRatExp (e1,aro,e2) => executeAddRatExp(e1,aro,e2)
		| MultRatExp (e1,rmo,e2) => executeMultRatExp(e1,rmo,e2)
		| LRparenExp e => executeExp(e)
		| LRcurlyExp e => executeExp(e)
		| RelExp (e1,ro,e2) => executeRelExp(e1,ro,e2)
		| NotExp e => executeNotExp(e)
		| InvExp e => executeInvExp(e)
		| TrueExp => BoolVal true
		| FalseExp => BoolVal false
		(* | NumExp i => IntVal i *)
		| RationalExp re => executeToRational(0, re)
		| IdExp i => envLookup(i,!gbEnv)

and executeToRationalExp(sign: int, e:Expression):value=
	case e of
		RationalExp r => executeToRational(sign, r)
		| _ => raise brokenTypes
and executeToRational(sign: int, re:RationalExpression):value=
	case re of
		RationalExpression1 (n1, n2, n3) => (
			let
				val b1 = toBigInt(n1);
				val b2 = toBigInt(n2);
				val b3 = toBigInt(n3)
			in
				RatVal(sign, b1, b2, b3)
				(* (sign, b1, b2, b3) *)
			end
		)
		| RationalExpression2 (n1, n2) => (
			let
				val b1 = toBigInt(n1);
				val b2 = toBigInt("-1");
				val b3 = toBigInt(n2)
			in
				RatVal(sign, b1, b2, b3)
				(* (sign, b1, b2, b3) *)
			end
		)

and executeMakeRatExp(e:Expression)=
	case e of
		RationalExp r => 
		(
			let
				val r = executeToRational(0, r)
			in
				r
			end
		)
		| _ => raise brokenTypes
and executeAddIntExp(e1:Expression, ao:AddOp, e2:Expression)=
	case (ao,executeExp(e1),executeExp(e2)) of 
	(PLUS,RatVal e1, RatVal e2) => 
	(
		print(Rational.showDecimal(e1));
		print("\n");
		print(Rational.showDecimal(e2));
		print("\n");
		(* print("Reached here"); *)
		(* print(Rational.showDecimal(Rational.add(e1, e2))); *)
		print("\n");
		RatVal(Rational.add(e1, e2))

	)
	| (INTMINUS,RatVal e1, RatVal e2) => RatVal(Rational.subtract(e1, e2))
	| _ => raise brokenTypes
	
and executeRatExp(e:Expression)=
	case e of
		RationalExp r => (
			let
				val r = executeToRational(0, r)
			in
				r
			end
		)
		| _ => raise brokenTypes

and executeShowRatExp(e:Expression)=
	case e of
		RationalExp r => (
			let
				val r = executeToRational(0, r)
			in
				r
			end
		)
		| _ => raise brokenTypes

and executeShowDeciExp(e:Expression)=
	case e of
		RationalExp r => (
			let
				val r = executeToRational(0, r)
			in
				r
			end
		)
		| _ => raise brokenTypes

and executeFromDeciExp(e:Expression)=
	case e of
		RationalExp r => (
			let
				val r = executeToRational(0, r)
			in
				r
			end
		)
		| _ => raise brokenTypes

and executeToDeciExp(e:Expression)=
	case e of
		RationalExp r => (
			let
				val r = executeToRational(0, r)
			in
				r
			end
		)
		| _ => raise brokenTypes

and executeBoolExp(e1:Expression, bop: BoolOp, e2:Expression)=
	case (bop,executeExp(e1),executeExp(e2)) of 
	(ORELSE,BoolVal b1, BoolVal b2) => BoolVal (b1 orelse b2)
	| (ANDALSO,BoolVal b1, BoolVal b2) => BoolVal (b1 andalso b2)
	| _ => raise brokenTypes

and executeMultExp(e1:Expression, mo: MultOp, e2: Expression)=
	case (mo,executeExp(e1),executeExp(e2)) of 
	(INTDIV,RatVal e1, RatVal e2) => RatVal(Rational.divide(e1, e2))
	| (MOD,RatVal e1, RatVal e2) => RatVal(Rational.modulo(e1, e2))
	| (INTMULT,RatVal e1, RatVal e2) => RatVal(Rational.multiply(e1, e2))
	| _ => raise brokenTypes

and executeAddRatExp(e1:Expression, aro: RatAddOp, e2: Expression)=
	case (aro,executeExp(e1),executeExp(e2)) of 
	(RATPLUS,RatVal e1, RatVal e2) => RatVal(Rational.add(e1, e2))
	| (RATMINUS,RatVal e1, RatVal e2) => RatVal(Rational.subtract(e1, e2))
	| _ => raise brokenTypes

and executeMultRatExp(e1:Expression, rmo: RatMultOp, e2: Expression)=
	case (rmo,executeExp(e1),executeExp(e2)) of 
	(RATMULT,RatVal e1, RatVal e2) => RatVal(Rational.multiply(e1, e2))
	| (RATDIV,RatVal e1, RatVal e2) => RatVal(Rational.divide(e1, e2))
	| _ => raise brokenTypes			

(* and executeLRparenExp(e:Expression)=
	case e of
		RationalExp r => Rational.showRat(r)

and executeLRcurlyExp(e:Expression)=
	case e of
		RationalExp r => Rational.showRat(r) *)

and executeRelExp(e1:Expression, ro: RelOp, e2: Expression)=
	case (ro,executeExp(e1),executeExp(e2)) of 
	(LESSTHAN, RatVal e1, RatVal e2) => BoolVal(Rational.less(e1, e2))
	| (GREATERTHAN, RatVal e1, RatVal e2) => BoolVal(Rational.less(e2, e1))
	| (EQUAL, RatVal e1, RatVal e2) => BoolVal(Rational.equal(e1, e2))
	| (EQUAL, BoolVal e1, BoolVal e2) => BoolVal(e1 = e2)
	| (LESSTHANOREQUAL, RatVal e1, RatVal e2) => BoolVal(not(Rational.less(e2, e1)) )
	| (GREATERTHANOREQUAL, RatVal e1, RatVal e2) => BoolVal(not(Rational.less(e1, e2)))
	| (NOTEQUAL, RatVal e1, RatVal e2) => BoolVal(not(Rational.equal(e1, e2)))
	| (NOTEQUAL, BoolVal e1, BoolVal e2) => BoolVal(not(e1 = e2))
	| _ => raise brokenTypes

and executeInvExp(e:Expression)=
	case e of
		RationalExp r => (
			let
				val r = executeToRational(0, r)
			in
				r
			end
		)
		| _ => raise brokenTypes

and executeWhileExp(e:Expression,c:CommandSeq)=
	case executeExp(e) of
		BoolVal x => if(x=true)then (executeCSeq(c);executeWhileExp(e,c)) else (RatVal(Rational.rat(BigInt.zero)))
		| _ => raise brokenTypes


and executeCallExp(id1: id)=
	(
		let
			val v = envLookup(id1,!gbEnv)
		in
			v
		end
	)
and executePrintExp(e: Expression)=(
	let 
		val v = executeExp(e)
	in
		v
	end

	(* case executeExp(e) of 
		RatVal x => Rational.showDecimal(x)
		| _ => raise brokenTypes *)
)


(* and executePrintExp(e: Expression)=(
	case e of
		RationalExp r => (
			let
				val r = executeToRational(0, r)
			in
				case r of
					RatVal rnew => (
						print(Rational.showDecimal(rnew));
						print("\n");
						r
					)
					| BoolVal bv => (
						print(Bool.toString(bv));
						print("\n");
						BoolVal true
					)
			end
		)
		| _ => 
		(
			raise brokenTypes
		)
) *)

and executeConditionalExp(e1:Expression, cs2:CommandSeq, cs3:CommandSeq)=
	case executeExp(e1) of
		BoolVal x => if(x=true)then executeCSeq(cs2) else executeCSeq(cs3)
		| _ => raise brokenTypes

and executeNotExp(e:Expression)=
	case (executeExp(e)) of
		RatVal i => BoolVal (not (Rational.equal(i, Rational.rat(BigInt.zero))))
		(* | BoolVal b => BoolVal (not b) *)
		| _ => raise brokenTypes

and executeWriteExp(e:Expression)=executeExp(e)

and executeReadExp(ident:id)=
	(
		let
			val v = envLookup(ident,!gbEnv)
		in
			v
		end
	)
and executeAssignExp(id:id,e:Expression)=executeExp(e)
and executeCmd(c:Command)=
	case c of
		CmdAssignmentCmd ac => 
		(
			case ac of
				AssignmentCmdType (id1, e2) => executeAssignExp(id1,e2)
		)
		| CmdWhileCmd wc => 
		(
			case wc of
				WhileCmdType (e1, cs2) => executeWhileExp(e1,cs2)
		)
		| CmdCallCmd cc => 
		(
			case cc of
				CallCmdType id1 => executeCallExp(id1)
		)
		| CmdReadCmd rc => 
		(
			case rc of
				ReadCmdType id1 => executeReadExp(id1)
		)
		| CmdPrintCmd pc => 
		(
			case pc of
				(* PrintCmdType e1 =>executePrintExp(executeExp(e1)) *)
				PrintCmdType e1 => executePrintExp(e1)
		)
		| CmdConditionalCmd condc => 
		(
			case condc of
				ConditionalCmdType (e1,cs2,cs3) => executeConditionalExp(e1,cs2,cs3)
		)

and executeCSeqNoBraces(c:CommandSeqNoBraces)=
	case c of
		CmdSeqNoBraces2 (csnb2,c) => (executeCSeqNoBraces(csnb2);executeCmd(c))
		| CmdSeqNoBraces c => executeCmd(c)
			
and executeCSeq(c:CommandSeq)=
	case c of
		CmdSeqExpSeq csnb => executeCSeqNoBraces(csnb)
		| EmptyCSeq => RatVal(Rational.rat(BigInt.zero))

and intereval(programBlk: Block)=
  	let val b = programBlk
  	in
  	(
		case programBlk of
		  	BLK (d1,c1) => 
			(
				gbEnv := [];
				envAdd(d1);

				(* adds variable to the env *)
				(* 2 type: procedure: id, variables: bool, int, bigint, rational *)

				(* print_memory(!gbEnv); *)
				(* IntVal 0 *)
				executeCSeq(c1)
				(* print_memory(!gbEnv) *)
			)
			| BlockCSeq (cs) => 
			(
				gbEnv := [];
				executeCSeq(cs)
				(* print_memory(!gbEnv) *)
			)
  	)
  	end

and evaluate(p: Program)= 
    case p of 
        PROG s1 => intereval(s1)

end