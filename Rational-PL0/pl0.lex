val accum : (string *int * int) list ref = ref [];
structure Tokens=Tokens
	type pos = int
	type linenum=int ref
	type cpos= int ref
	type svalue  = Tokens.svalue
	type ('a,'b) token = ('a,'b) Tokens.token
	type lexresult = (svalue,pos) token
	
	val linenum = ref 1
	val pos = ref 1
	val cpos = ref 1
	val eof = fn () => Tokens.EOF(!linenum,!linenum)
	val error = fn (x,line:int,current:int) => TextIO.output(TextIO.stdOut,"Unknown Token:"^(Int.toString(line))^":"^(Int.toString(current))^x^"\n")
	
	fun increase (x:int ref,y:int) = (x:=(!x)+y)
	
%%

%header (functor PL0LexFun(structure Tokens:PL0_TOKENS));
alpha = [a-zA-Z];
ws = [\ \t\n];
digit=[0-9];
start_comment= \(\*;
end_comment=\*\);

%%

\n		=>(linenum := !linenum+1;cpos:= 1; increase(pos,1); lex());
{ws}+ 	=> (increase(cpos,size(yytext));lex());
{start_comment}(.|ws|\n)*{end_comment} => (increase(cpos,size(yytext));lex());



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"rational"	 	 =>(accum:= ("RATIONALNUM \"rational\"",(!linenum),(!cpos))::(!accum); increase (cpos,8);Tokens.RATIONALNUM(!linenum,!cpos));
"integer"	 	 =>(accum:= ("INTEGERNUM \"integer\"",(!linenum),(!cpos))::(!accum); increase (cpos,7);Tokens.INTEGERNUM(!linenum,!cpos));
"boolean"	 	 =>(accum:= ("BOOLEANNUM \"boolean\"",(!linenum),(!cpos))::(!accum); increase (cpos,7);Tokens.BOOLEANNUM(!linenum,!cpos));
"tt"		   =>(accum:= ("TRUE \"true\"",(!linenum),(!cpos))::(!accum); increase (cpos,2);Tokens.TRUE(!linenum,!cpos));
"ff"		   =>(accum:= ("FALSE \"false\"",(!linenum),(!cpos))::(!accum); increase (cpos,2);Tokens.FALSE(!linenum,!cpos));
"var"		   =>(accum:= ("VAR \"var\"",(!linenum),(!cpos))::(!accum); increase (cpos,3);Tokens.VAR(!linenum,!cpos));
"if"		   =>(accum:= ("IF \"if\"",(!linenum),(!cpos))::(!accum); increase (cpos,2);Tokens.IF(!linenum,!cpos));
"then"		 =>(accum:= ("THEN \"then\"",(!linenum),(!cpos))::(!accum);increase (cpos,4);Tokens.THEN(!linenum,!cpos));
"else"		 =>(accum:= ("ELSE \"else\"",(!linenum),(!cpos))::(!accum); increase (cpos,4);Tokens.ELSE(!linenum,!cpos));
"fi"		   =>(accum:= ("FI \"fi\"",(!linenum),(!cpos))::(!accum); increase (cpos,2);Tokens.FI(!linenum,!cpos));
"while"		 =>(accum:= ("WHILE \"while\"",(!linenum),(!cpos))::(!accum); increase (cpos,5);Tokens.WHILE(!linenum,!cpos));
"do"	  	 =>(accum:= ("DO \"do\"",(!linenum),(!cpos))::(!accum); increase (cpos,2);Tokens.DO(!linenum,!cpos));
"od"		 =>(accum:= ("OD \"od\"",(!linenum),(!cpos))::(!accum); increase (cpos,2);Tokens.OD(!linenum,!cpos));
"procedure"	  	 =>(accum:= ("PROCEDURE \"procedure\"",(!linenum),(!cpos))::(!accum); increase (cpos,9);Tokens.PROCEDURE(!linenum,!cpos));
"print"	 	 =>(accum:= ("PRINT \"print\"",(!linenum),(!cpos))::(!accum); increase (cpos,5);Tokens.PRINT(!linenum,!cpos));
"read"		 =>(accum:= ("READ \"read\"",(!linenum),(!cpos))::(!accum); increase (cpos,4);Tokens.READ(!linenum,!cpos));
"call"		 =>(accum:= ("CALL \"call\"",(!linenum),(!cpos))::(!accum); increase (cpos,4);Tokens.CALL(!linenum,!cpos));


"inverse"		 =>(accum:= ("INVERSE \"inverse\"",(!linenum),(!cpos))::(!accum); increase (cpos,7);Tokens.INVERSE(!linenum,!cpos));

".+."			   =>(accum:= ("RATPLUS \".+.\"",(!linenum),(!cpos))::(!accum);increase (cpos,3);Tokens.RATPLUS(!linenum,!cpos));
".-."			   =>(accum:= ("RATMINUS \".-.\"",(!linenum),(!cpos))::(!accum);increase (cpos,3);Tokens.RATMINUS(!linenum,!cpos));
".*."			   =>(accum:= ("RATMULT \".*.\"",(!linenum),(!cpos))::(!accum);increase (cpos,3);Tokens.RATMULT(!linenum,!cpos));
"./."			   =>(accum:= ("RATDIV \"./.\"",(!linenum),(!cpos))::(!accum);increase (cpos,3);Tokens.RATDIV(!linenum,!cpos));
"."			   =>(accum:= ("DECI \".\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.DECI(!linenum,!cpos));
"make_rat"	 	 =>(accum:= ("MAKERAT \"make_rat\"",(!linenum),(!cpos))::(!accum); increase (cpos,8);Tokens.MAKERAT(!linenum,!cpos));
"rat"	 	 =>(accum:= ("RAT \"rat\"",(!linenum),(!cpos))::(!accum); increase (cpos,3);Tokens.RAT(!linenum,!cpos));
"showRat"	 	 =>(accum:= ("SHOWRAT \"showRat\"",(!linenum),(!cpos))::(!accum); increase (cpos,7);Tokens.SHOWRAT(!linenum,!cpos));
"showDecimal"		   =>(accum:= ("SHOWDECI \"showDecimal\"",(!linenum),(!cpos))::(!accum); increase (cpos,11);Tokens.SHOWDECI(!linenum,!cpos));
"fromDecimal"		   =>(accum:= ("FROMDECI \"fromDecimal\"",(!linenum),(!cpos))::(!accum); increase (cpos,11);Tokens.FROMDECI(!linenum,!cpos));
"toDecimal"		   =>(accum:= ("TODECI \"toDecimal\"",(!linenum),(!cpos))::(!accum); increase (cpos,9);Tokens.TODECI(!linenum,!cpos));

"-"			   =>(accum:= ("INTMINUS \"-\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.INTMINUS(!linenum,!cpos));
"*"			   =>(accum:= ("INTMULT \"*\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.INTMULT(!linenum,!cpos));
"/"			   =>(accum:= ("INTDIV \"/\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.INTDIV(!linenum,!cpos));
"%"	 	 =>(accum:= ("MOD \"%\"",(!linenum),(!cpos))::(!accum); increase (cpos,1);Tokens.MOD(!linenum,!cpos));

"~"   	   =>(accum:= ("NEGATE \"~\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.NEGATE(!linenum,!cpos));
"+"			   =>(accum:= ("PLUS \"+\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.PLUS(!linenum,!cpos));

"!"			   =>(accum:= ("NOT \"!\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.NOT(!linenum,!cpos));
"&&"			   =>(accum:= ("ANDALSO \"&&\"",(!linenum),(!cpos))::(!accum);increase (cpos,2);Tokens.ANDALSO(!linenum,!cpos));
"||"			   =>(accum:= ("ORELSE \"||\"",(!linenum),(!cpos))::(!accum);increase (cpos,2);Tokens.ORELSE(!linenum,!cpos));

"<"			   =>(accum:= ("LESSTHAN \"<\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.LESSTHAN(!linenum,!cpos));
">"			   =>(accum:= ("GREATERTHAN \">\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.GREATERTHAN(!linenum,!cpos));
"="			   =>(accum:= ("EQUAL \"=\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.EQUAL(!linenum,!cpos));
"<="			 =>(accum:= ("LESSTHANOREQUAL \"<=\"",(!linenum),(!cpos))::(!accum);increase (cpos,2);Tokens.LESSTHANOREQUAL(!linenum,!cpos));
">="			 =>(accum:= ("GREATERTHANOREQUAL \">=\"",(!linenum),(!cpos))::(!accum);increase (cpos,2);Tokens.GREATERTHANOREQUAL(!linenum,!cpos));
"<>"			 =>(accum:= ("NOTEQUAL \"<>\"",(!linenum),(!cpos))::(!accum);increase (cpos,2);Tokens.NOTEQUAL(!linenum,!cpos));

":="			 =>(accum:= ("ASSIGN \":=\"",(!linenum),(!cpos))::(!accum);increase (cpos,2);Tokens.ASSIGN(!linenum,!cpos));

"("			   =>(accum:= ("LPAREN \"(\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.LPAREN(!linenum,!cpos));
")"			   =>(accum:= ("RPAREN \")\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.RPAREN(!linenum,!cpos));
"{"			   =>(accum:= ("LCURLY \"{\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.LCURLY(!linenum,!cpos));
"}"			   =>(accum:= ("RCURLY \"}\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.RCURLY(!linenum,!cpos));

";"			   =>(accum:= ("SEMICOLON \";\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.SEMICOLON(!linenum,!cpos));
","			   =>(accum:= ("COMMA \",\"",(!linenum),(!cpos))::(!accum);increase (cpos,1);Tokens.COMMA(!linenum,!cpos));


%%%%%%%%%%%%%%%%%%%%

\r	=> (lex());
({alpha}({alpha}|{digit})*)  => (accum:= ("ID \""^yytext^"\"",(!linenum),(!cpos))::(!accum);increase (cpos,size(yytext));Tokens.ID(yytext,!linenum,!cpos));
({digit}({digit})*)  => (accum:= ("NUM \""^yytext^"\"",(!linenum),(!cpos))::(!accum);increase (cpos,size(yytext));Tokens.NUM(yytext,!linenum,!cpos));

.		=>(error (yytext,!linenum,!cpos);increase (cpos,size(yytext));
					lex());
