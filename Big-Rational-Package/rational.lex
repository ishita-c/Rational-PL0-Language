structure Tokens= Tokens
  type pos =int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token
  val pos = ref 1
  val col = ref 1
  val index = ref 0
  val eof = fn () => (print("]\n");Tokens.EOF(!pos, !pos))
  fun show(x:string):int = 
    if(!index < 0) then(0)
    else
      if (!index = 0) then (index := (!index) +1; print("["^x);0)
      else (print(", "^x);0)

  %%

%header (functor RationalLexFun(structure Tokens:Rational_TOKENS));
digit=[0-9];
%%

\n  => (pos := (!pos) + 1; col := 1; lex());
" " => (col :=(!col)+1;lex());
{digit}+ => (show("NUM \""^yytext^"\"");col :=(!col)+size(yytext);Tokens.NUM(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),!pos, !pos));
"+" => (show("PLUS\"+\"");col :=(!col)+1;Tokens.PLUS(!pos,!pos));
"-" => (show("MINUS\"-\"");col :=(!col)+1;Tokens.MINUS(!pos,!pos));
"*" => (show("MULT\"*\"");col :=(!col)+1;Tokens.MULT(!pos,!pos));
"/" => (show("DIV\"/\"");col :=(!col)+1;Tokens.DIV(!pos,!pos));
"." => (show("DECI\".\"");col :=(!col)+1;Tokens.DECI(!pos,!pos));
"~" => (show("UNARYMINUS\"~\"");col :=(!col)+1;Tokens.UNARYMINUS(!pos,!pos));
")" => (show("RPAREN \")\"");col :=(!col)+1;Tokens.RPAREN(!pos,!pos));
"(" => (show("LPAREN \"(\"");col :=(!col)+1;Tokens.LPAREN(!pos,!pos));

. => (lex());