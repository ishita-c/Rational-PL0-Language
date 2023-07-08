structure PL0LrVals = PL0LrValsFun(structure Token = LrParser.Token)
structure PL0Lex = PL0LexFun(structure Tokens = PL0LrVals.Tokens);
structure PL0Parser =
      Join(structure LrParser = LrParser
               structure ParserData = PL0LrVals.ParserData
               structure Lex = PL0Lex)
val parsingerror:bool ref=ref false;
val parseroutput:string ref=ref "";

fun helper (nil) = ()
| helper (hd::nil : (string * int * int) list) = (print (#1 (hd)))
| helper (hd::tl : (string * int * int) list) = (print ( (#1 (hd)) ^ ","); helper (tl));

fun helper_detailed (nil) = ()
| helper_detailed (hd::nil : (string * int * int) list) = print ( "->"^(#1 (hd)) ^ "," ^ Int.toString(#2 (hd)) ^ "," ^ Int.toString(#3 (hd)) ^ "," ^ "<-")
| helper_detailed (hd::tl : (string * int * int) list) = (print ( "->"^(#1 (hd)) ^ "," ^ Int.toString(#2 (hd)) ^ "," ^ Int.toString(#3 (hd)) ^ "," ^ "<-"); helper_detailed (tl));

fun printList (sList : (string * int * int) list ref) = (
  let val x = List.rev (!sList) in (
    print ("[");
    helper (x);
    print ("]\n")
  )
  end
);


fun printList_detailed (sList : (string * int * int) list ref) = (
  let val x = List.rev (!sList) in (
    print ("[");
    helper_detailed (x);
    print ("]\n")
  )
  end
);

fun invoke lexstream =
	let 
	fun print_error (s, pos:int, cpos :int) =
                (if ((!parsingerror) = false) then (printList (PL0Lex.UserDeclarations.accum);
                parsingerror := true;
                TextIO.output(TextIO.stdOut,"Syntax Error:" ^ (Int.toString pos) ^ ":" ^ (Int.toString cpos) ^ ":" ^ s ^ "\n")) else ()) 
        in
            PL0Parser.parse(0,lexstream,print_error,())
        end

fun stringToLexer file =
    let 
    	val input = TextIO.openIn file
    	val str : string = TextIO.inputAll input
    	val done = ref false
        val lexer=  PL0Parser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
    (
        (* print("Inside\n");
        lexer;
        print("Done\n"); *)
        lexer
    )
    end 
        
fun parse (lexer) =
    let val dummyEOF = PL0LrVals.Tokens.EOF(0,0)
        val (result, lexer) = invoke lexer
        val (nextToken, lexer) = PL0Parser.Stream.get lexer
    in
    (
        print("Inside\n");
        (if PL0Parser.sameToken(nextToken, dummyEOF) then result
        else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result))
    )
    end

fun lexerOutput (x :string) = 
    let val mylexer = stringToLexer (x)
    in (
        PL0Lex.UserDeclarations.linenum := 1 ;
        PL0Lex.UserDeclarations.cpos := 1 ;
        PL0Lex.UserDeclarations.accum := [];
        print("Parse called\n");
        parse (mylexer);
        print("printList called\n");
        printList (PL0Lex.UserDeclarations.accum);
        parsingerror:=false
        (* print(!parseroutput) *)
	) end


(* fun parseFile (x:string) = parse (stringToLexer(x)) *)
fun parseFile (x:string) = stringToLexer(x)

fun generateAST(x:string) = 
    let 
        val mylexer = stringToLexer (x)
        val (result, lexer) = invoke mylexer
    in 
    (
        result
    )
    end
