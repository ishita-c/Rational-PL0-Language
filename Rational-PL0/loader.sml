CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

(* use bigint structure and rational structure *)

use "bigint.sml";
use "rational.sml";
(* open Rational; *)
use "pl0_ast.sml";
use "pl0_evaluator.sml";
use "pl0.yacc.sig";
use "pl0.yacc.sml";
use "pl0.lex.sml";

use "load_pl0.sml";
open Evaluator;
(* generateAST("input.txt"); *)

fun interpret(inputFile: string, outputFile: string) = 
            let 
                val outs = TextIO.openOut(outputFile)
            in
                case evaluate(generateAST(inputFile)) of
                    RatVal x => TextIO.output(outs,Rational.showDecimal(x));
                TextIO.closeOut outs

            end;      

Control.Print.printLength := 2000; (* set printing parameters so that *)
Control.Print.printDepth := 2000; (* we’ll see all details *)
Control.Print.stringDepth := 2000;

