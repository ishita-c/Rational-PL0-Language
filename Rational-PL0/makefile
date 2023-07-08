all:
	ml-lex pl0.lex
	@echo  "\n\nLoading yacc file ..."
	ml-yacc pl0.yacc
	@echo  "Yacc file generated\n\n"
	rlwrap sml use "loader.sml";

clean:
	@echo  "Cleaning up ..."
	rm -f pl0.lex.sml pl0.yacc.sml pl0.yacc.desc pl0.yacc.sig
	@echo  "Cleaned up\n"
