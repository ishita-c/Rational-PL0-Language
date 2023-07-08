all:
	ml-lex rational.lex
	@echo  "\n\nLoading yacc file ..."
	ml-yacc rational.yacc
	@echo  "Yacc file generated\n\n"
	rlwrap sml;

clean:
	@echo  "Cleaning up ..."
	rm -f rational.lex.sml rational.yacc.sml rational.yacc.desc rational.yacc.sig
	@echo  "Cleaned up\n\n"