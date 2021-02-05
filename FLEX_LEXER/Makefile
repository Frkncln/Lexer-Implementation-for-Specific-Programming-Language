
#If you want to add filename , compile with:  'make FILE=filename.g++'
#else just 'make'

all:  getf


getf: gpp_lexer.c
	gcc gpp_lexer.c -o a.out	


gpp_lexer.c: gpp_lexer.l
	flex -o gpp_lexer.c gpp_lexer.l
 

