
%{

#include "stdio.h"

%}


/*rule definitions of G++ lexer*/
%%
"and"		fprintf(yyout,"KW_AND\n");		printf("KW_AND\n");				/*and */
"or"			fprintf(yyout,"KW_OR\n");		printf("KW_OR\n");		 		/*or */
"not"		fprintf(yyout,"KW_NOT\n");		printf("KW_NOT\n");		 	/*nto */
"equal"		fprintf(yyout,"KW_EQUAL\n");		printf("KW_EQUAL\n");		 	/*equal */
"less"		fprintf(yyout,"KW_LESS\n");		printf("KW_LESS\n");		 	/*less */
"nil"		fprintf(yyout,"KW_NIL\n");		printf("KW_NIL\n");		 	/*nil */
"list"		fprintf(yyout,"KW_LIST\n");		printf("KW_LIST\n");		 		/*list */
"append"		fprintf(yyout,"KW_APPEND\n");		printf("KW_APPEND\n");		 	/*append */
"concat"		fprintf(yyout,"KW_CONCAT\n");		printf("KW_CONCAT\n");		 	/*concat */
"set"		fprintf(yyout,"KW_SET\n");		printf("KW_SET\n");		 	/*set */
"deffun"		fprintf(yyout,"KW_DEFFUN\n");		printf("KW_DEFFUN\n");		 	/*deffun */
"for"		fprintf(yyout,"KW_FOR\n");		printf("KW_FOR\n");				/*for */
"if"			fprintf(yyout,"KW_IF\n");		printf("KW_IF\n");		 		/*if */
"exit"		fprintf(yyout,"KW_EXIT\n");		printf("KW_EXIT\n");		 	/*exit */
"load"		fprintf(yyout,"KW_LOAD\n");		printf("KW_LOAD\n");		 	/*load */
"disp"		fprintf(yyout,"KW_DISP\n");		printf("KW_DISP\n");		 	/*disp */
"true"		fprintf(yyout,"KW_TRUE\n");		printf("KW_TRUE\n");		 	/*true */
"false"		fprintf(yyout,"KW_FALSE\n");		printf("KW_FALSE\n");			/*false */
"+"			fprintf(yyout,"OP_PLUS\n");		printf("OP_PLUS\n");		 	/*plus */
"-"			fprintf(yyout,"OP_MINUS\n");		printf("OP_MINUS\n");			/*minus */
"/"			fprintf(yyout,"OP_DIV\n");		printf("OP_DIV\n");		 	/*division */
"*"			fprintf(yyout,"OP_MULT\n");		printf("OP_MULT\n");		 	/*multiplier */
"**"			fprintf(yyout,"OP_DBLMULT\n");	printf("OP_DBLMULT\n");		 /*double multiplier */
","			fprintf(yyout,"OP_COMMA\n");		printf("OP_COMMA\n");		 	/*comma */
"\""			fprintf(yyout,"OP_OC\n");		printf("OP_OC\n");		 		/*open quote */
"`"			fprintf(yyout,"OP_CC\n");		printf("OP_CC\n");		 		/*close quote  */
"("			fprintf(yyout,"OP_OP\n");		printf("OP_OP\n");		 		/*open paranthesis */
")"			fprintf(yyout,"OP_CP\n");		printf("OP_CP\n");		 		/*close paranthesis */




";;"+([. + - / *]|[A-Za-z]|[0-9]|[ \t])* 	fprintf(yyout,"COMMENT\n");printf("COMMENT\n");		/* expression for comment token*/ 

";"+[a-zA-Z0-9]+			fprintf(yyout,"ERROR INVALID-TOKEN\n");printf("ERROR INVALID-TOKEN\n");

[a-zA-Z0-9]+";"+			fprintf(yyout,"ERROR INVALID-TOKEN\n");printf("ERROR INVALID-TOKEN\n");

[a-zA-Z0-9]+";"+[a-zA-Z0-9]+			fprintf(yyout,"ERROR INVALID-TOKEN\n");printf("ERROR INVALID-TOKEN\n");		

[0]					fprintf(yyout,"VALUE\n");printf("VALUE\n");

[1-9][0-9]* 			fprintf(yyout,"VALUE\n");printf("VALUE\n");							/*regular expression for invalid integer value */  

[0-9][0-9]* 			fprintf(yyout,"ERROR-INVALID VALUE (zero leading)\n");printf("ERROR-INVALID VALUE (zero leading)\n");	
								/*regular expression for 0 digit */ 
[0-9]+[a-zA-Z]+[a-zA-Z0-9]+			fprintf(yyout,"ERROR INVALID-IDENTIFIER\n");printf("ERROR INVALID-IDENTIFIER\n");		


[a-zA-Z][_a-zA-Z0-9]* 	fprintf(yyout,"IDENTIFIER\n");printf("IDENTIFIER\n");		
	

^\n               yyterminate();  /* terminating for twice entering blank space*/

[ \t]+  ;	
[ \n]  ;

.	fprintf(yyout,"ERROR unexpected input\n");printf("ERROR unexpected input\n");

%%


int main(int argc, char *argv[])
{
	extern FILE *yyin, *yyout; 
  
   
    
    
    if(argc > 1 ){
    	
    	yyin = fopen(argv[1], "r"); 
    
    }
    else{
    		
    		printf("There is no file name on commandline \nEnter your input here please\n ENTERED   for REPL mode\nEnter twice for exit\n"); 
    		
    }
    	
   
    yyout = fopen("parsed_cpp.txt", "w"); 
  
    yylex();

   return 0;

}
int yywrap(){return(1);}




