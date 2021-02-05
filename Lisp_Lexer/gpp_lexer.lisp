
; KEYWORDS
(defvar KW_AND "and")
(defvar KW_OR "or")
(defvar KW_NOT "not")
(defvar KW_EQUAL "equal")
(defvar KW_LESS "less")
(defvar KW_NIL "nil")
(defvar KW_LIST "list")
(defvar KW_APPEND "append")
(defvar KW_CONCAT "concat")
(defvar KW_EXIT "exit")
(defvar KW_LOAD "load")
(defvar KW_DISP "disp")
(defvar KW_TRUE "true")
(defvar KW_SET "set")
(defvar KW_DEFFUN "deffun")
(defvar KW_FOR "for")
(defvar KW_IF "if")
(defvar KW_FALSE "false")
(defvar OP_OC "\"")
(defvar OP_CC "\"")
(defvar OP_OP "(")
(defvar OP_CP ")")
(defvar OP_DBLMULT "**")
(defvar OP_COMMA ",")
(defvar OP_PLUS "+")
(defvar OP_MINUS "-")
(defvar OP_DIV "/")
(defvar OP_MULT "*")
(defvar COMMENT ";;")
(setq SEMICOLON ";")





(setq isKEYorID 0)
(setq cmcount 0)
(setq comline 0)
(setq opcount 0)
(setq DFA 0)
(setq isIdentifier 0)
(setq isDblmult 0)
(setq isMult 0)
(setq isComment 0)
(setq check 2)
(setq isInteger 0)
(setq isOperator 0)

(setq outfile "parsed_lisp.txt");output filename for parsing outputs
(setq token_key_id "")
(setq token_int "")
(setq token_operator "")
(setq token_mult "")
(setq token_comment "")
;;---------------------------------------------------------------------------------------------------------------------

;interpreter function
(defun gppinterpreter (linearg)
	(format t "filename = ~s" linearg )
	(format t "> " ) 
	
	(cond 
		((eq linearg nil)
			(setf comline 0)
			(format t "setted comline 0 because = ~s" linearg )
		)
		((> (length linearg) 0)
			(setf comline 1)
			(format t "setted comline 1 because = ~s" linearg )
		)
	
	
	)
	(gpplexer linearg)
)

;lexer function
;gets filename
(defun gpplexer(filename )
	
	(cond
		((eq comline 1)
			
			(with-open-file (stream filename :if-does-not-exist :error)
				(do ((chr (read-char stream nil) (read-char stream nil))) ;reads char by char from file
					((or (null chr) (eq DFA 1)))
					
					(gpp_lexer_helper chr stream) ;specifies every chr according to rules, then sends it to lexer
				)	
			)
		)
	
		;terminal condition
		((eq comline 0)
			(block outer(loop
							 (setf check 1)
							(setq trmcom_new (read-line))
						(when (eq (length trmcom_new) 0) 	
							(return-from outer)
							)
							
							(setq trmcom_new (coerce trmcom_new 'list))
						
						(lexer_terminal trmcom_new)
			
			))
	
	)
))

(defun lexer_terminal(data)

			(block outer (loop for i from 0 to (- (length data) 1) ;reads char by char form string "data"
	    		do 
	    		(if (eq DFA 0)
	        		(progn	
	        			(setq chr (nth i data))
	        			
	        			(if (eq #\; chr)(progn
						(setf cmcount (+ cmcount 1))
						(if (eq 2 cmcount)(progn
	        					(setf token_comment (concatenate 'string token_comment(list chr)))
	        					(setf isComment 1)	
							(print_lexer token_comment)
							(setq cmcount 0)
							(return-from outer)
							)
						)
						

						)
							(gpp_lexer_helper chr data) 
							
					)

	        		)
			)
		))

)




;helper function (gpplexer)
(defun gpp_lexer_helper (chr stream);******************************************************lexer helper
	
	(cond
				
		;condition to check integer
		((digit-char-p chr)
			(setf token_int (concatenate 'string token_int (list chr))) ;add chr into token_int string
			(setf isInteger 1)
		
		)

		;condition to check  identifier
		((alpha-char-p chr)
			(setf token_key_id (concatenate 'string token_key_id (list chr))) ;adds chr into token_key_id string
			(setf isKEYorID 1)
			(setf isIdentifier 1)
		)

		((or (eq chr #\Space) (eq chr #\Tab) (eq chr #\Newline) (eq chr nil))
		
			(cond 
				((eq (length token_mult) 2) (setf isDblmult 1))
				((eq (length token_mult) 1) (setf isMult 1))
			)

			(cond
				((eq isInteger 1) (print_lexer token_int)) 
				((eq isKEYorID 1) (print_lexer token_key_id) ) 
				((or (eq isMult 1) (eq isDblmult 1) ) (print_lexer token_mult))	 	
			)				
		)
		((or (string-equal chr OP_PLUS) (string-equal chr OP_MINUS) (string-equal chr OP_DIV) 
			 (string-equal chr OP_OP) (string-equal chr OP_CP) (string-equal chr OP_OC) (string-equal chr OP_CC) )
				
			(cond
				((eq isInteger 1) (print_lexer token_int)) 
				((eq isIdentifier 1) (print_lexer token_key_id)	)	
			)		
						
			(setf isOperator 1)	
			(setf token_operator (concatenate 'string token_operator (list chr))) 
			(print_lexer token_operator) ;calls function to print operator 					

		)
		((string-equal chr OP_MULT)
			(setf token_mult (concatenate 'string token_mult (list chr))) 
		)
		((string-equal chr SEMICOLON)
			(setf token_comment (concatenate 'string token_comment(list chr))) ;adds chr to token_comment string
			
			(if (eq (length token_comment) 2) 
				(progn 
					(setf isComment 1)
					(print_lexer token_comment) 
					
					(setf stream (readfile stream)) 
					
				)
			)

		)			

	)

)


(defun readfile(stream)
	(loop
		(setf item (read-char stream )) ;reads char by char from file
			
		(when (eq item #\Newline) (return-from readfile stream)) ;when item is 'newline' return file stream
	)
)



;writefile function for adding items to file
(defun writeToFile (filename content)
  (with-open-file (stream  filename 
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create )
  (format stream content)
	(terpri stream)
  )


)


;prints keywords to terminal according to gpplexer function
(defun print_lexer (token)

	(cond
		
		((eq isOperator 1) 
			(cond
			
				((string-equal token OP_PLUS) (format t "OP_PLUS") (writeToFile outfile "OP_PLUS"))
				((string-equal token OP_MINUS) (format t "OP_MINUS")(writeToFile outfile "OP_MINUS"))
				((string-equal token OP_DIV) (format t "OP_DIV")(writeToFile outfile "OP_DIV") )	
				((string-equal token OP_OP) (format t "OP_OP")(writeToFile outfile "OP_OP") )
				((string-equal token OP_CP) (format t "OP_CP")(writeToFile outfile "OP_CP") )
				((and(string-equal token OP_OC)(= opcount 0) )
							
					(format t "OP_OC")(writeToFile outfile "OP_OC")
					(setf opcount 1)
				) 
					
				((and(string-equal token OP_CC)(= opcount 1))
					(format t "OP_CC")(writeToFile outfile "OP_CC")
						(setf opcount 0) 
				)
					
				
				  	
			)
				
			(setf isOperator 0)
			(setf token_operator "")
		)

		;if token is dblmult operator 
		((and (eq isDblmult 1) (string-equal token OP_DBLMULT) )
			(format t "OP_DBLMULT")
			(writeToFile outfile "OP_DBLMULT") 
			(setf isDblmult 0)
			(setf token_mult "")	
		)

		;if token is mult operator 
		((and (eq isMult 1) (string-equal token OP_MULT) )
			(format t "OP_MULT") 
			(writeToFile outfile "OP_MULT")
			(setf isMult 0)
			(setf token_mult "")	
		)

		;if token is reserved keyword 
		((eq isKEYorID 1)
			(setq syntax_check (subseq token 0 (- (length token) 1))) ;string to check syntax error for reserved words

			(cond 
				
				((or (string-equal syntax_check KW_AND) (string-equal syntax_check KW_OR) (string-equal syntax_check KW_NOT) 
					 (string-equal syntax_check KW_EQUAL) (string-equal syntax_check KW_LESS) (string-equal syntax_check KW_NIL) 
					 (string-equal syntax_check KW_LIST) (string-equal syntax_check KW_APPEND) (string-equal syntax_check KW_CONCAT) 
					 (string-equal syntax_check KW_SET) (string-equal syntax_check KW_DEFFUN) (string-equal syntax_check KW_FOR) 
					 (string-equal syntax_check KW_IF) (string-equal syntax_check KW_EXIT) (string-equal syntax_check KW_LOAD) 	
					 (string-equal syntax_check KW_DISP) (string-equal syntax_check KW_TRUE) (string-equal syntax_check KW_FALSE) )

					(format t "SYNTAX ERROR ~S cant be tokenize" token)
					(setf DFA 1) ;terminate program
				)

					
				((string-equal token KW_CONCAT) (format t "KW_CONCAT") (writeToFile outfile "KW_CONCAT "))
				((string-equal token KW_SET) (format t "KW_SET") (writeToFile outfile "KW_SET "))
				((string-equal token KW_DEFFUN) (format t "KW_DEFFUN") (writeToFile outfile "KW_DEFFUN "))	
				((string-equal token KW_FOR) (format t "KW_FOR") (writeToFile outfile "KW_FOR "))
				((string-equal token KW_IF) (format t "KW_IF") (writeToFile outfile "KW_IF "))
				((string-equal token KW_EQUAL) (format t "KW_EQUAL") (writeToFile outfile "KW_EQUAL "))
				((string-equal token KW_LESS) (format t "KW_LESS") (writeToFile outfile "KW_LESS "))
				((string-equal token KW_NIL) (format t "KW_NIL") (writeToFile outfile "KW_NIL ") )
				((string-equal token KW_LIST) (format t "KW_LIST") (writeToFile outfile "KW_LIST "))
				((string-equal token KW_APPEND) (format t "KW_APPEND") (writeToFile outfile "KW_APPEND "))
				((string-equal token KW_EXIT) (format t "KW_EXIT") (writeToFile outfile "KW_EXIT "))
				((string-equal token KW_LOAD) (format t "KW_LOAD") (writeToFile outfile "KW_LOAD "))	
				((string-equal token KW_DISP) (format t "KW_DISP") (writeToFile outfile "KW_DISP "))
				((string-equal token KW_TRUE) (format t "KW_TRUE") (writeToFile outfile "KW_TRUE "))
				((string-equal token KW_FALSE) (format t "KW_FALSE") (writeToFile outfile "KW_FALSE "))
				((string-equal token KW_AND) (format t "KW_AND") (writeToFile outfile "KW_AND "))
				((string-equal token KW_OR) (format t "KW_OR") (writeToFile outfile "KW_OR "))
				((string-equal token KW_NOT) (format t "KW_NOT") (writeToFile outfile "KW_NOT "))
				
				(t 	
					(setf token (coerce token 'list))
					(cond ((or (eq (car token) #\0 )(eq (car token) #\1 )(eq (car token) #\2 )(eq (car token) #\3 )
								(eq (car token) #\4 )(eq (car token) #\5 )(eq (car token) #\6 )
								(eq (car token) #\7 )(eq (car token) #\8 )(eq (car token) #\9 ))

								(format t "INVALID ınput (zero leading)")(writeToFile outfile "INVALID ınput Identifier ") 
							)
					)(format t "IDENTIFIER") (writeToFile outfile "IDENTIFIER "))
			)

			(setf isKEYorID 0)
			(setf isIdentifier 0)
			(setf token_key_id "")
		)

		;if token is comment 
		((eq isComment 1)
			(format t "COMMENT")
			(writeToFile outfile "COMMENT ")
			(setf isComment 0)
			(setf token_comment "")
		)

		;if token is identifier t
		((eq isIdentifier 1)
			(setf token (coerce token 'list))
			(cond
				( (> (length token) 1)   
					(cond ((or (eq (car token) #\0 )(eq (car token) #\1 )(eq (car token) #\2 )(eq (car token) #\3 )
								(eq (car token) #\4 )(eq (car token) #\5 )(eq (car token) #\6 )
								(eq (car token) #\7 )(eq (car token) #\8 )(eq (car token) #\9 ))

								(format t "INVALID ınput (zero leading)")(writeToFile outfile "INVALID ınput Identifier ") 
							)
					)
				
				
				) ;integer value leadings 0
				(t (format t "IDENTIFIER")	(writeToFile outfile "IDENTIFIER ")) ;ıdentifier
			)
			
			(setf isIdentifier 0)
			(setf isKEYorID 0)
			(setf token_key_id "")
		)

		((eq isInteger 1) 
			(format t "value içinde")
			(setf token (coerce token 'list)) 

			(cond
				((and (> (length token) 1) (eq (car token) #\0 ) ) (format t "INVALID VALUE (zero leading)")(writeToFile outfile "INVALID VALUE (zero leading)") ) 
				;integer value leadings 0
				(t (format t "VALUE") (writeToFile outfile "VALUE ")) ;integer value
			)

			(setf isInteger 0)
			(setf token_int "")
		)

	)
	(terpri)
	(format t "PRINTLEXER LAST" )
							
	(terpri) ;to print from new line
)

;TEST 

(defun testLexicalAnalyzer ()
	
	(print (type-of (nth 0 *args*)))
	(format t "in TERMINAL. you writed =")
	
	(print (nth 0 *args*))
	
	(terpri)
	
	
	
	(format t "JUST PRESS ENTER  REPL in TERMINAL.")
	(terpri)
	(format t "If you want get test from file \nENTER 'filename.g++' for EXECUTE FILE")
	(terpri)
	(format t "DON'T FORGET! ENTER .g++ filetype ")
	(terpri)


	(gppinterpreter (nth 0 *args*))
)


(testLexicalAnalyzer)

