(load "sexpr-parser.scm")
(load "tag-parser.scm")
(load "semantic-analyzer.scm")
(load "strings.scm")

(define list->set
	(lambda(s)
		(fold-right 
			(lambda(a s)
				(if (ormap (lambda(si) (equal? a si)) s)
					s
					(cons a s)))
			`()
				s)
	)
)

(define fraction?
	(lambda(x)
		(and (number? x) (not (= (denominator x) 1)))
	)
)

(define get-all-subelements-list ;recieves "1" list (or a pair), and retrieves all subelements, accumulator_list = '() at start
	(lambda(lst accumulator_list)
				(if (list? (car lst)) 
						 `(,(car lst) ,(cdr lst) ,@(get-all-subelements-list (car lst) accumulator_list) ,@(get-all-subelements (cdr lst) accumulator_list) ,lst)
						 `(,@(get-all-subelements (car lst) accumulator_list) ,@(get-all-subelements (cdr lst) accumulator_list) ,lst)
				)
	)
)

(define get-all-subelements 	;recives "1" list of items and returns all the subelements of all items
	(lambda(lst accumulator_list)
		(cond
			((null? lst) `(() ,accumulator_list))
			((eq? (void) lst) `(,(void) ,accumulator_list))
			((or (list? lst) (pair? lst)) (get-all-subelements-list lst accumulator_list))
			((const? lst) 
				(if (fraction? lst) `(,(numerator lst) ,(denominator lst) ,lst ,@accumulator_list)
					`(,lst ,@accumulator_list)))
			((symbol? lst) `(,lst ,(symbol->string lst) ,@accumulator_list ))
			((vector? lst) `(,@(const-list->sorted-set (vector->list lst)) ,@accumulator_list ,lst))
		)
))

(define contains?		;;checks if data1 is contained in data2
	(lambda(data1 data2)
		(if (equal? data1 data2) #t
			(cond
        		((or (null? data2) (eq? data2 "") (eq? data2 '#())) #f)
				((fraction? data2) (or (equal? data1 (numerator data2)) (equal? data1 (denominator data2))))
				((symbol? data2) (equal? data1 (symbol->string data2)))
				((or (char? data1) (and (number? data1) (not (fraction? data1))) (boolean? data1) (eq? (void) data1) (null? data1)) #t)
				((or (pair? data2) (list? data2) (vector? data2))
					(if (vector? data2) (or (contains? data1 (car (vector->list data2))) (contains? data1 (cdr (vector->list data2)))) 
						(or (contains? data1 (car data2)) (contains? data1 (cdr data2)))))
        (else #f)
			)
		)
	)
)

(define get-all-list-subelements	;recives a list of "constants list" and returns their subelements
	(lambda(const-lst accumulator_list)
		(if (null? const-lst) accumulator_list 
				`(,@(get-all-subelements (car const-lst) accumulator_list) ,@(get-all-list-subelements (cdr const-lst) accumulator_list))
		)
	)
)

(define const-list->sorted-set		;recieves a list of items and returns the topologically sorted set of every item and subitem (make sure to combine the consts list from the whole program before calling this)
	(lambda(lst)
		(sort contains? (list->set (append `(() ,(void) #t #f) (get-all-list-subelements lst '()))))
	)
)	

(define find-all-consts1 ;;gets all the consts from a single expression
	(lambda (lst acc) 
	    (if (and (list? lst) (not (null? lst)) (equal? (car lst) 'const))
	    	(append acc  (cdr lst))
	         (if (and (not (null? lst)) (list? lst))
			 	`(,@(find-all-consts1 (car lst) acc) ,@(find-all-consts1 (cdr lst) acc))
			     acc
			  )
	    )
        
	)
)

(define find-all-consts ;wraps finds-all-consts1 to make us able to use map
	(lambda(expr)
		(find-all-consts1 expr '())
		))


(define file->string   ;recieves an input file and returns a string representing all the sexprs in it
	(lambda (in-file)
		(let ((in-port (open-input-file in-file)))
			(letrec ((run
 				(lambda ()
					(let ((ch (read-char in-port)))
						(if (eof-object? ch)
							(begin
								(close-input-port in-port)
							'())
				(cons ch (run)))))))
				(list->string
				(run))))))


(define str->sexpr (lambda(str)  ;reads a string representing sexprs and returns a list of sexpers (unparsed)
	(<sexpr-star> (string->list str) (lambda (x y) x) (lambda () 'fail))
))

(define sexpr-list->parsed-sexpr-list   ;recieves a list of unparsed sexprs and returns a parsed list of said sexprs
	(lambda(sexpr-list)
		(map (lambda (x) (annotate-tc (pe->lex-pe (box-set (remove-applic-lambda-nil  (parse x)))))) sexpr-list) ;; add tc-applic back later
))

(define find-all-consts-list ;;recieves a list of parsed sexprs and gets all the consts from all list members
	(lambda(expr-list)
		(apply append (map find-all-consts expr-list))
	)
)

(define file->sorted-const-set ;recieves file name (as string) and returns he sorted constant-set
	(lambda(input_file)
		(const-list->sorted-set (find-all-consts-list (sexpr-list->parsed-sexpr-list (str->sexpr (file->string input_file)))))
	)
)

(define file->parsed-sexpr-list
	(lambda(input_file)
		(sexpr-list->parsed-sexpr-list (str->sexpr (file->string input_file)))
	)
)

(define get-vector-adresses	 ;send the vector after doing (vecotor->list)
	(lambda(vector sorted-set adress-list)
		(if (null? vector) 
				adress-list
				(get-vector-adresses (cdr vector) sorted-set `(,@adress-list ,(calc-index (car vector) sorted-set 0)))
		)

))

(define calc-index
	(lambda(item lst index)
		(if (null? lst) 'error-index-not-found
			(if (equal? item (car lst)) index
					(calc-index item (cdr lst) (+ 1 index))
			)
		)
	)
)

(define build-const-table
	(lambda(sorted-set const-table remainder-list)
		(if (null? remainder-list) 
				const-table
				(let ((item (car remainder-list)))
					(if (or (and (number? item) (not (fraction? item))) (eq? (void) item) (boolean? item) (char? item) (null? item)) 
							(build-const-table sorted-set (append const-table `((,(length const-table) ,item ,item))) (cdr remainder-list))
							(cond
								((string? item)
									(build-const-table sorted-set (append const-table `((,(length const-table) ,item ,(string-length item)))) (cdr remainder-list))
								)

								((symbol? item)
									(build-const-table sorted-set 
										(append const-table 
											`((,(length const-table) ,item ,(calc-index (symbol->string item) sorted-set 0)))) (cdr remainder-list))
								)								

								((or (pair? item) (list? item)) 
									(build-const-table 
										sorted-set
										(append
											const-table 
											`((,(length const-table) ,item (,(calc-index (car item) sorted-set 0)  ,(calc-index (cdr item) sorted-set 0))))
										) 
										(cdr remainder-list)
									)
								)
								((symbol? item) (build-const-table sorted-set const-table (cdr remainder-list)))
								((fraction? item) 
									(build-const-table
										sorted-set
										(append
											const-table
											`((,(length const-table) ,item (,(calc-index (numerator item) sorted-set 0)  ,(calc-index (denominator item) sorted-set 0))))
										)
											(cdr remainder-list)
									)
									
								)

								((vector? item)
									(build-const-table
										sorted-set
										(append 
											const-table 
											`((,(length const-table) ,item  (,@(get-vector-adresses (vector->list item) sorted-set '()))))
										)
										(cdr remainder-list)
									)
								)
							)
					)
				)
			)
		)
)	


(define file->const-table
	(lambda(input_file)
		(let ((sorted-set (file->sorted-const-set input_file)))
			(build-const-table sorted-set '() sorted-set)
		)
	)
)

(define const-table->code
	(lambda(const-table res-str)
		(if (null? const-table) 
				res-str
				(let (
						(index (caar const-table))
						(item (cadar const-table))
						(content (caddar const-table))
					 )
						(cond
							((null? item) 
									(const-table->code 
										(cdr const-table)
										(string-append
											res-str
											"sobNil:\n\tdq SOB_NIL\n"
										)
									)
							)

							((eq? (void) item) 
									(const-table->code 
										(cdr const-table)
										(string-append
											res-str
											"sobVoid:\n\tdq SOB_VOID\n"
										)
									)
							)

							((symbol?  item)  ;;implemented in another way
								(const-table->code 
									(cdr const-table)
									(string-append
										res-str
										(format "sobSymbol~a:\n" (calc-index item sorted-set 0))
										"dq 0\n"
									)
								)
							)

							((and (number? item) (not (fraction? item)))
									(const-table->code 
										(cdr const-table)
										(string-append
											res-str
											(format "sobInt~a:\n\t" (calc-index item sorted-set 0))
											(format "dq MAKE_LITERAL(T_INTEGER, ~a)\n" content)
										)
									)
							)

							((and (boolean? item) item)			;boolean true
									(const-table->code 
										(cdr const-table)
										(string-append
											res-str
											"sobBoolTrue:\n\tdq SOB_TRUE\n"
										)
									)
							)

							((and (boolean? item) (not item))			;boolean false
									(const-table->code 
										(cdr const-table)
										(string-append
											res-str
											"sobBoolFalse:\n\tdq SOB_FALSE\n"
										)
									)
							)

							((char? item) 
									(const-table->code 
										(cdr const-table)
										(string-append
											res-str
											(format "sobChar~a:\n\t" index)
											(format "dq MAKE_LITERAL(T_CHAR, ~a)\n" (char->integer content))
										)
									)
							)

							((string? item) 
									(const-table->code 
										(cdr const-table)
										(string-append
											res-str
											(format "sobString~a:\n\t" index)
											(if (eq? "" item) "dq MAKE_LITERAL(T_STRING, 0)\n"
												(format "MAKE_LITERAL_STRING \"~a\"\n" item)
											)
										)
									)
							)

							((pair? item) 
									(const-table->code 
										(cdr const-table)
										(string-append
											res-str
											(format "sobPair~a:\n\t" index)
											(format "dq MAKE_LITERAL_PAIR(~a, ~a)\n" (get-label (car item) (car content)) (get-label (cdr item) (cadr content)))
										)
									)
							)

							((vector? item) 
									(const-table->code 
										(cdr const-table)
										(string-append
											res-str
											(format "sobVector~a:\n\t" index)
											(if (eq? '() (vector->list item)) "dq MAKE_LITERAL(T_VECTOR, 0)\n"
												(format "MAKE_LITERAL_VECTOR ~a~a\n" 
													(get-label (car (vector->list item)) (car content)) 
													(make-vector-address-string (cdr (vector->list item)) (cdr content) "")
												)
											)

										)
									)
							)

							((fraction? item) 
									(const-table->code 
										(cdr const-table)
										(string-append
											res-str
											(format "sobFraction~a:\n\t" index)
											(format "dq MAKE_LITERAL_FRACTION(sobInt~a, sobInt~a)\n" (calc-index (numerator item) sorted-set 0) (calc-index (denominator item) sorted-set 0))
										)
									)
							)


						)
				)
		)
	)
)

(define make-symbol-table-strings
	(lambda(sorted-set remainder-list)
		(if (null? remainder-list) ""
			(if (symbol? (car remainder-list))
				(string-append
					(format "mov r10, sobString~a\n" (calc-index (symbol->string (car remainder-list)) sorted-set 0))
					"MAKE_LITERAL_SYMBOL r10, r9\n"
					(format "mov [sobSymbol~a], r9\n" (calc-index (car remainder-list) sorted-set 0))
					"sym_tab_append r9\n"
					(make-symbol-table-strings sorted-set (cdr remainder-list))
				)
				(make-symbol-table-strings sorted-set (cdr remainder-list))
			)
		)
	)
)
	
(define make-vector-address-string
	(lambda(vector-as-list addresses res-str)
		(if (null? vector-as-list) res-str
			(make-vector-address-string (cdr vector-as-list) (cdr addresses)
				(string-append
					res-str
					" ,"
					(get-label (car vector-as-list) (car addresses))
				)
			)
	))
)

(define get-label
	(lambda(item address)
		(cond
				((and (number? item) (not (fraction? item))) (format "sobInt~a" address))
				((null? item) "sobNil")
				((eq? #t item) "sobBoolTrue")
				((eq? #f item) "sobBoolFalse")
				((eq? (void) item) "sobVoid")
				((char? item) (format "sobChar~a" address))
				((string? item) (format "sobString~a" address))
				((pair? item) (format "sobPair~a" address))
				((vector? item) (format "sobVector~a"  address))
				((fraction? item) (format "sobFraction~a"  address))
				((symbol? item) (format "sobSymbol~a" address)) ;placeholder - to be implemented later
		)

	)
)

(define or-helper
	(lambda (args lst-length curr-int lambda-level prev-n curr-n )	
		(string-append
			(if (> lst-length 1)
				(string-append
					(codegen-helper (car args) lambda-level prev-n curr-n  sorted-set)
					"cmp rax, sobBoolFalse\n"
					(format "jne L_or_exit_~a\n" curr-int)
					(or-helper (cdr args) (length (cdr args)) curr-int lambda-level prev-n curr-n )
				)
				(codegen-helper (car args) lambda-level prev-n curr-n  sorted-set))
)))

(define codegen-helper            ;start with level -1
	(lambda (exp lambda-level prev-n curr-n sorted-set)
	    (cond
	    	((eq? (car exp) 'const)
	    		(cond
	    			((and (number? (cadr exp)) (not (fraction? (cadr exp))))
	    					(format "mov rax,sobInt~a\n" (calc-index (cadr exp) sorted-set 0))
	    			)

	    			((fraction? (cadr exp))
	    				(format "mov rax,sobFraction~a\n" (calc-index (cadr exp) sorted-set 0))
	    			)

	    			((boolean? (cadr exp))
	    				(if (eq? #t (cadr exp)) "mov rax,sobBoolTrue\n" "mov rax,sobBoolFalse\n")
	    			)

	    			((string? (cadr exp))
	    				(format "mov rax,sobString~a\n" (calc-index (cadr exp) sorted-set 0))
	    			)

	    			((pair? (cadr exp))
	    				(format "mov rax,sobPair~a\n" (calc-index (cadr exp) sorted-set 0))
	    			)

	    			((char? (cadr exp))
	    				(format "mov rax,sobChar~a\n" (calc-index (cadr exp) sorted-set 0))
	    			)

	    			((vector? (cadr exp))
	    				(format "mov rax,sobVector~a\n" (calc-index (cadr exp) sorted-set 0))
	    			)
	    			((symbol? (cadr exp))
		    			(format "mov rax, sobSymbol~a\n" (calc-index (cadr exp) sorted-set 0))
	    			)
	    			((null? (cadr exp))
		    			"mov rax, sobNil\n"
	    			)
	    			((null? (cadr exp))
		    			"mov rax, sobNil\n"
	    			)
	    			((eq? (void) (cadr exp))
		    			"mov rax, sobVoid\n"
	    			)
	    		)
	   				
			)

			((eq? (car exp) 'seq)
				(if (null? (cadr exp)) ""
					(string-append
						(codegen-helper (caadr exp) lambda-level prev-n curr-n  sorted-set)
						(codegen-helper `(seq ,(cdadr exp)) lambda-level prev-n curr-n  sorted-set)
					)
				)
			)

			((eq? (car exp) 'fvar)
				(format "mov qword rax, L_fvar_~a\n" (calc-index-fvar (cadr exp) fvar-tab 0))
			)

			((eq? (car exp) 'box)
				(string-append
					(codegen-helper (cadr exp) lambda-level prev-n curr-n  sorted-set)
					"mov r14, rax\n"
					"my_malloc 8\n"
					"mov qword [rax], r14\n"
				)
			)

			((eq? (car exp) 'box-set)
				(string-append
					(codegen-helper (caddr exp) lambda-level prev-n curr-n  sorted-set) ;content
					"mov rbx, rax\n"
					"push rbx\n"
					(codegen-helper (cadr exp) lambda-level prev-n curr-n  sorted-set) ;addr
					"pop rbx\n"
					"mov qword [rax], rbx\n"
					"mov rax, sobVoid\n"
				)
			)

			((eq? (car exp) 'box-get)
				(string-append
					(codegen-helper (cadr exp) lambda-level prev-n curr-n  sorted-set) ;addr
					"mov rax, qword [rax]\n"
				)
			)

			((eq? (car exp) 'if3)
		      	(let ((curr-int (next-int)))
			      	(string-append 
			      	(codegen-helper (cadr exp) lambda-level prev-n curr-n  sorted-set) ;codegen test
					"cmp rax, sobBoolFalse\n"
					(format "je L_if3_else_~a\n" curr-int) ;label
					(codegen-helper (caddr exp) lambda-level prev-n curr-n  sorted-set) ;codegen dit
					(format "jmp L_if3_exit_~a\n" curr-int)	;label
					(format "L_if3_else_~a:\n" curr-int);label
					(codegen-helper (cadddr exp) lambda-level prev-n curr-n  sorted-set) ;codegen dif
					(format "L_if3_exit_~a:\n" curr-int);label
					)				
	      		)
	        )

	        ((eq? (car exp) 'or)
		      	(let ((curr-int (next-int)))
			      	(string-append
			      	(or-helper (cadr exp) (length (cadr exp)) curr-int lambda-level prev-n curr-n )
			      	(format "L_or_exit_~a:\n" curr-int))				
		      	)
		    )

		    ((eq? (car exp) 'pvar)
			    (string-append
			    (format "mov rax, qword[rbp + (4 + ~a) * 8]\n" (caddr exp))
		      	)
		    )
			
			((and (eq? (car exp) 'set) (eq? (caadr exp) 'pvar))
			    (string-append
			    (codegen-helper (caddr exp) lambda-level prev-n curr-n  sorted-set)
			    (format "mov qword[rbp + (4 + ~a) * 8], rax\n" (car (cddadr exp)))
			    "mov rax, sobVoid\n"
		      	)
		    )

			((eq? (car exp) 'bvar)
			    (string-append
			    "mov rax, qword[rbp + 2 * 8]\n" ;env
			    (format "mov rax, qword[rax + ~a * 8]\n" (caddr exp)) ;env [maj]
			    (format "mov rax, qword[rax + ~a * 8]\n" (cadddr exp)) ;env [maj][min]
		      	)
		    )

		    ((and (eq? (car exp) 'set) (eq? (caadr exp) 'bvar))
			    (string-append
			    "push rbx\n"
			    (codegen-helper (caddr exp) lambda-level prev-n curr-n  sorted-set)
			    "mov rbx, qword[rbp + 2 * 8]\n" 
			    (format "mov rbx, qword[rbx + ~a * 8]\n" (car (cddadr exp))) ;env [maj]
			    (format "mov qword[rbx + ~a * 8], rax\n" (cadr (cddadr exp))) ;env [maj][min]
			    "mov rax, sobVoid\n"
			    "pop rbx\n"
		      	)
		    )

		    ((and (eq? (car exp) 'set) (eq? (caadr exp) 'fvar))
			    (string-append
			    (codegen-helper (caddr exp) lambda-level prev-n curr-n  sorted-set)
			    "mov rbx, rax\n"
			    "push rbx\n"
			    (codegen-helper (cadr exp) lambda-level prev-n curr-n  sorted-set)
			    "pop rbx\n"
			    "mov qword [rax], rbx\n"
			    "mov rax, sobVoid\n"
			    "pop rbx\n"
		      	)
		    )

		    ((eq? (car exp) 'define)
			    (string-append
			    	(codegen-helper (cadr exp) lambda-level prev-n curr-n  sorted-set)
			    	"mov rbx, rax\n"
			    	"push rbx\n"
			    	(codegen-helper (caddr exp) lambda-level prev-n curr-n  sorted-set)
			    	"pop rbx\n"
			    	"mov rax, [rax]\n"
			    	"mov qword [rbx], rax\n"
			    	"mov rax, sobVoid\n"
		      	)
		    )

		    ((eq? (car exp) 'lambda-simple)
		      	(let ((curr-int (next-int))
		      		 )
			      	(string-append 
			      	
			      	(format "my_malloc ~a\n" (* (+ 1 lambda-level) 8)) ;ext env
			      	"mov qword rbx, rax\n"
			      	(format "my_malloc ~a\n" (* prev-n 8)) ;ext env[0]
			      	"mov qword rcx, rax\n"

			      	;;;;;;;;;;;;;;;;first loop - copying previous n arguments from stack to rcx[i];;;;;;;
			      	"mov qword rdx, 0\n"
			      	"mov qword r9, 0\n"
			      	(format "loop_arg_copy_start_~a:\n" curr-int) ;loop start for argument copying
			      	(format "cmp qword rdx, ~a \n" prev-n)
			      	(format "je loop_arg_copy_end_~a\n" curr-int)
			      	

			      	;"imul r9, rdx, 8\n" ; r9 = 8 * rdx
			      	"mov qword r8, [rbp + 32 + r9]\n" ;get argument[rdx]			      
			      	"mov qword [rcx + r9], r8\n" ; mov to rcx[rdx], r8

			      	"inc qword rdx\n" ; prev-n --
			      	"add qword r9, 8\n"
			      	(format "jmp loop_arg_copy_start_~a\n" curr-int)
			      	(format "loop_arg_copy_end_~a:\n" curr-int)
			      	"mov qword [rbx], rcx\n" ;mov the copied vector to rbx[0]
			      	;;;;;;;;;;;;;;;first loop end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

			      	;;;;;;;;;;;;;;;second loop - copying envs;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			      	"mov qword r10, 0\n" ; counter
			      	"mov qword r9, 0\n"
			      	(format "mov qword rdx, ~a\n"   lambda-level)
			      	"cmp rdx, 0\n"
			      	(format "jle loop_env_copy_end_~a\n" curr-int)

			      	(format "loop_env_copy_start_~a:\n" curr-int) ;loop start for env copying
			      	"cmp qword rdx, r10\n"
			      	(format "je loop_env_copy_end_~a\n" curr-int)

			      	"mov r8, [rbp + 16]\n" ; r8 = env

			      	
			      	"mov qword r8, [r8 + r9]\n" ; r8 = r8[r9]

			      	"mov qword [rbx + 8 + r9], r8\n"
			      	"inc qword r10\n"
			      	"add qword r9, 8\n"
			      	(format "jmp loop_env_copy_start_~a\n" curr-int)
			      	(format "loop_env_copy_end_~a:\n" curr-int)
			      	;;;;;;;;;;;;;;end of env copying;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			      	"my_malloc 16\n"
			      	"mov r13, rax\n"
			      	(format "MAKE_LITERAL_CLOSURE r13, rbx, code_lambda_~a\n"  curr-int)
			      	

			      	(format "jmp closure_end_~a\n" curr-int)
			      	(format "code_lambda_~a:\n" curr-int)
			      	"push qword rbp\n"
			      	"mov qword rbp, rsp\n"
			      	(codegen-helper (caddr exp) (+ 1 lambda-level) (length (cadr exp)) curr-n sorted-set)
			      	;;;;stack cleaning;;;;;
			      	"mov r15, [rbp + 24]\n"
			      	"add r15, 3\n"
			      	"imul r15, r15, 8\n"
			      	;;;;;;;;;;;;;;;;;;;;;;;
			      	"leave\n"
			      	"ret\n"
			      	(format "closure_end_~a:\n" curr-int)

			      	(format "mov rax ,r13\n")
					)				
	      		)
	        )

			((eq? (car exp) 'lambda-opt)
		      	(let ((curr-int (next-int))
		      		 )
			      	(string-append 
			      	(format "my_malloc ~a\n" (* (+ 1 lambda-level) 8)) ;ext env
			      	"mov qword rbx, rax\n"
			      	(format "my_malloc ~a\n" (* prev-n 8)) ;ext env[0]
			      	"mov qword rcx, rax\n"

			      	;;;;;;;;;;;;;;;;first loop - copying previous n arguments from stack to rcx[i];;;;;;;
			      	"mov qword rdx, 0\n"
			      	"mov qword r9, 0\n"
			      	(format "loop_arg_copy_start_~a:\n" curr-int) ;loop start for argument copying
			      	(format "cmp qword rdx, ~a \n" prev-n)
			      	(format "je loop_arg_copy_end_~a\n" curr-int)
			      	

			      	;"imul r9, rdx, 8\n" ; r9 = 8 * rdx
			      	"mov qword r8, [rbp + 32 + r9]\n" ;get argument[rdx]			      
			      	"mov qword [rcx + r9], r8\n" ; mov to rcx[rdx], r8

			      	"inc qword rdx\n" ; prev-n --
			      	"add qword r9, 8\n"
			      	(format "jmp loop_arg_copy_start_~a\n" curr-int)
			      	(format "loop_arg_copy_end_~a:\n" curr-int)
			      	"mov qword [rbx], rcx\n" ;mov the copied vector to rbx[0]
			      	;;;;;;;;;;;;;;;first loop end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

			      	;;;;;;;;;;;;;;;second loop - copying envs;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			      	"mov qword r10, 0\n" ; counter
			      	"mov qword r9, 0\n"
			      	(format "mov qword rdx, ~a\n"   lambda-level)
			      	"cmp rdx, 0\n"
			      	(format "jle loop_env_copy_end_~a\n" curr-int)

			      	(format "loop_env_copy_start_~a:\n" curr-int) ;loop start for env copying
			      	"cmp qword rdx, r10\n"
			      	(format "je loop_env_copy_end_~a\n" curr-int)

			      	"mov r8, [rbp + 16]\n" ; r8 = env

			      	
			      	"mov qword r8, [r8 + r9]\n" ; r8 = r8[r9]

			      	"mov qword [rbx + 8 + r9], r8\n"
			      	"inc qword r10\n"
			      	"add qword r9, 8\n"
			      	(format "jmp loop_env_copy_start_~a\n" curr-int)
			      	(format "loop_env_copy_end_~a:\n" curr-int)
			      	;;;;;;;;;;;;;;end of env copying;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			      	"my_malloc 16\n"
			      	"mov r13, rax\n"
			      	(format "MAKE_LITERAL_CLOSURE r13, rbx, code_lambda_~a\n"  curr-int)
			      	

			      	(format "jmp closure_end_~a\n" curr-int)
			      	(format "code_lambda_~a:\n" curr-int)
			      	"push qword rbp\n"
			      	"mov qword rbp, rsp\n"

			      	;;;;;;;;;;;;;;;;;;;;;;;;;stack correction starts here - r15 is loop counter;;;;;;;;;;
			      	"mov r15, [rbp + 24]\n"
			      	(format "sub r15, ~a\n" (length (cadr exp)))
			      	"cmp r15, 0\n" ; exclusive register for saving lambda-opt optional arg count
			      	(format "je end_of_stack_correction_~a \n" curr-int) ;if number of optional args is NOT 0 

			      	"mov qword r14, [rbp+24]\n"
			      	"sub r14, r15\n" ;r14 = n (number of non-opt args)
			      	;;;list construction;;;;
			      	"mov qword rbx, sobNil\n"
			      	"sub rbx, start_of_data\n"

			      	"mov rdx, r15\n" ; rdx = m (number of opt args)

			      	"mov r13, r14\n"
			      	"imul r13, r13, 8\n" ; r13 = r13 * 8

			      	"mov r12, r15\n"
			      	"imul r12, r12, 8\n" ; r12 = r12 * 8

			      	"add r12, r13\n"

			      	(format "list_construction_start_~a:\n" curr-int)
			      	"mov rcx, [rbp + 24 + r12]\n"
			      	"sub rcx, start_of_data\n"
					
					"mov r9, rcx\n"
					"shl r9, 30\n"
					"or  r9, rbx\n"
					"shl r9, 4\n"
					"or r9, T_PAIR\n"
					"mov rbx, r9\n"

					"sub rdx, 1\n"
					"sub r12, 8\n" ;bandaid...but works...
					"cmp rdx, 0\n"
					(format "je after_list_construction_~a\n" curr-int)

					"my_malloc 8\n"
					"mov [rax], rbx\n"
					"mov rbx, rax\n"
					"sub rbx, start_of_data\n"
					(format "jmp list_construction_start_~a\n" curr-int)

					(format "after_list_construction_~a:\n"  curr-int)

					"my_malloc 8\n"
					"mov [rax], rbx\n"
					"mov qword [rbp + 32 + 8*r14], rax\n" ; move constructed list to stack					
			      	(format "end_of_stack_correction_~a:\n" curr-int) ;forget about stack correction! we just clean everything up when we return!
			      	;;;;;;;;;;;;;;;;;;;;;;;;;stack correction ends here ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

			      	(codegen-helper (cadddr exp) (+ 1 lambda-level) (+ 1 (length (cadr exp))) curr-n sorted-set)
			      	;;;;stack cleaning;;;;;
			      	"mov r15, [rbp + 24]\n"
			      	"add r15, 3\n"
			      	"imul r15, r15, 8\n"
			      	;;;;;;;;;;;;;;;;;;;;;;;
			      	"leave\n"
			      	"ret\n"
			      	(format "closure_end_~a:\n" curr-int)

			      	(format "mov rax ,r13\n")
					)				
	      		)
	        )

			((eq? (car exp) 'applic)
			      	(string-append
			      		"push qword sobNil\n"
			      		(apply string-append 
			      			(map (lambda(ei) 
			      					(string-append 
			      						(codegen-helper ei lambda-level prev-n curr-n sorted-set)
			      						"push qword rax\n"
			      					)
			      				  )
			      					(reverse (caddr exp))))

			      		(format "push qword ~a\n" (length (caddr exp)))
			      		(codegen-helper (cadr exp) lambda-level prev-n (length (caddr exp)) sorted-set)
			      		"mov qword rbx, [rax]\n"
			      		
			      		"CLOSURE_ENV rbx\n"
			      		"push qword rbx\n"

			      		"mov qword rax, [rax]\n"
			      		"CLOSURE_CODE rax\n"
		

			      		"call rax\n"
			      		"add rsp , r15\n"
			      		
			      	)
			    

		    )
			
			((eq? (car exp) 'tc-applic)
				(let ((curr-int (next-int))
					  (prev-applic curr-n)
					 )
			      	(string-append
			      		"push qword sobNil\n"
			      		(apply string-append 
			      			(map (lambda(ei) 
			      					(string-append
			      						(codegen-helper ei lambda-level prev-n curr-n sorted-set)
			      						"push qword rax\n"
			      					)
			      				  )
			      					(reverse (caddr exp))))

			      		(format "push qword ~a\n" (length (caddr exp)))			      	
			      		(codegen-helper (cadr exp) lambda-level prev-n (length (caddr exp))  sorted-set)
			      		
			      		"mov qword rbx, [rax]\n"
			      		
			      		"CLOSURE_ENV rbx\n"
			      		"push qword rbx\n"

			      		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Stack correction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			      		"mov r15, qword [rbp + 24]\n" ;old stack arg count

			      		"mov r11, rbp\n" ;; saving current rsp for stack offset calculations
			      		"mov rbp, qword [rbp]\n" ;saving old rbp before push

						"mov r9, qword [r11 + 8]\n" ;copy old ret add
						"push r9\n" ;; pushing old return address

						"mov r8, -8\n" ;counter for rsp offset calculations
						"mov r10, 0\n" ;;counter for loop

						"mov r9, r15\n" ;old stack arg count
						"add r9, 4\n" ;add 4 before multiplying
						"imul r9, r9, 8\n" ; counter for copying to old frame start, r9 = (4 + old-arg-c) * 8

						(format "tcapplic_loop_start_~a:\n" curr-int)
						(format "cmp r10, ~a\n" (+ 4 (length (caddr exp))))
						(format "je tcapplic_loop_end_~a\n" curr-int)

						"mov rbx, qword [r11 + r8]\n"
						"mov qword [r11 + r9], rbx\n"

						"sub r8, 8\n"
						"sub r9, 8\n"
						"add r10, 1\n"

						(format "jmp tcapplic_loop_start_~a\n" curr-int)
						(format "tcapplic_loop_end_~a:\n" curr-int)
						"add r15, 5\n"
						"imul r15, r15, 8\n"						
						"add rsp, r15\n"
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

			      		"mov qword rax, [rax]\n"
			      		"CLOSURE_CODE rax\n"
			      		"jmp rax\n"
			      	)
				)
		    )
		    (else (format "error-i cant identify ~a" exp))
		)
	)
)

(define codegen1
	(lambda(expr)
		(string-append
			(codegen-helper expr -1 0 0 sorted-set)
			"push qword [rax]\ncall write_sob_if_not_void\nadd rsp, 1*8\n\n"
		)
	)
)

(define code-gen
	(lambda(sexpr-list)
		(apply string-append (map codegen1 sexpr-list))
	)
)

(set! base_label 2)

(define next-int
	(lambda()
		(let ((curr-label  base_label))
			(begin
				(set! base_label (+ 1 curr-label))
				curr-label
			)
)))


(define rt-support-code
	(string-append

		"make_vector_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"

		"push rbx\n"
		"push rcx\n"
		"push r9\n"
		"push r8\n"

		"mov r8, qword [rbp + 24]\n"
		"cmp r8,1\n"
		"jne make_vector_2_arg\n"
		"my_malloc 8\n"
		"mov qword [rax], 0\n"
		"shl qword [rax],4 \n"
		"or qword [rax], T_INTEGER\n"
		"mov An(1), rax\n"

		"make_vector_2_arg:"
		"mov rcx, An(0)\n" ; counter
		"mov rcx, qword [rcx]\n"
		"DATA rcx\n"
		"mov r9, rcx\n"
		"mov r8, rcx\n"

		"cmp rcx,0\n"
		"je make_empty_vector\n"

		"mov rbx, An(1)\n" ; get the item
		
		"imul rcx, rcx, 8\n"
		"my_malloc rcx\n"
		"mov rcx, 0\n"

		"vector_loop:\n"
		"mov qword [rax + rcx], rbx\n"
		"sub r9, 1\n"
		"cmp r9, 0\n"
		"je vector_loop_end\n"
		"add rcx, 8\n"
		"jmp vector_loop\n"

		"make_empty_vector:\n"
		"my_malloc 1\n"

		"vector_loop_end:\n"
		"sub rax, start_of_data\n"
		"mov rcx, rax\n"
		"my_malloc 8\n"
		"or qword [rax], r8\n"
		"shl qword [rax], 30\n"
		"or qword [rax], rcx\n"
		"shl qword [rax], 4\n"
		"or qword [rax], T_VECTOR\n"


		"pop r8\n"
		"pop r9\n"
		"pop rcx\n"
		"pop rbx\n"

		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"make_string_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"

		"push rbx\n"
		"push rcx\n"
		"push r9\n"

		"cmp qword [rbp + 24], 1\n"
		"jne make_string_2_args\n"
		"my_malloc 8\n"
		"mov qword [rax], 0\n"
		"shl qword [rax], 4\n"
		"or qword [rax], T_CHAR\n"
		"mov An(1), rax\n"

		"make_string_2_args:\n"
		"mov rcx, An(0)\n" ; counter
		"mov rcx, qword [rcx]\n"
		"DATA rcx\n"
		"mov r9, rcx\n"

		"mov rbx, An(1)\n" ; get the char
		"mov rbx, qword [rbx]\n"
		"DATA rbx\n" ;get char value

		"cmp rcx, 0\n"
		"je make_empty_string\n"
		"my_malloc rcx\n"
		"jmp make_string_loop\n"

		"make_empty_string:\n"
		"my_malloc 1\n"
		"jmp make_string_loop_end\n"

		"make_string_loop:\n"
		"or qword [rax], rbx\n"
		"sub rcx, 1\n"
		"cmp rcx, 0\n"
		"je make_string_loop_end\n"
		"shl qword [rax], 8\n"
		"jmp make_string_loop\n"

		"make_string_loop_end:\n"
		"sub rax, start_of_data\n"
		"mov rcx, rax\n"
		"my_malloc 8\n"
		"or qword [rax], r9\n"
		"shl qword [rax], 30\n"
		"or qword [rax], rcx\n"
		"shl qword [rax], 4\n"
		"or qword [rax], T_STRING\n"

		"pop r9\n"
		"pop rcx\n"
		"pop rbx\n"

		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"string_to_symbol_code:
		push rbp
		mov rbp, rsp

		push r9
		push r10
		push rbx
		push rcx

		mov rax, An(0) ;; get the string 
		;STRING_ELEMENTS rax
		
		mov rbx, symbol_table ;; head of list
		cmp qword [rbx], 0 ; if list head is empty then...
		je loop_end_false

		loop:
		mov qword rcx, rbx ; copy the link
		mov rcx, qword [rbx]
		shr rcx, 4
		;STRING_ELEMENTS rcx

		cmp rax, rcx  ;; check to see if string adresses are equal
		je func_end ;; if equal jump to true and exit loop
		mov rbx, qword [rbx + 8]
		cmp qword [rbx], 0  ;;if not equal check to see if were at list end
		je loop_end_false ;;if at list end jump to false and exit loop
		jmp loop

		loop_end_false:
		mov r10, An(0)
		MAKE_LITERAL_SYMBOL r10, r9
		sym_tab_append r9
		
		func_end:
		mov rax, rbx

		pop rcx
		pop rbx
		pop r10
		pop r9	

		mov r15, [rbp + 24]\n
		add r15, 3\n
		imul r15, r15, 8\n
		leave
		ret\n"

		"boolean_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, [rbp + 32]\n"
		"push qword rbx\n"
		"mov rbx, [rax] \n"
		"TYPE rbx\n"
		"cmp rbx , T_BOOL\n"
		"jne not_bool\n"
		"mov qword rax, sobBoolTrue\n"
		"jmp bool_exit\n"
		"not_bool:\n"
		"mov qword rax, sobBoolFalse\n"
		"bool_exit:\n"
		"pop qword rbx\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"apply_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		
		"push qword sobNil\n"
		"mov rax, An(1)\n" ;get list
		"mov rcx, 0\n" ; counter

		;;;;;;;;;;;;;;calculate list length ;;;;;;;;;;;;;;;
		"apply_calc_list_len_start:\n"
		"cmp rax, sobNil\n" ;check if were at list end
		"je apply_calc_list_len_end\n"
		"add rcx, 1\n" ;if its not null increase by 1
		"GET_CDR rax\n"
		"jmp apply_calc_list_len_start\n"
		"apply_calc_list_len_end:\n"
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		"mov r9, rcx\n"

		"mov rax, An(1)\n" ;get list
		"cmp rcx,0\n"
		"je push_func\n"
		"mov rdx, rcx\n" ;copy counter to use in loop

		"apply_list_flatten_start:\n"
		"cmp rdx, 1\n" ;
		"je apply_list_flatten_push\n" 
		"GET_CDR rax\n"
		"sub rdx, 1\n"
		"jmp apply_list_flatten_start\n"

		"apply_list_flatten_push:\n"
		"GET_CAR rax\n"
		"push rax\n"

		"mov rax, An(1)\n" ;get list
		"sub rcx, 1\n"
		"cmp rcx, 0\n"
		"je push_func\n"
		"mov rdx, rcx\n"
		"jmp apply_list_flatten_start\n"
		"apply_list_flatten_end:\n"

		"push_func:\n"
		"push r9\n" ; arg n count
		"mov rax, An(0)\n"
		"mov qword rbx, [rax]\n"
		"CLOSURE_ENV rbx\n"
		"push qword rbx\n"

		"mov qword rax, [rax]\n"
		"CLOSURE_CODE rax\n"
		"call rax\n"		
		"add rsp , r15\n"


		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"string?_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n" 
		"TYPE rax\n"
		"cmp rax , T_STRING\n"
		"jne not_string\n"
		"mov rax , sobBoolTrue\n"
		"jmp string_exit\n"
		"not_string:\n"
		"mov rax , sobBoolFalse\n"
		"string_exit:\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"char?_code:\n"	
		"push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n" 
		"TYPE rax\n"
		"cmp rax , T_CHAR\n"
		"jne not_char\n"
		"mov rax , sobBoolTrue\n"
		"jmp char_exit\n"
		"not_char:\n"
		"mov rax , sobBoolFalse\n"
		"char_exit:\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"integer?_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n" 
		"TYPE rax\n"
		"cmp rax , T_INTEGER\n"
		"jne not_integer\n"
		"mov rax , sobBoolTrue\n"
		"jmp integer_exit\n"
		"not_integer:\n"
		"mov rax , sobBoolFalse\n"
		"integer_exit:\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"


		"symbol?_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n" 
		"TYPE rax\n"
		"cmp rax , T_SYMBOL\n"
		"jne not_symbol\n"
		"mov rax , sobBoolTrue\n"
		"jmp symbol_exit\n"
		"not_symbol:\n"
		"mov rax , sobBoolFalse\n"
		"symbol_exit:\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"vector?_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n" 
		"TYPE rax\n"
		"cmp rax , T_VECTOR\n"
		"jne not_vector\n"
		"mov rax , sobBoolTrue\n"
		"jmp vector_exit\n"
		"not_vector:\n"
		"mov rax , sobBoolFalse\n"
		"vector_exit:\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"pair?_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n" 
		"TYPE rax\n"
		"cmp rax , T_PAIR\n"
		"jne not_pair\n"
		"mov rax , sobBoolTrue\n"
		"jmp pair_exit\n"
		"not_pair:\n"
		"mov rax , sobBoolFalse\n"
		"pair_exit:\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"zero?_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n"
		"DATA rax\n" 
		"cmp rax , 0\n"
		"jne not_zero\n"
		"mov rax , sobBoolTrue\n"
		"jmp zero_exit\n"
		"not_zero:\n"
		"mov rax , sobBoolFalse\n"
		"zero_exit:\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"null?_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n" 
		"TYPE rax\n"
		"cmp rax , T_NIL\n"
		"jne not_null\n"
		"mov rax , sobBoolTrue\n"
		"jmp null_exit\n"
		"not_null:\n"
		"mov rax , sobBoolFalse\n"
		"null_exit:\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"procedure?_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n" 
		"TYPE rax\n"
		"cmp rax , T_CLOSURE\n"
		"jne not_procedure\n"
		"mov rax , sobBoolTrue\n"
		"jmp procedure_exit\n"
		"not_procedure:\n"
		"mov rax , sobBoolFalse\n"
		"procedure_exit:\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"number?_code:\n" ;;;;not integer and not fraction is not a number;;;;
        "push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n" 
		"TYPE rax\n"
		"cmp rax , T_INTEGER\n"
		"je is_number\n"
		"cmp rax , T_FRACTION\n"
		"jne not_number\n" 
		"is_number:\n"
		"mov rax , sobBoolTrue\n"
		"jmp number_exit\n"
		"not_number:\n"
		"mov rax , sobBoolFalse\n"
		"number_exit:\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"car_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n"
		"shr rax, 34\n"
		"add rax, start_of_data\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"cdr_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n"
		"shl rax, 30\n"
		"shr rax, 34\n"
		"add rax, start_of_data\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"cons_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"push rbx\n"
		"push rcx\n" 
		"my_malloc 8\n"
		"mov qword rcx, An(0)\n"  
		"mov qword [rax], rcx\n"
		"sub qword [rax], start_of_data\n"
		"shl qword [rax], ((WORD_SIZE - TYPE_BITS) >> 1)\n" 
		"mov rbx, An(1)\n"
		"sub rbx, start_of_data\n"
		"or qword [rax], rbx\n" 
		"shl qword [rax], TYPE_BITS\n" 
		"or qword [rax], T_PAIR\n"
		"pop rcx\n" 
		"pop rbx\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"integer_to_char_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"push rbx\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n"
		"shr rax, TYPE_BITS\n"
		"shl rax, TYPE_BITS\n"
		"or rax, T_CHAR\n"
		"mov rbx, rax\n"
		"my_malloc 8\n"
		"mov [rax], rbx\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"pop rbx\n"
		"leave\n"
		"ret\n"

		"char_to_integer_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"push rbx\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n"
		"DATA rax\n"
		"mov rbx, rax\n"
		"my_malloc 8\n"
		"mov qword [rax], rbx\n"
		"shl qword [rax], 4\n"
		"or qword [rax], T_INTEGER\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"pop rbx\n"
		"leave\n"
		"ret\n"

		"denominator_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n"
		"TYPE rax\n"
		"cmp rax , T_INTEGER\n"
		"jne fraction_denominator\n"
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		"my_malloc 8\n"
		"mov rbx, 1\n"
		"or qword [rax], rbx\n"
		"shl qword [rax], 4\n"
		"or qword [rax], T_INTEGER\n" 
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		"jmp denominator_exit\n"
		"fraction_denominator:\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n"
		"shl rax, 30\n"
		"shr rax, 34\n"
		"add rax, start_of_data\n"
		"denominator_exit:\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"numerator_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n"
		"TYPE rax\n"
		"cmp rax , T_INTEGER\n"
		"jne fraction_numerator\n"
		"mov rax, An(0)\n" 
		
		"jmp numerator_exit\n"
		"fraction_numerator:\n"
		"mov rax, An(0)\n"
		"mov rax, [rax]\n"
		"shr rax, 34\n"
		"add rax, start_of_data\n"
		"numerator_exit:\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"negative?_code:\n"
		"push rbp\n"
		"mov rbp, rsp\n"
		"mov rax, An(0)\n"
		"mov rax, [rax]\n"
		"TYPE rax\n"
		"cmp rax,T_INTEGER\n"
		"jne negative_fraction\n"
		"mov rax, An(0)\n"
		"mov rax, [rax]\n"
		"sar rax, 4\n"
		"cmp rax, 0\n"
		"jge not_negative\n"
		"mov rax , sobBoolTrue\n"
		"jmp negative_exit\n"
		"not_negative:\n"
		"mov rax , sobBoolFalse\n"
		"jmp negative_exit\n"
		
		
		"negative_fraction:\n"
		"mov rax, An(0)\n"
		"mov rax, [rax]\n"
		"sar rax, 34\n"
		"add rax, start_of_data\n"
		"mov rax, [rax]\n"
		"sar rax, 4\n"
		"cmp rax, 0\n"
		"jge not_negative_fraction\n"
		"mov rax , sobBoolTrue\n"
		"jmp negative_exit\n"
		"not_negative_fraction:\n"
		"mov rax , sobBoolFalse\n"
		
		
		
		"negative_exit:\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"gcd:\n"
		"push rbp\n"
		"mov rbp, rsp\n"

		"mov rdx, 0\n"
		"mov rax, [rbp + 2*8]\n" ; first
		"mov r9, [rbp + 3*8]\n" ; second
		"iabs rax\n"
		"iabs r9\n"
		"cmp rax, r9\n"
		"jge .loop\n"
		"xchg rax, r9\n"
	
		".loop:\n"
		"cmp r9, 0\n"
		"je .done\n"
		"mov rdx, 0\n"
		"cqo\n"
		"idiv r9\n"
		"mov rax, r9\n"
		"mov r9, rdx\n"
		"jmp .loop\n"

		".done:\n"
		"pop rbp\n"
		"ret\n"

		"mult_code:\n"
		"push rbp\n" 
		"mov rbp, rsp\n"
		"push rcx\n"
		"push r10\n"
		"push rbx\n"
		"push r11\n"
		"mov rax, An(0)\n"
		"mov rax, [rax]\n"
		"TYPE rax\n"
		"cmp rax, T_INTEGER\n"
		"jne first_fraction\n"

		"mov rcx, An(0)\n"
		"mov rcx, [rcx]\n"
		"sar rcx, 4\n"
		"mov r10,0\n"
		"or r10, rcx\n"
		"sal r10, 30\n"
		"mov qword rcx, 1\n"
		"or r10, rcx\n"
		"sal r10, 4\n"
		"or r10, T_FRACTION\n"
		"jmp check_second\n"

		"first_fraction:\n"
		"mov rax, An(0)\n" ;first argument is fraction
		"mov rax, [rax]\n"
		"mov qword r10, rax\n"
		"sar r10, 34\n"
		"add r10, start_of_data\n"
		"mov r10, [r10]\n"
		"sar r10, 4\n"
		"sal r10, 34\n"
		"mov qword rbx, rax\n"
		"sal rbx, 30\n"
		"sar rbx, 34\n"
		"add rbx, start_of_data\n"
		"mov rbx, [rbx]\n"
		"sar rbx, 4\n"
		"sal rbx, 4\n"
		"or r10, rbx\n"
		"or r10, T_FRACTION\n"


		"check_second:\n"
		"mov rax, An(1)\n"
		"mov rax, [rax]\n"
		"TYPE rax\n"
		"cmp rax, T_INTEGER\n"
		"jne second_fraction\n"
		"mov rcx, An(1)\n"
		"mov rcx, [rcx]\n"
		"sar rcx, 4\n"
		"mov r11,0\n"
		"or r11, rcx\n"
		"sal r11, 30\n"
		"mov qword rcx, 1\n"
		"or r11, rcx\n"
		"sal r11, 4\n"
		"or r11, T_FRACTION\n"
		"jmp mul_exit\n"

		"second_fraction:\n"
		"mov rax, An(1)\n" ;second argument is fraction
		"mov rax, [rax]\n"
		"mov qword r11, rax\n"
		"sar r11, 34\n"
		"add r11, start_of_data\n"
		"mov r11, [r11]\n"
		"sar r11, 4\n"
		"sal r11, 34\n"
		"mov qword rbx, rax\n"
		"sal rbx, 30\n"
		"sar rbx, 34\n"
		"add rbx, start_of_data\n"
		"mov rbx, [rbx]\n"
		"sar rbx, 4\n"
		"sal rbx, 4\n"
		"or r11, rbx\n"
		"or r11, T_FRACTION\n"

		"mul_exit:\n"
		"mov qword rax, r10\n"
		"sar rax, 34\n"
		"mov qword rcx, r11\n"
		"sar rcx, 34\n"
		"imul rcx\n"
		"mov qword rbx, rax\n" ; mone*mone in rbx
		"mov qword rax, r10\n"
		"sal rax, 30\n"
		"sar rax, 34\n"
		"mov qword rcx, r11\n"
		"sal rcx, 30\n"
		"sar rcx, 34\n"
		"imul rcx\n"
		"mov qword rcx, rax\n" ; mehane*mehane in rcx

		"cmp rbx, 0\n"
		"jne check_mul_mehane\n"
		"my_malloc 8\n"
		"sal rbx, 4\n"
        "or rbx, T_INTEGER\n"
		"mov qword [rax], rbx\n"
		"jmp mul_exit_forsure\n"

		"check_mul_mehane:\n"
		"cmp rcx, 1\n"
		"jne check_gcd\n"
		"my_malloc 8\n"
		"sal rbx, 4\n"
        "or rbx, T_INTEGER\n"
		"mov qword [rax], rbx\n"
		"jmp mul_exit_forsure\n"

		"check_gcd:\n"
		"add rsp, 1*8\n"
		"push rbx\n"
		"push rcx\n"
		
		"call gcd\n"
		"add rsp, 2*8\n"
		"mov qword r12, rax\n" ; now return value in r12

		"mov qword rax, rbx\n"
		"cqo\n"
		"idiv r12\n"
		"mov qword rbx, rax\n"

		"mov qword rax, rcx\n"
		"cqo\n"
		"idiv r12\n"
		"mov qword rcx, rax\n"

		"cmp rcx, 1\n"
		"jne not_int_forsure\n"
		"sal rbx, 4\n"
        "or rbx, T_INTEGER\n"
        "my_malloc 8\n"
		"mov qword [rax], rbx\n"
		"jmp mul_exit_forsure\n"

		"not_int_forsure:\n"
		"sal rbx, 4\n"
        "or rbx, T_INTEGER\n"
		"my_malloc 8\n"
		"mov [rax], rbx\n"
		"mov rbx, rax\n"
		"sub rbx, start_of_data\n"
		"sal rbx, 34\n"

		"sal rcx, 4\n"
        "or rcx, T_INTEGER\n"
		"my_malloc 8\n"
		"mov [rax], rcx\n"
		"mov rcx, rax\n"
		"sub rcx, start_of_data\n"
		"sal rcx, 4\n"
		"or rbx, rcx\n"
		"or rbx, T_FRACTION\n"
		"my_malloc 8\n"
		"mov [rax], rbx\n"

		"mul_exit_forsure:\n"
		"pop r11\n"
		"pop rbx\n"
		"pop r10\n"
		"pop rcx\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"plus_code:\n"
		"push rbp\n" 
		"mov rbp, rsp\n"
		"push rcx\n"
		"push r10\n"
		"push rbx\n"
		"push r11\n"
		"mov rax, An(0)\n"
		"mov rax, [rax]\n"
		"TYPE rax\n"
		"cmp rax, T_INTEGER\n"
		"jne first_plus_fraction\n"

		"mov rcx, An(0)\n"
		"mov rcx, [rcx]\n"
		"sar rcx, 4\n"
		"mov r10,0\n"
		"or r10, rcx\n"
		"sal r10, 30\n"
		"mov qword rcx, 1\n"
		"or r10, rcx\n"
		"sal r10, 4\n"
		"or r10, T_FRACTION\n"
		"jmp check_plus_second\n"

		"first_plus_fraction:\n"
		"mov rax, An(0)\n" ;first argument is fraction
		"mov rax, [rax]\n"
		"mov qword r10, rax\n"
		"sar r10, 34\n"
		"add r10, start_of_data\n"
		"mov r10, [r10]\n"
		"sar r10, 4\n"
		"sal r10, 34\n"
		"mov qword rbx, rax\n"
		"sal rbx, 30\n"
		"sar rbx, 34\n"
		"add rbx, start_of_data\n"
		"mov rbx, [rbx]\n"
		"sar rbx, 4\n"
		"sal rbx, 4\n"
		"or r10, rbx\n"
		"or r10, T_FRACTION\n"


		"check_plus_second:\n"
		"mov rax, An(1)\n"
		"mov rax, [rax]\n"
		"TYPE rax\n"
		"cmp rax, T_INTEGER\n"
		"jne second_plus_fraction\n"
		"mov rcx, An(1)\n"
		"mov rcx, [rcx]\n"
		"sar rcx, 4\n"
		"mov r11,0\n"
		"or r11, rcx\n"
		"sal r11, 30\n"
		"mov qword rcx, 1\n"
		"or r11, rcx\n"
		"sal r11, 4\n"
		"or r11, T_FRACTION\n"
		"jmp plus_exit\n"

		"second_plus_fraction:\n"
		"mov rax, An(1)\n" ;second argument is fraction
		"mov rax, [rax]\n"
		"mov qword r11, rax\n"
		"sar r11, 34\n"
		"add r11, start_of_data\n"
		"mov r11, [r11]\n"
		"sar r11, 4\n"
		"sal r11, 34\n"
		"mov qword rbx, rax\n"
		"sal rbx, 30\n"
		"sar rbx, 34\n"
		"add rbx, start_of_data\n"
		"mov rbx, [rbx]\n"
		"sar rbx, 4\n"
		"sal rbx, 4\n"
		"or r11, rbx\n"
		"or r11, T_FRACTION\n"


		"plus_exit:\n"
		"mov qword rax, r10\n"
		"sar rax, 34\n"
		"mov qword rcx, r11\n"
		"sal rcx, 30\n"
		"sar rcx, 34\n"
		"imul rcx\n"
		"mov rcx, rax\n" 
		"mov qword rax, r11\n"
		"sar rax, 34\n"
		"mov qword rbx, r10\n"
		"sal rbx, 30\n"
		"sar rbx, 34\n"
		"imul rbx\n"
		"mov rbx, rax\n"
		"add rbx, rcx\n" ;mone+mone after in rbx
		"mov qword rax, r10\n"
		"sal rax, 30\n"
		"sar rax, 34\n"
		"mov qword rcx, r11\n"
		"sal rcx, 30\n"
		"sar rcx, 34\n"
		"imul rcx\n"
		"mov qword rcx, rax\n" ; mehane*mehane in rcx

		"cmp rbx, 0\n"
		"jne check_plus_mehane\n"
		"my_malloc 8\n"
		"sal rbx, 4\n"
        "or rbx, T_INTEGER\n"
		"mov qword [rax], rbx\n"
		"jmp plus_exit_forsure\n"

		"check_plus_mehane:\n"
		"cmp rcx, 1\n"
		"jne check_plus_gcd\n"
		"my_malloc 8\n"
		"sal rbx, 4\n"
        "or rbx, T_INTEGER\n"
		"mov qword [rax], rbx\n"
		"jmp plus_exit_forsure\n"

		"check_plus_gcd:\n"
		"add rsp, 1*8\n"
		"push rbx\n"
		"push rcx\n"
		
		"call gcd\n"
		"add rsp, 2*8\n"
		"mov qword r12, rax\n" ; now return value in r12

		"mov qword rax, rbx\n"
		"cqo\n"
		"idiv r12\n"
		"mov qword rbx, rax\n"

		"mov qword rax, rcx\n"
		"cqo\n"
		"idiv r12\n"
		"mov qword rcx, rax\n"

		"cmp rcx, 1\n"
		"jne not_int_plus_forsure\n"
		"sal rbx, 4\n"
        "or rbx, T_INTEGER\n"
        "my_malloc 8\n"
		"mov qword [rax], rbx\n"
		"jmp plus_exit_forsure\n"

		"not_int_plus_forsure:\n"
		"sal rbx, 4\n"
        "or rbx, T_INTEGER\n"
		"my_malloc 8\n"
		"mov [rax], rbx\n"
		"mov rbx, rax\n"
		"sub rbx, start_of_data\n"
		"sal rbx, 34\n"

		"sal rcx, 4\n"
        "or rcx, T_INTEGER\n"
		"my_malloc 8\n"
		"mov [rax], rcx\n"
		"mov rcx, rax\n"
		"sub rcx, start_of_data\n"
		"sal rcx, 4\n"
		"or rbx, rcx\n"
		"or rbx, T_FRACTION\n"
		"my_malloc 8\n"
		"mov [rax], rbx\n"

		"plus_exit_forsure:\n"
		"pop r11\n"
		"pop rbx\n"
		"pop r10\n"
		"pop rcx\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"minus_code:\n"
		"push rbp\n" 
		"mov rbp, rsp\n"
		"push rcx\n"
		"push r10\n"
		"push r13\n"
		"push r11\n"
		;"push r13\n"
		;"mov qword r13, 1\n"
		"mov rax, An(0)\n"
		"mov rax, [rax]\n"
		"TYPE rax\n"
		"cmp rax, T_INTEGER\n"
		"jne first_minus_fraction\n"

		"mov rcx, An(0)\n"
		"mov rcx, [rcx]\n"
		"sar rcx, 4\n"
		"mov r10,0\n"
		"or r10, rcx\n"
		"sal r10, 30\n"
		"mov qword rcx, 1\n"
		"or r10, rcx\n"
		"sal r10, 4\n"
		"or r10, T_FRACTION\n"
		"jmp check_minus_second\n"

		"first_minus_fraction:\n"
		"mov rax, An(0)\n" ;first argument is fraction
		"mov rax, [rax]\n"
		"mov qword r10, rax\n"
		"sar r10, 34\n"
		"add r10, start_of_data\n"
		"mov r10, [r10]\n"
		"sar r10, 4\n"
		"sal r10, 34\n"
		"mov qword r13, rax\n"
		"sal r13, 30\n"
		"sar r13, 34\n"
		"add r13, start_of_data\n"
		"mov r13, [r13]\n"
		"sar r13, 4\n"
		"sal r13, 4\n"
		"or r10, r13\n"
		"or r10, T_FRACTION\n"


		"check_minus_second:\n"
		"mov rax, An(1)\n"
		"mov rax, [rax]\n"
		"TYPE rax\n"
		"cmp rax, T_INTEGER\n"
		"jne second_minus_fraction\n"
		"mov rcx, An(1)\n"
		"mov rcx, [rcx]\n"
		"sar rcx, 4\n"
		"mov r11,0\n"
		"or r11, rcx\n"
		"sal r11, 30\n"
		"mov qword rcx, 1\n"
		"or r11, rcx\n"
		"sal r11, 4\n"
		"or r11, T_FRACTION\n"
		"jmp minus_exit\n"

		"second_minus_fraction:\n"
		"mov rax, An(1)\n" ;second argument is fraction
		"mov rax, [rax]\n"
		"mov qword r11, rax\n"
		"sar r11, 34\n"
		"add r11, start_of_data\n"
		"mov r11, [r11]\n"
		"sar r11, 4\n"
		"sal r11, 34\n"
		"mov qword r13, rax\n"
		"sal r13, 30\n"
		"sar r13, 34\n"
		"add r13, start_of_data\n"
		"mov r13, [r13]\n"
		"sar r13, 4\n"
		"sal r13, 4\n"
		"or r11, r13\n"
		"or r11, T_FRACTION\n"


		"minus_exit:\n"
		"mov qword rax, r10\n"
		"sar rax, 34\n"
		"mov qword r13, r11\n"
		"sal r13, 30\n"
		"sar r13, 34\n"
		"imul r13\n"
		"mov r13, rax\n" 
		"mov qword rax, r11\n"
		"sar rax, 34\n"
		"mov qword rcx, r10\n"
		"sal rcx, 30\n"
		"sar rcx, 34\n"
		"imul rcx\n"
		"mov rcx, rax\n"
		"sub r13, rcx\n" ;mone-mone after in rbx
		;"cmp rbx, 0\n"
		;"jg positive\n"
		;"mov qword r13, 0\n"
		;"neg rbx\n"
		;"positive:\n"
		"mov qword rax, r10\n"
		"sal rax, 30\n"
		"sar rax, 34\n"
		"mov qword rcx, r11\n"
		"sal rcx, 30\n"
		"sar rcx, 34\n"
		"imul rcx\n"
		"mov qword rcx, rax\n" ; mehane*mehane in rcx

		"cmp r13, 0\n"
		"jne check_minus_mehane\n"
		"my_malloc 8\n"
		"sal r13, 4\n"
        "or r13, T_INTEGER\n"
		"mov qword [rax], r13\n"
		"jmp minus_exit_forsure\n"

		"check_minus_mehane:\n"
		"cmp rcx, 1\n"
		"jne check_minus_gcd\n"
		;"cmp r13, 0\n"
		;"jg cont1\n"
		;"neg rbx\n"
		"cont1:\n"
		"my_malloc 8\n"
		"sal r13, 4\n"
        "or r13, T_INTEGER\n"
		"mov qword [rax], r13\n"
		"jmp minus_exit_forsure\n"

		"check_minus_gcd:\n"
		"add rsp, 1*8\n"
		"push r13\n"
		"push rcx\n"
		
		"call gcd\n"
		"add rsp, 2*8\n"
		"mov qword r12, rax\n" ; now return value in r12

		"mov qword rax, r13\n"
		"cqo\n"
		"idiv r12\n"
		"mov qword r13, rax\n"
		;"cmp r13, 0\n"
		;"jg cont2\n"
		;"neg rbx\n"

		;"cont2:\n"
		"mov qword rax, rcx\n"
		"cqo\n"
		"idiv r12\n"
		"mov qword rcx, rax\n"

		"cmp rcx, 1\n"
		"jne not_int_minus_forsure\n"
		"sal r13, 4\n"
        "or r13, T_INTEGER\n"
        "my_malloc 8\n"
		"mov qword [rax], r13\n"
		"jmp minus_exit_forsure\n"

		"not_int_minus_forsure:\n"
		"sal r13, 4\n"
        "or r13, T_INTEGER\n"
		"my_malloc 8\n"
		"mov [rax], r13\n"
		"mov r13, rax\n"
		"sub r13, start_of_data\n"
		"sal r13, 34\n"

		"sal rcx, 4\n"
        "or rcx, T_INTEGER\n"
		"my_malloc 8\n"
		"mov [rax], rcx\n"
		"mov rcx, rax\n"
		"sub rcx, start_of_data\n"
		"sal rcx, 4\n"
		"or r13, rcx\n"
		"or r13, T_FRACTION\n"
		"my_malloc 8\n"
		"mov [rax], r13\n"

		"minus_exit_forsure:\n"
		;"pop r13\n"
		"pop r11\n"
		"pop r13\n"
		"pop r10\n"
		"pop rcx\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"div_code:\n"
		"push rbp\n" 
		"mov rbp, rsp\n"
		"push rcx\n"
		"push r10\n"
		"push rbx\n"
		"push r11\n"
		"mov rax, An(0)\n"
		"mov rax, [rax]\n"
		"TYPE rax\n"
		"cmp rax, T_INTEGER\n"
		"jne first_div_fraction\n"

		"mov rcx, An(0)\n"
		"mov rcx, [rcx]\n"
		"sar rcx, 4\n"
		"mov r10,0\n"
		"or r10, rcx\n"
		"sal r10, 30\n"
		"mov qword rcx, 1\n"
		"or r10, rcx\n"
		"sal r10, 4\n"
		"or r10, T_FRACTION\n"
		"jmp check_div_second\n"

		"first_div_fraction:\n"
		"mov rax, An(0)\n" ;first argument is fraction
		"mov rax, [rax]\n"
		"mov qword r10, rax\n"
		"sar r10, 34\n"
		"add r10, start_of_data\n"
		"mov r10, [r10]\n"
		"sar r10, 4\n"
		"sal r10, 34\n"
		"mov qword rbx, rax\n"
		"sal rbx, 30\n"
		"sar rbx, 34\n"
		"add rbx, start_of_data\n"
		"mov rbx, [rbx]\n"
		"sar rbx, 4\n"
		"sal rbx, 4\n"
		"or r10, rbx\n"
		"or r10, T_FRACTION\n"


		"check_div_second:\n"
		"mov rax, An(1)\n"
		"mov rax, [rax]\n"
		"TYPE rax\n"
		"cmp rax, T_INTEGER\n"
		"jne second_div_fraction\n"
		"mov rcx, An(1)\n"
		"mov rcx, [rcx]\n"
		"sar rcx, 4\n"
		"mov r11,0\n"
		"or r11, rcx\n"
		"sal r11, 30\n"
		"mov qword rcx, 1\n"
		"or r11, rcx\n"
		"sal r11, 4\n"
		"or r11, T_FRACTION\n"
		"jmp div_exit\n"

		"second_div_fraction:\n"
		"mov rax, An(1)\n" ;second argument is fraction
		"mov rax, [rax]\n"
		"mov qword r11, rax\n"
		"sar r11, 34\n"
		"add r11, start_of_data\n"
		"mov r11, [r11]\n"
		"sar r11, 4\n"
		"sal r11, 34\n"
		"mov qword rbx, rax\n"
		"sal rbx, 30\n"
		"sar rbx, 34\n"
		"add rbx, start_of_data\n"
		"mov rbx, [rbx]\n"
		"sar rbx, 4\n"
		"sal rbx, 4\n"
		"or r11, rbx\n"
		"or r11, T_FRACTION\n"

		"div_exit:\n"
		"mov qword rax, r10\n"
		"sar rax, 34\n"
		"mov qword rbx, r11\n"
		"sal rbx, 30\n"
		"sar rbx, 34\n"
		"imul rbx\n"
		"mov qword rbx, rax\n" ; mone*mehane in rbx
		"mov qword rax, r10\n"
		"sal rax, 30\n"
		"sar rax, 34\n"
		"mov qword rcx, r11\n"
		"sar rcx, 34\n"
		"imul rcx\n"
		"mov qword rcx, rax\n" ; mehane*mone in rcx

		"cmp rcx, 0\n"
        "jge cont2\n"
		"neg rcx\n"
		"neg rbx\n"

		"cont2:\n"

		"cmp rbx, 0\n"
		"jne check_div_mehane\n"
		"my_malloc 8\n"
		"sal rbx, 4\n"
        "or rbx, T_INTEGER\n"
		"mov qword [rax], rbx\n"
		"jmp div_exit_forsure\n"

        "check_div_mehane:\n"
		"cmp rcx, 1\n"
		"jne check_div_gcd\n"
		"my_malloc 8\n"
		"sal rbx, 4\n"
        "or rbx, T_INTEGER\n"
		"mov qword [rax], rbx\n"
		"jmp div_exit_forsure\n"

		"check_div_gcd:\n"
		"add rsp, 1*8\n"
		"push rbx\n"
		"push rcx\n"
		
		"call gcd\n"
		"add rsp, 2*8\n"
		"mov qword r12, rax\n" ; now return value in r12

		"mov qword rax, rbx\n"
		"cqo\n"
		"idiv r12\n"
		"mov qword rbx, rax\n"

		"mov qword rax, rcx\n"
		"cqo\n"
		"idiv r12\n"
		"mov qword rcx, rax\n"

		"cmp rcx, 1\n"
		"jne not_div_int_forsure\n"
		"sal rbx, 4\n"
        "or rbx, T_INTEGER\n"
        "my_malloc 8\n"
		"mov qword [rax], rbx\n"
		"jmp div_exit_forsure\n"

		"not_div_int_forsure:\n"
		"sal rbx, 4\n"
        "or rbx, T_INTEGER\n"
		"my_malloc 8\n"
		"mov [rax], rbx\n"
		"mov rbx, rax\n"
		"sub rbx, start_of_data\n"
		"sal rbx, 34\n"

		"sal rcx, 4\n"
        "or rcx, T_INTEGER\n"
		"my_malloc 8\n"
		"mov [rax], rcx\n"
		"mov rcx, rax\n"
		"sub rcx, start_of_data\n"
		"sal rcx, 4\n"
		"or rbx, rcx\n"
		"or rbx, T_FRACTION\n"
		"my_malloc 8\n"
		"mov [rax], rbx\n"

		"div_exit_forsure:\n"
		"pop r11\n"
		"pop rbx\n"
		"pop r10\n"
		"pop rcx\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"not_code:\n"
		"push rbp\n" 
		"mov rbp, rsp\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n" 
		"TYPE rax\n"
		"cmp rax, T_BOOL\n"
		"je its_bool\n"
		"mov rax , sobBoolFalse\n"
		"jmp not_exit\n"
		"its_bool:\n"
		"mov qword rax, An(0)\n" 
		"mov rax, [rax]\n"
		"DATA rax\n"
		"cmp rax, 1\n"
		"je its_false\n"
		"mov rax, sobBoolTrue\n"
		"jmp not_exit\n"
		"its_false:\n"
		"mov rax, sobBoolFalse\n"
		"not_exit:\n"	
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"remainder_code:\n"
		"push rbp\n" 
		"mov rbp, rsp\n"
		"push rbx\n"
		"push rdx\n"
		"push r11\n"
		"mov qword r11, 1\n"
		"mov rax, An(0)\n" 
		"mov rax, [rax]\n" 
		"sar rax, 4\n"
		"cmp rax, 0\n"
		"jge cont\n"
		"neg rax\n"
		"mov qword r11, 0\n"
		"cont:\n"
		"mov rbx, An(1)\n" 
		"mov rbx, [rbx]\n" 
		"sar rbx, 4\n"
		"neg rbx\n"
		"mov qword rdx, 0\n"
		"idiv rbx\n"
		"mov qword rax, rdx\n"
		"cmp r11, 0\n"
		"jne was_positive\n"
		"neg rax\n" 
		"was_positive:\n"
		"sal rax, 4\n"
		"or rax, T_INTEGER\n"
		"mov qword rbx, rax\n"
		"my_malloc 8\n"
		"mov [rax], rbx\n"
		"pop r11\n"
		"pop rdx\n"
		"pop rbx\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"symbol_to_string_code:\n"
		"push rbp\n" 
		"mov rbp, rsp\n"

		"mov rax, An(0)\n" 
		"mov rax, [rax]\n"
		"shr rax, 4\n"

		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"eq?_code:\n"
		"push rbp\n" 
		"mov rbp, rsp\n"
		"push rbx\n"
		"mov rax, An(0)\n" 
		"mov rbx, An(1)\n"
		"mov rax, qword [rax]\n"
		"mov rbx, qword [rbx]\n"
		"TYPE rax\n"
		"TYPE rbx\n"
		"cmp rax, rbx\n"
		"jne eq?_false\n"

		"mov rax, An(0)\n" 
		"mov rbx, An(1)\n"
		"mov rax, qword [rax]\n"
		"mov rbx, qword [rbx]\n"
		"DATA rax\n"
		"DATA rbx\n"
		"cmp rax, rbx\n"
		"je equal\n"
		"eq?_false:\n"
		"mov rax , sobBoolFalse\n"
		"jmp eq?_exit\n"
		"equal:\n"
		"mov rax, sobBoolTrue\n"
		"eq?_exit:\n"
		"pop rbx\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"

		"vector_code:\n"
		"push rbp\n" 
		"mov rbp, rsp\n"
		"push r9\n"
		"push rcx\n"
		"push r11\n"
		"push r12\n"
		"push r13\n"
		"mov r9, [rbp+24]\n"
		"mov rax, r9\n"
		"mov qword r12, 8\n"
		"imul r12\n"
		"mov qword r13, rax\n"
		"my_malloc r13\n"
		"mov qword r12, rax\n"
		"mov rcx, 0\n"
		"start:\n"
		"cmp rcx, r9\n"
		"je my_vector_exit\n"
		"mov qword r11, An(rcx)\n" 
		"mov [r12+rcx*8], r11\n"
		"add rcx, 1\n"
		"jmp start\n"
		"my_vector_exit:\n"
		"my_malloc 8\n"
		"mov qword r13, r9\n"
		"shl r13, 30\n"
		"sub qword r12, start_of_data\n"
		"or r13, r12\n"
		"shl r13, 4\n"
		"or r13, T_VECTOR\n"
		"mov [rax], r13\n"
		"pop r13\n"
		"pop r12\n"
		"pop r11\n"
		"pop rcx\n"
		"pop r9\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"
		
		"vector_length_code:\n"
		"push rbp\n" 
		"mov rbp, rsp\n"
		"push rbx\n"
		"mov rbx, An(0)\n" 
        "mov rbx, [rbx]\n"
        "shr rbx, 34\n"
        "my_malloc 8\n"
        "shl rbx, 4\n"
        "or rbx, T_INTEGER\n"
        "mov [rax], rbx\n"
        "pop rbx\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"
		
		"string_length_code:\n"
		"push rbp\n" 
		"mov rbp, rsp\n"
		"push rbx\n"
		"mov rbx, An(0)\n" 
        "mov rbx, [rbx]\n"
        "shr rbx, 34\n"
        "my_malloc 8\n"
        "shl rbx, 4\n"
        "or rbx, T_INTEGER\n"
        "mov [rax], rbx\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"pop rbx\n"
		"leave\n"
		"ret\n"
		
		"vector_ref_code:\n"
		"push rbp\n" 
		"mov rbp, rsp\n"
		"push rbx\n"
		"push rcx\n"
		"push r11\n"
		"mov rbx, An(0)\n" 
		"mov rcx, An(1)\n"
        "mov rbx, [rbx]\n"
        "shl rbx, 30\n"
        "shr rbx, 34\n"
        "mov rcx, [rcx]\n"
        "shr rcx, 4\n"
        "add qword rbx, start_of_data\n"
        "mov qword r11, [rbx+8*rcx]\n"
        "mov r11, [r11]\n"
        "my_malloc 8\n"
        "mov qword [rax], r11\n"
        "pop r11\n"
		"pop rcx\n"
		"pop rbx\n"
		"mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"leave\n"
		"ret\n"
		
		"string_ref_code:\n"
		"push rbp\n" 
		"mov rbp, rsp\n"
		"push rbx\n"
		"push rcx\n"
		"push r11\n"
		"mov rbx, An(0)\n" 
		"mov rcx, An(1)\n"
        "mov rbx, [rbx]\n"
        "STRING_ELEMENTS rbx\n"
        "mov rcx, [rcx]\n"
        "shr rcx, 4\n"
        "mov r11, qword [rbx + rcx]\n"
        "shl r11, 56\n"
        "shr r11, 56\n"
        "shl r11, 4\n"
        "or r11, T_CHAR\n"
        "my_malloc 8\n"
        "mov [rax], r11\n"
        "mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"pop r11\n"
		"pop rcx\n"
		"pop rbx\n"
		"leave\n"
		"ret\n"
		
		"vector_set_code:\n"
		"push rbp\n" 
		"mov rbp, rsp\n"
		"push rbx\n"
		"push rcx\n"
		"push r12\n"
		"push r13\n"
		"mov rbx, An(0)\n" 
		"mov rcx, An(1)\n"
		"mov r12, An(2)\n"
        "mov rbx, [rbx]\n"
        "shl rbx, 30\n"
        "shr rbx, 34\n"
        "add qword rbx, start_of_data\n"
        "mov rcx, [rcx]\n"
        "shr rcx, 4\n"
        "mov qword [rbx+rcx*8],r12\n"
        "my_malloc 8\n"
		"mov qword r13, An(0)\n"
		"mov r13, [r13]\n"
		"shr r13, 34\n"
		"shl r13, 30\n"
		"sub qword rbx, start_of_data\n"
		"or r13, rbx\n"
		"shl r13, 4\n"
		"or r13, T_VECTOR\n"
		"mov [rax], r13\n"
		"mov rax, sobVoid\n"
        "mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"pop r13\n"
		"pop r12\n"
		"pop rcx\n"
		"pop rbx\n"
		"leave\n"
		"ret\n"
		
		"string_set_code:\n"
		"push rbp\n" 
		"mov rbp, rsp\n"
		"push rbx\n"
		"push rcx\n"
		"push r12\n"
		"push r13\n"
		"mov rbx, An(0)\n" 
		"mov rcx, An(1)\n"
		"mov r12, An(2)\n"
        "mov rbx, [rbx]\n"       
        "shl rbx, 30\n"
        "shr rbx, 34\n"
        "add qword rbx, start_of_data\n"
        "mov r12, [r12]\n"
        "shr r12, 4\n"
        "mov rcx, [rcx]\n"
        "shr rcx, 4\n"
        "mov [rbx+rcx], r12b\n"
		"mov  r13, An(0)\n"
		"mov r13, [r13]\n"
		"shr r13, 34\n"
		"shl r13, 30\n"
		"sub rbx, start_of_data\n"
		"or r13, rbx\n"
		"shl r13, 4\n"
		"or r13, T_STRING\n"
		"my_malloc 8\n"
		"mov [rax], r13\n"
		"mov rax, sobVoid\n"
        "mov r15, [rbp + 24]\n"
		"add r15, 3\n"
		"imul r15, r15, 8\n"
		"pop r13\n"
		"pop r12\n"
		"pop rcx\n"
		"pop rbx\n"
		"leave\n"
		"ret\n"

	)
)

(define rt-closures-make
	(lambda(fvar-tab)
		(string-append
			"make_boolean?_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'boolean? fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'boolean? fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], boolean_code\n" (calc-index-fvar 'boolean? fvar-tab 0))

			"make_make_string_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'make-string fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'make-string fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], make_string_code\n" (calc-index-fvar 'make-string fvar-tab 0))

			"make_make_vector_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'make-vector fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'make-vector fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], make_vector_code\n" (calc-index-fvar 'make-vector fvar-tab 0))

			"string_to_symbol_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'string->symbol fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'string->symbol fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], string_to_symbol_code\n" (calc-index-fvar 'string->symbol fvar-tab 0))

			"make_vector_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'vector fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'vector fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], vector_code\n" (calc-index-fvar 'vector fvar-tab 0))
			
            "make_vector_ref_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'vector-ref fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'vector-ref fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], vector_ref_code\n" (calc-index-fvar 'vector-ref fvar-tab 0))
			
			"make_vector_set_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'vector-set! fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'vector-set! fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], vector_set_code\n" (calc-index-fvar 'vector-set! fvar-tab 0))
			
			"make_string_set_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'string-set! fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'string-set! fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], string_set_code\n" (calc-index-fvar 'string-set! fvar-tab 0))
			
			
            "make_string_ref_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'string-ref fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'string-ref fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], string_ref_code\n" (calc-index-fvar 'string-ref fvar-tab 0))
			
			"make_vector_length_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'vector-length fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'vector-length fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], vector_length_code\n" (calc-index-fvar 'vector-length fvar-tab 0))
			
			"make_string_length_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'string-length fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'string-length fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], string_length_code\n" (calc-index-fvar 'string-length fvar-tab 0))

			"make_not_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'not fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'not fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], not_code\n" (calc-index-fvar 'not fvar-tab 0))

			"make_eq?_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'eq? fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'eq? fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], eq?_code\n" (calc-index-fvar 'eq? fvar-tab 0))

			"make_remainder_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'remainder fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'remainder fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], remainder_code\n" (calc-index-fvar 'remainder fvar-tab 0))

			"make_symbol_to_string_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'symbol->string fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'symbol->string fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], symbol_to_string_code\n" (calc-index-fvar 'symbol->string fvar-tab 0))

			"make_string?_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'string? fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'string? fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], string?_code\n" (calc-index-fvar 'string? fvar-tab 0))

			"make_char?_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'char? fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'char? fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], char?_code\n" (calc-index-fvar 'char? fvar-tab 0))

			"make_integer?_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'integer? fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'integer? fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], integer?_code\n" (calc-index-fvar 'integer? fvar-tab 0))

			"make_symbol?_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'symbol? fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'symbol? fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], symbol?_code\n" (calc-index-fvar 'symbol? fvar-tab 0))

			"make_vector?_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'vector? fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'vector? fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], vector?_code\n" (calc-index-fvar 'vector? fvar-tab 0))

			"make_pair?_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'pair? fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'pair? fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], pair?_code\n" (calc-index-fvar 'pair? fvar-tab 0))

			"make_zero?_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'zero? fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'zero? fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], zero?_code\n" (calc-index-fvar 'zero? fvar-tab 0))

			"make_null?_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'null? fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'null? fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], null?_code\n" (calc-index-fvar 'null? fvar-tab 0))

			"make_procedure?_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'procedure? fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'procedure? fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], procedure?_code\n" (calc-index-fvar 'procedure? fvar-tab 0))

			"make_car_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'car fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'car fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], car_code\n" (calc-index-fvar 'car fvar-tab 0))

			
			"make_number?_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'number? fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'number? fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], number?_code\n" (calc-index-fvar 'number? fvar-tab 0))

			"make_rational?_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'rational? fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'rational? fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], number?_code\n" (calc-index-fvar 'rational? fvar-tab 0))

			"make_cdr_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'cdr fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'cdr fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], cdr_code\n" (calc-index-fvar 'cdr fvar-tab 0))

			"make_cons_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'cons fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'cons fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], cons_code\n" (calc-index-fvar 'cons fvar-tab 0))

			"make_char_to_integer_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'char->integer fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'char->integer fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], char_to_integer_code\n" (calc-index-fvar 'char->integer fvar-tab 0))

			"make_integer_to_char_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'integer->char fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'integer->char fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], integer_to_char_code\n" (calc-index-fvar 'integer->char fvar-tab 0))

			"make_denominator_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'denominator fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'denominator fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], denominator_code\n" (calc-index-fvar 'denominator fvar-tab 0))

			"make_numerator_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'numerator fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'numerator fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], numerator_code\n" (calc-index-fvar 'numerator fvar-tab 0))

			"make_mult_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar '_* fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar '_* fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], mult_code\n" (calc-index-fvar '_* fvar-tab 0))

			"make_plus_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar '_+ fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar '_+ fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], plus_code\n" (calc-index-fvar '_+ fvar-tab 0))

			"make_minus_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar '_- fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar '_- fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], minus_code\n" (calc-index-fvar '_- fvar-tab 0))

			"make_div_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar '_/ fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar '_/ fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], div_code\n" (calc-index-fvar '_/ fvar-tab 0))

			"make_nagative?_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar '_negative? fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar '_negative? fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], negative?_code\n" (calc-index-fvar '_negative? fvar-tab 0))

			"make_apply_closure:\n"
			"mov rax, 0x55555\n"
			"sal rax, 30\n"
			(format "or rax, L_fvar_~a + 8 - start_of_data\n" (calc-index-fvar 'apply fvar-tab 0))
			"sal rax, 4\n"
			"or rax, T_CLOSURE\n"
			(format "mov qword [L_fvar_~a], rax\n" (calc-index-fvar 'apply fvar-tab 0))
			(format "mov qword [L_fvar_~a + 8], apply_code\n" (calc-index-fvar 'apply fvar-tab 0))
		)
	)
)



(define rt-support-funcs 
	'(_append apply < = > _+ _/ _* _- boolean? car cdr char->integer char? cons denominator eq? integer? integer->char list make-string make-vector
		not null? number? numerator pair? procedure? rational? remainder set-car! set-cdr! string-length string-ref string-set! string->symbol
		string? symbol? symbol->string vector vector-length vector-ref vector-set! vector? zero? _negative?))

(define find-all-fvars1 ;;gets all the consts from a single expression
	(lambda (lst acc) 
	    (if (and (list? lst) (not (null? lst)) (equal? (car lst) 'fvar))
	    	(append acc  (cdr lst))
	         (if (and (not (null? lst)) (list? lst))
			 	`(,@(find-all-fvars1 (car lst) acc) ,@(find-all-fvars1 (cdr lst) acc))
			     acc
			  )
	    )
        
	)
)

(define find-all-fvars ;wraps finds-all-consts1 to make us able to use map
	(lambda(expr)
		(find-all-fvars1 expr '())
		))


(define find-all-fvars-list ;;recieves a list of parsed sexprs and gets all the consts from all list members
	(lambda(expr-list)
		(apply append (map find-all-fvars expr-list))
	)
)

(define file->fvar-set ;recieves file name (as string) and returns fvar-set
	(lambda(input_file)
		(list->set (append rt-support-funcs (find-all-fvars (sexpr-list->parsed-sexpr-list (str->sexpr (file->string input_file))))))
	)
)

(define fvar-set->fvar-tab
	(lambda(fvar-set index)
		(if (null? fvar-set) '()  
				(append (list (list (car fvar-set) index)) (fvar-set->fvar-tab (cdr fvar-set) (+ 1 index)))
		)
	)
)

(define file->fvar-tab
	(lambda(input_file)
		(let ((fvar-set (file->fvar-set input_file)))
			(fvar-set->fvar-tab fvar-set 0)			
		)
	)
)

(define fvar->code 
	(lambda(fvar-tab)
		(if (null? fvar-tab) ""
				(string-append
					(format "L_fvar_~a:\n" (cadar fvar-tab))
					"dq 0, 0\n"
					(fvar->code (cdr fvar-tab))
				)
		)
	)
)

(define calc-index-fvar
	(lambda(item fvar-tab index)
		(if (null? fvar-tab) 'error-index-not-found
			(if (equal? item (caar fvar-tab)) index
					(calc-index-fvar item (cdr fvar-tab) (+ 1 index))
			)
		)
	)
)

(define write-to-file
  (lambda (input_file filename)
    (let ((output (open-output-file filename)))
    	(begin 
    		(set! sorted-set (file->sorted-const-set input_file))
    		(set! fvar-tab (file->fvar-tab input_file))
    		(write (display 
    					(string-append 
    						start-string
    						(const-table->code (file->const-table input_file) "")
    						(fvar->code fvar-tab)
    						pre-code-string
    						(make-symbol-table-strings sorted-set sorted-set)
    						(rt-closures-make fvar-tab)
    						(code-gen (file->parsed-sexpr-list input_file))
    						"ret\n\n"
    						rt-support-code	
    						end-string 
    					)
    			output)) 
    	(close-output-port output)
))))

(write-to-file "input.scm" "output.s")
(exit)