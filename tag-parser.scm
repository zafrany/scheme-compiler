(load "qq.scm")

(define *reserved-words*
'(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))

(define reserved-word?
	(lambda (x)
		(member x *reserved-words*)
	)
)

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(define else-cond
  (lambda (exp1)
    (eq? (car exp1) 'else)))

(define var?
	(lambda(var)
		(and (symbol? var) (not (reserved-word? var)))
	)
)

(define void-object (list 'const (void)))

(define (has-duplicates? lst) ;returns #t if list have no duplicates
		  (cond 
		  	  ((list? lst) (has-duplicates-proper? lst))
		  	  ((pair? lst) (has-duplicates-proper? (flatten lst))) ; if not proper list then convert to one using flatten
		  	  (else #t)
		  	  ))


(define (has-duplicates-proper? lst) ;returns #t if list have no duplicates
		  (cond
		     ((null? lst) #t) 
		     ((not (not (member (car lst) (cdr lst)))) #f)
		     (else (has-duplicates-proper? (cdr lst)) )))

(define identify-lambda
  (lambda (argl ret-simple ret-opt ret-var)
    (cond 
        ((null? argl) (ret-simple '()))
        ((var? argl) (ret-var argl))
        (else 
          (identify-lambda
            (cdr argl)
            (lambda (s) (ret-simple `(,(car argl) ,@s))) ;simple
            (lambda (s opt) (ret-opt `(,(car argl) ,@s) opt)) ;opt
            (lambda (var) (ret-opt `(,(car argl)) var))) ;variadic
))))


(define list_or_pair?
	(lambda (lst)
		(or (pair? lst) (list? lst))))

(define remove-inner-begins
  (lambda(exp)
    (cond
      ((eq? exp '()) '())
      ((eq? (caar exp) 'seq) `(,@(remove-inner-begins (cadar exp)) ,@(remove-inner-begins (cdr exp))))
      (else `(,(car exp) ,@(remove-inner-begins (cdr exp))))
    )))
	
(define let-rec-helper
	(lambda (binding) 
		`(set! ,(car binding) ,@(cdr binding))))

(define let-rec-helper-applic
	(lambda (binding lst)
		(if (null? binding) lst 
			(let-rec-helper-applic (cdr binding)  (append '(#f) lst)))))

(define parse
  (lambda (sexpr)
    (cond
      ((var? sexpr) (list 'var sexpr))
      
      ((and (null? sexpr) (not (eq? sexpr '()))) void-object)
      
      ((or (boolean? sexpr) (char? sexpr) (number? sexpr) (string? sexpr) (vector? sexpr) (eq? sexpr '()))  (list 'const sexpr))
      
      ((and (list? sexpr) (eq? (car sexpr) 'quote)) 
			  (if (null? (cdr sexpr)) 'ERROR
			  (list 'const (cadr sexpr))))
      
      ((and (list? sexpr) (eq? (car sexpr) 'quasiquote)) (parse (expand-qq (cadr sexpr))))

      ((eq? (car sexpr) 'if)
                          (cond ((and (not (null? (cdr sexpr))) (not (null? (cadr sexpr))) (not (null? (cddr sexpr))) (not (null? (caddr sexpr))) (null? (cdddr sexpr)))
			        (list 'if3 (parse (cadr sexpr)) (parse (caddr sexpr)) void-object))
                                              
			        ((and (not (null? (cdr sexpr))) (not (null? (cadr sexpr))) (not (null? (cddr sexpr))) (not (null? (caddr sexpr))) (not (null? (cadddr sexpr))) (null? (cddddr sexpr)))
                                (list 'if3 (parse (cadr sexpr)) (parse (caddr sexpr)) (parse (cadddr sexpr))))
                                                  
				(else 'ERROR)))
      
      ((eq? (car sexpr) 'or)
				   (cond ((null? (cdr sexpr)) (parse '#f))
						 ((null? (cddr sexpr)) (parse (cadr sexpr)))
					 (else (list 'or (map parse (cdr sexpr))))))

      ((eq? (car sexpr) 'lambda)
         (if (eq? '() (cddr sexpr)) '(error: lambda body have no expressions!)
	 	     (let (
                (vars (cadr sexpr)) 
                (body (cddr sexpr))
              )
      		(if (has-duplicates?  vars)
                        (identify-lambda 
                          vars 
                          (lambda (vars) `(lambda-simple ,vars ,(parse `(begin ,@body))))    ;simple pattern
                          (lambda (vars1 vars2) `(lambda-opt ,vars1 ,vars2 ,(parse `(begin ,@body))))    ;opt pattern
                          (lambda (vars) `(lambda-opt () ,vars ,(parse `(begin ,@body)))) ;variadic pattern
                        ) 
      					          '(error: lambda vars have duplicates)
                                                                     ))))

      ;MIT define 
      ((and (eq? (car sexpr) 'define) (list_or_pair?  (cadr sexpr)))
       (let (
                (vars (cadr sexpr)) 
                (exprs (cddr sexpr))
            )
							   (parse (list 'define (car vars) `(lambda ,(cdr vars) ,@exprs)))
      ))
     
                    
      ((eq? (car sexpr) 'define)
        `(define ,(parse (cadr sexpr)) ,(parse (caddr sexpr))))

      ;sequence
      ((eq? (car sexpr) 'begin)
	       (if (null? (cdr sexpr)) `(const ,(void)) 
            (if (null? (cddr sexpr)) (parse (cadr sexpr)) `(seq ,(remove-inner-begins `(,(parse (cadr sexpr)) ,@(map parse (cddr sexpr))))))))
             

      ;and
      ((eq? (car sexpr) 'and)
        (let ((expr1 (cdr sexpr)))
          (if (eq? expr1 '()) (parse #t)
              (if (eq? (cdr expr1) '()) (parse (car expr1))
                `(if3 ,(parse (car expr1)) ,(parse `(and ,@(cdr expr1))) ,(parse #f))))))

      ;cond
      ((eq? (car sexpr) 'cond)
        (let ((expr-list (cdr sexpr)))
          (if (else-cond (car expr-list)) (parse `(begin ,@(cdar expr-list)))
            (if (eq? (cdr expr-list) '()) (parse `(if ,(caar expr-list) (begin ,@(cdar expr-list))))
              (parse `(if ,(caar expr-list) (begin ,@(cdar expr-list)) ,`(cond ,@(cdr expr-list))))))))



      ;set!
      ((eq? (car sexpr) 'set!)
       (let (
             (name (cadr sexpr))
             (body (caddr sexpr))

             )
         `(set ,(parse name) ,(parse body))))


      ;let
      ((eq? (car sexpr) 'let)
        (if (eq? (cadr sexpr) '()) 
          (parse `((lambda () ,@(cddr sexpr))))
		  
		  
		  (let 
				(
				 (binding (caadr sexpr))
				 (other-bindings (cdadr sexpr))
				 (body (caddr sexpr))
				 (other-bodies (cdddr sexpr))
				) 
					 (parse `((lambda (,(car binding) ,@(map  (lambda (binding) (car binding)) other-bindings)) ,body ,@other-bodies)
							,(cadr binding) ,@(map cadr other-bindings))))))

    
      ;empty let*
      ((and (eq? (car sexpr) 'let*) (eq? (cadr sexpr) '()))
           (parse `(begin ,(caddr sexpr) ,@(cdddr sexpr))))

      ;let*
      ((and (eq? (car sexpr) 'let*) (var? (caaadr sexpr)))

    		(let
    			(
    			(key (caaadr sexpr))
    			(value (car (cdaadr sexpr)))
    			(other-bindings (cdadr sexpr))
    			(body (cddr sexpr))
    			
    			)
    					(parse `(let ((,key ,value)) 
    					,(list 'let* other-bindings `(begin ,@body))))))

      ;letrec with empty bindings
      ((and (eq? (car sexpr) 'letrec) (eq? '() (cadr sexpr)))

         (let 
            (
             (binding (cadr sexpr))
             (body (cddr sexpr))
            )
            
             (parse
                (append 
                   `(
                      (lambda ()
                           ((lambda () (begin ,@body))))
                    )
                )
              )
            
         )
      )

      ;letrec
      ((eq? (car sexpr) 'letrec)
	  
    	   (let 
    				(
    				 (binding (caadr sexpr))
    				 (other-bindings (cdadr sexpr))
    				 (body (cddr sexpr))
    				 
    				)
    				
    				 (parse
    						(append 
    							 `(
    							    (lambda (,(car binding) ,@(map  (lambda (x) (car x)) other-bindings))  ;lambda name binding
    							  			,(let-rec-helper binding) ,@(map let-rec-helper other-bindings)    ;set! bindings
    							 			   ((lambda () (begin ,@body))))
    							  )
    							 			 (let-rec-helper-applic other-bindings '(#f)) ;appending #f's according to lambda num vars for application
    						)
              )
    				
    	   )
	    )

      
      ;applic
      (else (list 'applic (parse (car sexpr)) (map parse (cdr sexpr))))
      

      

       

      
      
      

      
      
      
   	
					
			
 )))
