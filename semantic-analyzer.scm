(define remove-applic-lambda-nil
  (lambda(exp)
  	(cond
  		((not (list? exp)) exp)
  		((null? exp) '() )
  		((and (eq? (car exp) 'applic) 
			  (eq? (caadr exp) 'lambda-simple)
			  (null? (cadadr exp))
			  (null? (caddr exp))
		 ) 
  					(remove-applic-lambda-nil (car (cddadr exp))))
    	(else `(,(remove-applic-lambda-nil (car exp)) ,@(remove-applic-lambda-nil (cdr exp))))
 )))


(define replace-set  ;recieves input of the form (var x)
	(lambda (var_name)
		`(box ,@`(,var_name))
		))

(define list-diff0 
	(lambda(lst1 lst2)
	  (cond ((null? lst1) '())
	        ((not (member (car lst1) lst2)) (cons (car lst1) (list-diff0 (cdr lst1) lst2)))
	        (else (list-diff0 (cdr lst1) lst2)))))

(define list-diff
	(lambda(lst1 lst2)
		`(,@(list-diff0 lst1 lst2) ,@(list-diff0 lst2 lst1))
		))

(define list-union
	(lambda(var lst)
		(if (member var lst) lst `(,var ,@lst))
		))

(define list-intersection 
	(lambda(lst1 lst2)
	  (if (null? lst1) '()
	      (if (member (car lst1) lst2)
	          (cons (car lst1) (list-intersection (cdr lst1) lst2))
	          (list-intersection (cdr lst1) lst2)))))


(define remove-duplicates 
	(lambda(lst)
  		(fold-right (lambda(x y) 
  			(cons x (filter (lambda(z) (not (equal? x z))) y))) '() lst)))

(define remove-empties
	(lambda(exp)
		(cond
			((or (not (list? exp)) (eq? exp '() ) ) exp)
			((eq? (car exp) '() )  (remove-empties (cdr exp) ))
			(else `(,(car exp) ,@(remove-empties (cdr exp)))))
		))

(define vars-levels
	(lambda(bindings level) 
			(remove-empties (map (lambda (binding) (if (eq? (cadr binding) level) (car binding) '())) bindings))
		))

(define generate-sets
	(lambda(lst)
		(map (lambda(x) `(set (var ,x) (box (var ,x)))) lst)
		))

(define find-bounds  ;return all bound variables that have a 'get' instance
	(lambda (exp lst_base lst_not_base bounds)
		(cond 
		  ((or (null? exp) (not (list? exp))) '()) 
		  ((list? (car exp)) `(,@(find-bounds (car exp) lst_base lst_not_base bounds) ,@(find-bounds (cdr exp) lst_base lst_not_base bounds)))
		  ((or (eq? (car exp) 'lambda-var) (eq? (car exp) 'lambda-simple)) (find-bounds (caddr exp) (cadr exp) (list-diff (list-diff lst_base (cadr exp)) (cadr exp)) bounds))
		  ((eq? (car exp) 'bvar) (if (member (cadr exp) lst_not_base) (list-union (cadr exp) bounds) bounds))
		  (else (find-bounds (cdr exp) lst_base lst_not_base bounds))
		)))	

(define find-sets  ;return all bound variables that have a 'set' instance with their level
	(lambda (exp lst_set level)
		(cond 
		  ((or (null? exp) (not (list? exp))) '()) 
		  ((list? (car exp)) `(,@(find-sets (car exp) lst_set level) ,@(find-sets (cdr exp) lst_set level)))
		  ((or (eq? (car exp) 'lambda-var) (eq? (car exp) 'lambda-simple)) (find-sets (caddr exp) lst_set (+ 1 level)))
		  ((eq? (car exp) 'set) `(,`(,(cadadr exp) bound) ,@(find-sets (cdr exp) lst_set level)))
		  ((eq? (car exp) 'set) `(,`(,(cadadr exp) ,level) ,@(find-sets (cdr exp) lst_set level)))
		  (else (find-sets (cdr exp) lst_set level))
		)))

(define calc-bind-list ;list of bindings to replace
	(lambda(bindings binding_levels)
		 (remove-empties
			(map (lambda(binding_levels) (if (not (member (car binding_levels) bindings)) '() binding_levels)) binding_levels))
		))

(define get-all-params ;call with lvl = 0 and parsed exp
	(lambda(exp level)
		(cond 
		  ((or (null? exp) (not (list? exp))) '())
		  ((or (eq? (car exp) 'lambda-var) (eq? (car exp) 'lambda-simple)) `( ,`(,(cadr exp) ,level) ,@(get-all-params (cdr exp) (+ level 1))))
		  ((list? exp) `(,@(get-all-params (car exp) level) ,@(get-all-params (cdr exp) level)))
		  (else (get-all-params (cdr exp) level))
		)
	))

(define reverse-list
	(lambda(lst)
  		(if (null? lst) '()
     	 (append (reverse-list (cdr lst)) (list (car lst))))
))

(define return-level-if-member
	(lambda(param param-list1)
		(cond
		  ((member param (caar param-list1)) `(,param ,(cadar param-list1)))
		  (else (return-level-if-member param (cdr param-list1)))
		  )))

(define fix-var-list0	;find the highest level occurance of param that is gonna be 'set'
	(lambda(param-list)
		(lambda(binding)
		  (if (eq? (cadr binding) 'bound) (return-level-if-member (car binding) param-list) binding) 
		)))

(define fix-var-list
	(lambda(var_list param-list)
		(remove-duplicates (map (fix-var-list0 param-list) var_list))
		))

(define box-constraints-vars  ;find all the vars that satisfy all 3 constraints ; exp is parsed; afterwards fix in case of (x bound)
	(lambda(exp)
		(fix-var-list 
		  (calc-bind-list 
			(remove-duplicates 
				(list-intersection 
					(find-bounds (replace-map0 exp '() '()) '() '() '()) 
					(map car (find-sets (replace-map0 exp '() '()) '() -1)))) 
				(find-sets (replace-map0 exp '() '()) '() -1))
		 (reverse-list (get-all-params exp 0)))))

(define replace-map0  ;recplace all instancrs of (var x) with bvar or pvar
 	(lambda(exp params bounds)
 		(cond
 			((eq? exp '()) '())
 			((not (list? exp)) exp)
 			((eq? (car exp) 'lambda-simple)
 				`(lambda-simple ,(cadr exp) ,(replace-map0 (caddr exp) (cadr exp) (list-diff (remove-duplicates (append params bounds)) (cadr exp)))))
 			((eq? (car exp) 'lambda-var)
 				`(lambda-simple ,(cadr exp) ,(replace-map0 (caddr exp) (cadr exp) (list-diff (remove-duplicates (append params bounds)) (cadr exp)))))
 			((and (eq? (car exp) 'var) (member (cadr exp) params)) `(pvar ,(cadr exp)))
 			((and (eq? (car exp) 'var) (member (cadr exp) bounds)) `(bvar ,(cadr exp)))
 			(else `(,(replace-map0 (car exp) params bounds) ,@(replace-map0 (cdr exp) params bounds)))

		)))

(define get-all-vars-levels ;get all the occurances that satisfy the constraints from 0 - lvl
	(lambda(bindings lvl)
		(if (eq? lvl -1) '() `(,@(vars-levels bindings lvl) ,@(get-all-vars-levels bindings (- lvl 1))))
	))

(define replace-map  ;do the final replacment, intial level : -1
 	(lambda(exp bindings level)
 		(cond
 			((eq? exp '())  '())
 			((not (list? exp)) exp)

 			((and 
 				(eq? (car exp) 'lambda-simple) 
 				(ormap (lambda(x) (member x (vars-levels bindings (+ level 1)))) (cadr exp))
 				(not (eq? (caaddr exp) 'seq)))  
 			  `(lambda-simple ,(cadr exp) (seq (,@(generate-sets (vars-levels bindings (+ level 1))) ,(replace-map (caddr exp) bindings (+ 1 level))))))

 			((and 
 				(eq? (car exp) 'lambda-var) 
 				(ormap (lambda(x) (member x (vars-levels bindings (+ level 1)))) (cadr exp))
 				(not (eq? (caaddr exp) 'seq)))  
 			  `(lambda-simple ,(cadr exp) (seq (,@(generate-sets (vars-levels bindings (+ level 1))) ,(replace-map (caddr exp) bindings (+ 1 level))))))

 			((and (eq? (car exp) 'lambda-simple) (ormap (lambda(x) (member x (vars-levels bindings (+ level 1)))) (cadr exp)))  
 			  `(lambda-simple ,(cadr exp) (seq (,@(generate-sets (vars-levels bindings (+ level 1))) ,@(replace-map (car (cdaddr exp)) bindings (+ 1 level))))))
 			((and (eq? (car exp) 'lambda-simple) (not (member (cadr exp) (vars-levels bindings (+ level 1)))))
 			  `(lambda-simple ,(cadr exp) ,(replace-map (caddr exp) bindings (+ level 1))))

 			((and (eq? (car exp) 'lambda-var)  (ormap (lambda(x) (member x (vars-levels bindings (+ level 1)))) (cadr exp))) 
 			  `(lambda-simple ,(cadr exp) (seq (,@(generate-sets (vars-levels bindings (+ level 1))) ,@(replace-map (car (cdaddr exp)) bindings (+ 1 level))))))
 			((and (eq? (car exp) 'lambda-var) (not (member (cadr exp) (vars-levels bindings (+ level 1)))))
 			  `(lambda-simple ,(cadr exp) ,(replace-map (caddr exp) bindings (+ level 1))))

 			((eq? (car exp) 'pvar) (if (member (cadr exp) (vars-levels bindings level)) `(box-get (var ,(cadr exp))) 
 																						`(var ,(cadr exp))))
 			
 			((eq? (car exp) 'bvar) (if (member (cadr exp) (get-all-vars-levels bindings level)) `(box-get (var ,(cadr exp))) 
 																								`(var ,(cadr exp))))

 			((eq? (car exp) 'set)  (if (member (cadadr exp) (get-all-vars-levels bindings level)) `(box-set (var ,(cadadr exp)) ,@(replace-map (cddr exp) bindings level))
 																								  `(set (var ,(cadadr exp)) ,@(replace-map (cddr exp) bindings level))))	
 			(else `(,(replace-map (car exp) bindings level) ,@(replace-map (cdr exp) bindings level)))

		)))


(define calc-index-bvar ;param list comes reversed for convinience
	(lambda(var params index)
		  (if (member (cadr var) (car params)) `(,index ,(calc-index-pvar (cadr var) (car params))) (calc-index-bvar var (cdr params) (+ 1 index)))
))

(define calc-index-pvar  ;find the index
	(lambda(var params)
		   (- (length params) (length (member var params)))
		))


(define or-exp?
  (lambda(exp)
    (and (pair? exp)  (equal? (car exp) 'or))))



(define without-last
  (lambda(ll)
    (if (null? (cdr ll)) (list)
        (cons (car ll) (without-last (cdr ll))))))

(define get-last
  (lambda(ll)
    (if (null? (cdr ll)) (car ll)
         (get-last (cdr ll)))))

(define if-exp?
  (lambda(exp)
    (and (pair? exp)  (equal? (car exp) 'if3))))
    
(define seq?
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'seq))))
    

(define lambda-simple-exp?
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'lambda-simple))))    

(define lambda-opt-exp?
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'lambda-opt))))


(define def-exp?
  (lambda(exp)
    (and (pair? exp)  (equal? (car exp) 'define))))


(define applic-exp?
  (lambda(exp)
    (and (pair? exp)  (equal? (car exp) 'applic))))
    
(define set-exp?
  (lambda(exp)
    (and (pair? exp)  (equal? (car exp) 'set))))
    
(define box-set-exp?
  (lambda(exp)
    (and (pair? exp)  (equal? (car exp) 'box-set))))
    
(define box-get-exp?
  (lambda(exp)
    (and (pair? exp)  (equal? (car exp) 'box-get))))
    
(define box-exp?
  (lambda(exp)
    (and (pair? exp)  (equal? (car exp) 'box))))




(define annotate-tc
  (lambda(expr)
    (letrec ((annotate
              (lambda(exp tc?)
             
                (cond 
                        ((and (list? exp) (or (eq? (car exp) 'var) (eq? (car exp) 'const) (eq? (car exp) 'pvar) (eq? (car exp) 'fvar) (eq? (car exp) 'bvar)))  
                        exp)
                
                       ((or-exp? exp) 
                       (let ((body (cadr exp)))
                         `(or ,(append (map (lambda(x) (annotate x #f)) (without-last body)) `(,(annotate (get-last body) tc?))))))
                         
                      ((if-exp? exp) `(if3 ,(annotate (cadr exp) #f) ,(annotate (caddr exp) tc?) ,(annotate (cadddr exp) tc?)))
                      
                      ((seq? exp) 
                       (let ((body (cadr exp)))
                         `(seq ,(append (map (lambda(x) (annotate x #f)) (without-last body)) `(,(annotate (get-last body) tc?))))))
                         
                      ((lambda-simple-exp? exp)
                       (let ((body (caddr exp))
                             (vars (cadr exp)))
                         `(lambda-simple ,vars ,(annotate body #t))))
                         
                      ((lambda-opt-exp? exp)
                       (let ((body (cadddr exp))
                             (vars (cadr exp))
                             (extra-vars (caddr exp)))
                         `(lambda-opt ,vars ,extra-vars ,(annotate body #t))))
                 
                      ((def-exp? exp)
                       (let* ((vars  (cadr exp))
                              (body (caddr exp)))           
                         `(define ,vars ,(annotate body #f))))
                         
                      ((set-exp? exp)
                      `(set,(cadr exp) ,(annotate (caddr exp) #f)))
                      
                      ((box-set-exp? exp)
                      `(box-set ,(cadr exp) ,(annotate (caddr exp) #f)))
                      
                      ((box-get-exp? exp)
                      `(box-get ,(annotate (cadr exp) #f)))
                      
                      ((box-exp? exp)
                      `(box ,(annotate (cadr exp) #f)))
                      
                         
                      ((applic-exp? exp)
                       (let* ((proc  (cadr exp))
                              (args (caddr exp)))
                         (if tc?
                         `(tc-applic ,(annotate proc #f) ,(map (lambda(x) (annotate x #f)) args))
                         `(applic ,(annotate proc #f) ,(map (lambda(x) (annotate x #f)) args)))))
                         
                         
                      (else exp)))))
      (annotate expr #f))))
                      

(define box-set ;exp is parsed
  (lambda(exp)
    (replace-map (replace-map0 exp '() '() ) 
           (box-constraints-vars (replace-map0  exp '() '())) -1)
    ))


(define add-params 
	(lambda (simulated_scope params)
		`(,@simulated_scope ,params)
		))

(define in-params?
	(lambda(var params)
		(not (eq? (member var params) #f))))


(define in-scope?
	(lambda(var simulated_scope)
		(if (null? simulated_scope) #f
			(if (member var (car simulated_scope)) #t
					(in-scope? var (cdr simulated_scope)) 
))))

(define calc-major-minor
	(lambda(var simulated_scope level)
		(if (null? simulated_scope) #f
			(if (member var (car simulated_scope)) `(,level ,(calc-index-pvar var (car simulated_scope)))
					(calc-major-minor var (cdr simulated_scope) (+ 1 level)) 
))))

(define analyse 
	(lambda(ast simulated_scope params) ;1
		(cond
			((or (not (list? ast)) (null? ast)) ast)

			((eq? (car ast) 'lambda-simple) ;2 - lambda-simple case
				(begin 
					(set! simulated_scope (add-params simulated_scope params)) ;3
					(set! params (cadr ast)) ;4
					`(lambda-simple ,@(analyse (cdr ast) simulated_scope params)) ;12-13
				)
			)

			((eq? (car ast) 'lambda-opt) ;2 - lambda-opt case
				(begin 
					(set! simulated_scope (add-params simulated_scope params)) ;3
					(set! params `(,@(cadr ast) ,(caddr ast))) ;4
					`(lambda-opt ,(cadr ast) ,(caddr ast) ,@(analyse (cdddr ast) simulated_scope params)) ;12-13
				)
			)	

			((eq? (car ast) 'var) ;5
				(if (in-params? (cadr ast) params) ;6
				 `(pvar ,(cadr ast) ,(calc-index-pvar (cadr ast) params)) ;7
				 		(if (in-scope? (cadr ast)  simulated_scope) ;8  
				 			(append `(bvar ,(cadr ast)) (calc-major-minor (cadr ast) (reverse-list simulated_scope) 0)) ;9 - sending reversed scope lists for ease of tagging, starting level always 0
				 				`(fvar ,(cadr ast))))) ;10-11

			(else
				`( ,(analyse (car ast) simulated_scope params) ,@(analyse (cdr ast) simulated_scope params)))
		)
	)	
)

(define pe->lex-pe  ;exp is parsed
  (lambda(exp)
    (analyse exp '() '() )))