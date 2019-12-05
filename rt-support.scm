(define (+ . lst) 
	(if (null? lst) 0
		 (fold-left _+ 0 lst)))

(define length
	(lambda(lst)
		(if (null? lst) 0
				(+ 1 (length (cdr lst)))
		)
	)
)

(define (- . lst) 
	(if (null? lst) "error_minus_cant_recieve_0_args"
		(if (= (length lst) 1) (_- 0 (car lst))
		 		(fold-left _- (car lst) (cdr lst)))))

(define (* . lst) 
	(if (null? lst) 1
		 (fold-left _* 1 lst)))

(define (/ . lst) 
	(if (null? lst) "error_minus_cant_recieve_0_args"
		(if (= (length lst) 1) (_/ 1 (car lst))
		 		(fold-left _/ (car lst) (cdr lst)))))

(define (append . lst) 
	(if (null? lst) '()
		 (fold-left _append '() lst)))

(define map
	(lambda(map_func lst)
			(cons (map_func (car lst))  
				(if (null? (cdr lst)) '() 
					(map map_func (cdr lst))))))

(define fold-left
	(lambda(func acc lst)
		(if (null? lst) acc
				 (fold-left func (func acc (car lst)) (cdr lst)))))


(define fold-right
	(lambda(func acc lst)
		(if (null? lst) acc
				 (func (car lst) (fold-right func acc (cdr lst))))))

(define (list . lst)
	(fold-right cons '() lst))

(define >
	(lambda x (car (fold-left (lambda (acc cur)(cons (and (binary-> (cdr acc) cur) (car acc)) cur)) (cons #t (car x)) (cdr x)))))

(define <
	(lambda x (car (fold-left (lambda (acc cur)(cons (and (binary-< (cdr acc) cur) (car acc)) cur)) (cons #t (car x)) (cdr x)))))

(define =
	(lambda(x . xs)
		(fold-left(lambda(m n) (and m n)) #t (map (lambda(k) (binary-= x k)) xs ))))

(define binary-<
	(lambda (x y) (_negative? (_- x y))))

(define binary->
	(lambda (x y) (_negative? (_- y x)))) 

(define binary-=
	(lambda (x y) (zero? (_- y x))))

(define _append
	(lambda (x y) 
		(if (null? x) y
			(cons (car x) (_append (cdr x) y)))))