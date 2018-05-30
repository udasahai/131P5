;(define (expr-let-compare x y arr)
	
;	(cond
		



;	))

(define test-expr-x '((lambda (a b) (f a b)) 1 2) )
(define test-expr-y '((lambda (c d) (f d c)) 3 4) )

(define (slist->string slst)
  (string-join (map symbol->string slst) "!"))

(define (expr-compare x y)
		( expr-let-compare x y (list) )
	)

(define (stringer x-term y-term)
		(if (equal? (car x-term) (car y-term)) (slist->string (list (car x-term) (car x-term)) ) (slist->string (list (car x-term) (car y-term)) ) )
)

(define (stringer-lambda x-term y-term)
		(if (equal? x-term y-term) (slist->string (list x-term x-term)) (slist->string (list x-term y-term)) )
)


(define (create-list x_list y_list)
		(if (equal? x_list (list)) (list) 
			(cons (stringer (car x_list) (car y_list)) (create-list (cdr x_list) (cdr y_list) ) ))		
	)

(define (search-array-y x arr)
		(if (equal? arr (list)) x
			(if (equal? (substring (car arr) 2 3) (symbol->string x)) (if (equal? (substring (car arr) 0 1) (substring (car arr) 2 3)) x (string->symbol (car arr)) ) (search-array-y x (cdr arr))) )	
	)

(define (real-variable-y x arr)
		
		(if (not (symbol? x)) x (search-array-y x arr) ))


(define (search-array-x x arr)
		(if (equal? arr (list)) x
			(if (equal? (substring (car arr) 0 1) (symbol->string x)) (if (equal? (substring (car arr) 0 1) (substring (car arr) 2 3)) x (string->symbol (car arr)) ) (search-array-x x (cdr arr))) )	
	)

(define (real-variable-x x arr)
		(if (not (symbol? x)) x (search-array-x x arr) ))



(define (lambda-list x_list y_list)
		(if (equal? x_list (list)) (list) 
			(cons (stringer-lambda (car x_list) (car y_list)) (lambda-list (cdr x_list) (cdr y_list)) ) )		
	)

(define (lambda-remain x)
	(cons (cons 'Lambda (cdr (car x))) (cdr x))
  )


(define test-expr-compare (lambda (x y)
	(if (and (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y))))
	       (equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y))))) #t #f )))



(define (expr-let-compare x y arr)
	(cond 
		[(equal? (real-variable-x x arr) (real-variable-y y arr)) (if (equal? (real-variable-x x arr) 'Lambda) 'lambda (real-variable-x x arr) )] ;if same then return 
		[(and (boolean? x) (boolean? y))
      			(if x '% '(not %))]
      	[ (and (list? x) (list? y) (list? (car x)) (list? (car y)) (equal? (car (car x)) 'lambda) (equal? (car (car y)) 'lambda)) (expr-let-compare (lambda-remain x) (lambda-remain y) (lambda-list (car (cdr (car x))) (car (cdr (car y))) )) ]
      	[ (and (list? x) (list? y) (equal? (car x) 'let) (equal? (car y) 'let)) (cons 'let (expr-let-compare (cdr x) (cdr y) (create-list (car (cdr x)) (car (cdr y)))))  ]
      	[(or (not (and (list? x) (list? y))) 
      		(not(= (length x) (length y) )) 
      		(or (equal? (car x) 'quote) (equal? (car y) 'quote)) 
      		(or (and (equal? (car x) 'if) (not (equal? (car y) 'if))) (and (equal? (car y) 'if) (not (equal? (car x) 'if))) ) )
      		(list 'if '% (real-variable-x x arr) (real-variable-y y arr))]

		[else 

		(cons (expr-let-compare (car x) (car y) arr ) (expr-let-compare (cdr x) (cdr y) arr ) )

		]
		) )






