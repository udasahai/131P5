;(define (expr-let-compare x y arr)
	
;	(cond
		



;	))


(define (expr-compare x y)
		( expr-let-compare x y (list) )
	)

(define (handle-let x y)
		(car x)		
	)


(define (expr-let-compare x y arr)
	(cond 
		[(equal? x y) x] ;if same then return 
		[(and (boolean? x) (boolean? y))
      			(if x '% '(not %))]
      	[ (and (list? x) (list? y) (equal? (car x) 'let) (equal? (car y) 'let)) (cons (car x) (handle-let (cdr x) (cdr y)) ) ]
      	[(or (not (and (list? x) (list? y))) 
      		(not(= (length x) (length y) )) 
      		(or (equal? (car x) 'quote) (equal? (car y) 'quote)) 
      		(or (and (equal? (car x) 'if) (not (equal? (car y) 'if))) (and (equal? (car y) 'if) (not (equal? (car x) 'if))) ) )
      		(list 'if '% x y)]

		[else 

		(cons (expr-let-compare (car x) (car y) (list) ) (expr-let-compare (cdr x) (cdr y) (list) ) )

		]
		) )






