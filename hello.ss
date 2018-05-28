(define (expr x y)
	(cond 
		[(equal? x y) x] ;if same then return 
		[(and (boolean? x) (boolean? y))
      			(if x '% '(not %))] 
		[else x]
		) )