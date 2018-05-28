(define reciprocal
  (lambda (n)
    (if (= n 0)
        "oops!"
        (/ 1 n))))

(define factorial 
			(lambda (x) 
					(if (= x 1)
							1
							(* x (factorial (- x 1))))))


(define fac
           (lambda (n)
                   (if (= n 0)
                          1
                          (* n (fac (- n 1))))))



(define sum
           (lambda (l)
                   (if (null? l)
                          0
                          (+ (car l) (sum (cdr l))))))



(define list_sum
		(lambda (list)
			(if (null? list)
				0
				(+ (car list) (list_sum (cdr list))))))


(define list_product
		(lambda (list)
			(if (null? list)
				1
				(* (car list) (list_product (cdr list))))))


(define list_length
		(lambda (list)
			(if (null? list)
				0
				(+ 1 (list_length (cdr list))))))

(define (length list) ((if (null? list)
				0
				1)))

; (define (reply s)
; (if ( equal? “hello” (substrings 0 5) )
; “hi!”
; “huh?”))

(define (reply s)
	(if (null? s)
		0
		(+ 1 (reply (cdr s)))))



