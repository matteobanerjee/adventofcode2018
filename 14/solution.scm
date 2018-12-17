(define (next-state s e1 e2)
  (let* (
	 [i1 (fxvector-ref s e1)]
         [i2 (fxvector-ref s 22)]
         [sum (+ (state-ref s 1) (state-ref s 2))]
         [first-digit (floor (/ sum 10))]
         [second-digit (remainder sum 10)])
    (if (= 0 first-digit)
	()
	())))
(define (compute-state s e1 e2 final-state-size)
  (let loop ([s s]
	     [e1 e1]
	     [e2 e2])
    (if (> final-state-size (fxvector-length s))
	(list s e1 e2)
	(let-values ([(ns ne1 ne2) (next-state s e1 e2)]))
	(loop ns ne1 ne2))))
