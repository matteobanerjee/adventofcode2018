#lang racket

(struct fuel-cell (x y) #:transparent)

(define (power-level fc serial)
  (let* ([rack-id (+ 10 (fuel-cell-x fc))]
         [power (* rack-id (+ serial (* rack-id (fuel-cell-y fc))))]
         [hundreds-digit (remainder (floor (/ power 100)) 10)])
    (- hundreds-digit 5)))

(define (sum-3x3-area top-left-fc serial)
  (let ([init-x (fuel-cell-x top-left-fc)]
        [init-y (fuel-cell-y top-left-fc)])
    (for*/sum ([x (in-range init-x (+ 3 init-x))]
               [y (in-range init-y (+ 3 init-y))])
      (power-level (fuel-cell x y) serial))))

(define grid
  (for*/list ([x (in-range 1 299)]
              [y (in-range 1 299)])
    (fuel-cell x y)))

(define (part-1-solution input)
  (argmax car (map (λ (x) (cons (sum-3x3-area x input) x)) grid)))

(printf "Part 1 solution: ~a\n" (cdr  (part-1-solution 7689)))

;; (sum-3x3-area (fuel-cell 21 61) 42)

;; todo test module
;; (power-level (fuel-cell 3 5) 8)
;; (power-level (fuel-cell 122 79) 57)
;; (power-level (fuel-cell 217 196) 39)
;; (power-level (fuel-cell 101 153) 71)



