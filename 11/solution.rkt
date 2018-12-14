#lang racket
(define max-x 300)
(define max-y 300)
(struct fuel-cell (x y) #:transparent)

(define (power-level x y serial)
  (let* ([rack-id (+ 10 x)]
         [power (* rack-id (+ serial (* rack-id y)))]
         [hundreds-digit (remainder (floor (/ power 100)) 10)])
    (- hundreds-digit 5)))

(define (sum-nxn-area top-left-fc serial n)
  (let ([init-x (fuel-cell-x top-left-fc)]
        [init-y (fuel-cell-y top-left-fc)])
    (for*/sum ([x (in-range init-x (+ n init-x))]
               [y (in-range init-y (+ n init-y))])
      (power-level x y serial))))

(define grid
  (for*/list ([x (in-range 1 (- max-x 2))]
              [y (in-range 1 (- max-y 2))])
    (fuel-cell x y)))

(define (part-1-solution input)
  (argmax car (map (λ (x) (cons (sum-nxn-area x input 3) x)) grid)))

(printf "Part 1 solution: ~a\n" (cdr  (part-1-solution 7689)))

(define (sum-power min-x x min-y y serial)
  (+ (for/sum ([a (in-range min-x x)]) (power-level a y serial))
     (for/sum ([a (in-range min-y (add1 y))]) (power-level x a serial))))

(define (max-all-areas-rec top-left-fc serial)
  (define min-x (fuel-cell-x top-left-fc))
  (define min-y (fuel-cell-y top-left-fc))
  (define (sum-new x y) (sum-power min-x x min-y y serial))
  (let loop ([prev-sum 0]
             [max-sum -inf.0]
             [iter 0]
             [max-iter 0])
    (let ([x (+ iter min-x)]
          [y (+ iter min-y)])
      (if (or (> x max-x) (> y max-y))
          (list max-sum top-left-fc (add1 max-iter))
          (let* ([new-sum (+ prev-sum (sum-new x y))]
                 [new-max? (> new-sum max-sum)])
            (loop new-sum (if new-max? new-sum max-sum) (add1 iter)
                  (if new-max? iter max-iter)))))))

(define (part-2-solution input)
  (argmax car (map (λ (x) (max-all-areas-rec x input)) grid)))

(printf "Part 2 solution: ~a\n" (cdr  (part-2-solution 7689)))



