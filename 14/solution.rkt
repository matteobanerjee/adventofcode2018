#lang racket

(define initial-state #(3 7))

(define (next-state state p1 p2)
  (let* ([r1 (vector-ref state p1)]
         [r2 (vector-ref state p2)]
         [sum (+ r1 r2)]
         [first-digit (floor (/ sum 10))]
         [second-digit (remainder sum 10)])
    (if (= 0 first-digit)
        (vector-append state (vector second-digit))
        (vector-append state (vector first-digit second-digit)))))

(define (next-recipe-index state current-index)
  (let ([value (vector-ref state current-index)])
    (remainder (+ current-index value 1) (vector-length state))))

(define (compute-state final-state-size)
  (let loop ([state initial-state]
             [p1 0]
             [p2 1])
    (if (> (vector-length state) final-state-size)
        state
        (let ([next-state (next-state state p1 p2)])
          (loop next-state
           (next-recipe-index next-state p1)
           (next-recipe-index next-state p2))))))

(define (part-1-solution num-recipes)
  (string-join
   (map number->string
        (take
         (drop
          (vector->list (compute-state (+ 10 num-recipes)))
          num-recipes)
         10))
   ""))

(printf "Part 1 solution: ~a\n" (part-1-solution 236021))
