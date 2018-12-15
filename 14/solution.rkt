#lang racket
(require data/gvector)

(struct state (vec e1 e2) #:transparent)

(define (state-index s elf)
  (cond
    [(= elf 1) (state-e1 s)]
    [(= elf 2) (state-e2 s)]
    [else (error "invalid elf number!")]))

(define (state-ref s elf) (vector-ref (state-vec s) (state-index s elf)))

(define initial-state (state #(3 7) 0 1))

(define (next-recipe-index vec current-index)
  (let ([value (vector-ref vec current-index)])
    (remainder (+ current-index value 1) (vector-length vec))))

(define (next-state s)
  (let* ([i1 (state-index s 1)]
         [i2 (state-index s 2)]
         [sum (+ (state-ref s 1) (state-ref s 2))]
         [first-digit (floor (/ sum 10))]
         [second-digit (remainder sum 10)]
         [new-vec (if (= 0 first-digit)
                      (vector-append (state-vec s) (vector second-digit))
                      (vector-append (state-vec s) (vector first-digit second-digit)))])
    (state new-vec (next-recipe-index new-vec i1) (next-recipe-index new-vec i2))))

(define (compute-state final-state-size)
  (let loop ([s initial-state])
    (if (> (vector-length (state-vec s)) final-state-size)
        s
        (loop (next-state s)))))

(define (part-1-solution num-recipes)
  (string-join
   (map number->string
        (take
         (drop
          (vector->list (state-vec (compute-state (+ 10 num-recipes))))
          num-recipes)
         10))
   ""))

;; (printf "Part 1 solution: ~a\n" (part-1-solution 236021))


;; --- Part Two ---

;; As it turns out, you got the Elves' plan backwards. They actually want to know how many recipes
;; appear on the scoreboard to the left of the first recipes whose scores are the digits from your puzzle input.

;;     51589 first appears after 9 recipes.
;;     01245 first appears after 5 recipes.
;;     92510 first appears after 18 recipes.
;;     59414 first appears after 2018 recipes.

;; How many recipes appear on the scoreboard to the left of the score sequence in your puzzle input?

(define (part-2-solution match-vec)
  (define match-len (vector-length match-vec))
  (define (check-match vec)
    (define len (vector-length vec))
    (for/fold ([match? #t])
        ([i (in-vector vec (- len match-len) len)]
         [j (in-vector match-vec)])
      #:break (not match?)
      (= i j)))
  (let loop ([i 0]
             [s initial-state])
    (let ([vec (state-vec s)])
      (cond
        [(< (vector-length vec) match-len) (loop (add1 i) (next-state s))]
        [(check-match vec)
         (vector-length (vector-drop-right vec match-len))]
        [else (loop (add1 i) (next-state s))]))))

(printf "Test 2 solution: ~a\n" (part-2-solution #(5 9 4 1 4)))
(printf "Test 2 solution: ~a\n" (part-2-solution #(5 1 5 8 9)))

(printf "Part 2 solution: ~a\n" (part-2-solution #(2 3 6 0 2 1)))


