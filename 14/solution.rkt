#lang racket
(require data/gvector)

(struct state (vec e1 e2) #:transparent #:mutable)

(define (state-index s elf)
  (cond
    [(= elf 1) (state-e1 s)]
    [(= elf 2) (state-e2 s)]
    [else (error "invalid elf number!")]))

(define (state-ref s elf) (gvector-ref (state-vec s) (state-index s elf)))


(define (next-recipe-index vec current-index)
  (let ([value (gvector-ref vec current-index)])
    (remainder (+ current-index value 1) (gvector-count vec))))

(define (next-state! s)
  (let* ([i1 (state-index s 1)]
         [i2 (state-index s 2)]
         [sum (+ (state-ref s 1) (state-ref s 2))]
         [first-digit (floor (/ sum 10))]
         [second-digit (remainder sum 10)]
         [vec (state-vec s)])
    (begin
      (if (= 0 first-digit)
             (gvector-add! vec second-digit)
             (gvector-add! vec first-digit second-digit))
      (set-state-e1! s (next-recipe-index vec i1))
      (set-state-e2! s (next-recipe-index vec i2))
      s)))

(define (compute-state s final-state-size)
  (let loop ()
    (next-state! s)
    (if (> (gvector-count (state-vec s)) final-state-size)
        s
        (loop))))

(define (part-1-solution s num-recipes)
  (string-join
   (map number->string
        (take
         (drop
          (gvector->list (state-vec (compute-state s (+ 10 num-recipes))))
          num-recipes)
         10))
   ""))

(printf "Part 1 solution: ~a\n" (part-1-solution (state (gvector 3 7) 0 1) 236021))

(define (part-2-solution s match-vec)
  (define match-len (vector-length match-vec))
  (define (check-match vec offset)
    (define len (gvector-count vec))
    (define start-idx (- len match-len offset))
    (if (< start-idx 0) #f
        (for/fold ([match? #t])
                  ([i (in-range start-idx len)]
                   [j (in-vector match-vec)])
          #:break (not match?)
          (= (gvector-ref vec i) j))))
  (let loop ()
    (next-state! s)
    (let ([vec (state-vec s)])
      (cond
        [(check-match vec 0) (- (gvector-count vec) match-len)]
        [(check-match vec 1) (- (gvector-count vec) match-len 1)]
        [else (loop)]))))

(printf "Part 2 solution: ~a\n" (part-2-solution (state (gvector 3 7) 0 1) #(2 3 6 0 2 1)))
