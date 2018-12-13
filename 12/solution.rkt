#lang racket

(define input (file->lines "./input"))

(struct pot-range (first-idx vec) #:transparent)


(define triple-false #(#f #f #f))

;; compact, then pad by three on both sides
(define (make-padded-pot-range first-idx vec)
  (define-values (ft lt)
    (for/fold ([first-true -1]
               [last-true -1])
              ([i (in-range (vector-length vec))])
      (let ([val (vector-ref vec i)])
        (values
         (if (and (= -1 first-true) val) i first-true)
         (if val i last-true)))))
  ;; todo: handle totally false
  (define new-first (- (+ first-idx ft) 3))
  (define compacted-vec (vector-copy vec ft (add1 lt)))
  (pot-range new-first (vector-append triple-false compacted-vec triple-false)))

(define (pot-range-sum pr)
  (define start-num (pot-range-first-idx pr))
  (define vec (pot-range-vec pr))
  (for/sum ([pot (in-vector vec)]
            [plant-num (in-range start-num (+ (vector-length vec) start-num 1))])
    (if pot plant-num 0)))

;; (: spec-to-vec (-> String (Vector Boolean)))
(define (spec->vec spec) (for/vector ([c (in-string spec)]) (equal? c #\#)))

(define initial-state
  (let* ([spec (string-trim (car input) "initial state: ")]
         [initial-vector (spec->vec spec)])
    (make-padded-pot-range 0 initial-vector)))

(define transformations
  (for/hash ([str (in-list (drop input 2))])
    (let ([trans-vec (spec->vec (substring str 0 5))]
          [result (equal? "#" (substring str (sub1 (string-length str))))])
      (values trans-vec result))))

(define (pot-range-apply-transformations pr)
  (define vec (pot-range-vec pr))

  (define (slice idx)
    (let ([start (- idx 2)]) (vector-copy vec start (+ 5 start))))

  (define new-vec
    (for/vector ([i (in-range 2 (- (vector-length vec) 2))])
      (let ([current-slice (slice i)])
        (hash-ref transformations (slice i) #f))))
  (make-padded-pot-range (+ 2 (pot-range-first-idx pr)) new-vec))

(define part-1-solution
  (for/fold ([pr initial-state]
             #:result (pot-range-sum pr))
            ([i (in-range 20)])
    (pot-range-apply-transformations pr)))

(printf "Part 1 solution: ~a\n" part-1-solution)

(define (find-steady-state pr)
  (let loop ([i 1]
             [prev pr]
             [next (pot-range-apply-transformations pr)])
    (if (= i 300);; (equal? (pot-range-vec prev) (pot-range-vec
        (cons (pot-range-sum next) (- (pot-range-sum next) (pot-range-sum prev)))
        (loop (add1 i) next (pot-range-apply-transformations next)))))

(find-steady-state initial-state)
(+ (* (- 50000000000 300) 81) 25098)




