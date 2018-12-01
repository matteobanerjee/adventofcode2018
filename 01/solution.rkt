#lang racket

(define input (map string->number (file->lines "./input")))

;; Part 1
(define part-1-solution
  (foldr + 0 input))

(printf "Part 1 Solution: ~a\n" part-1-solution)

;; Part 2
(define repeating-input
  (for/stream ([i (in-naturals)])
              (list-ref input (modulo i (length input)))))

(define part-2-solution
  (for/fold ([accum 0]
             [freq-set (set)]
             #:result accum)
            ([i (in-stream repeating-input)])
    #:break (set-member? freq-set accum)
    (values (+ accum i) (set-add freq-set accum))))

(printf "Part 2 Solution: ~a\n" part-2-solution)
