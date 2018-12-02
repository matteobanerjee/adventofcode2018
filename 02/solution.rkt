#lang racket

(define input (file->lines "./input"))

;; Part 1
(define (char-counts str)
  (for/fold ([counts (hash)])
            ([char (in-string str)])
    (hash-update counts char (curry + 1) 0)))

(define (has-repeated-char times str)
  (ormap (curry = times) (hash-values (char-counts str))))

(define (checksum list-of-strings)
  (* (count (curry has-repeated-char 2) list-of-strings)
     (count (curry has-repeated-char 3) list-of-strings)))

(printf "Part 1 Solution: ~a\n" (checksum input))



;; Part 2

;; naive, compare every string to every other string

(define (differ-by-1? s1 s2)
  (for/fold ([diff 0]
             #:result (= diff 1))
            ([c1 (in-string s1)]
             [c2 (in-string s2)])
    #:break (> diff 1)
    (if (eq? c1 c2) diff (+ diff 1))))


(define (search l)
  (define (find-match str str-list)
    (findf (curry differ-by-1? str) str-list))
  (let* ([_first (first l)]
         [_rest (rest l)]
         [maybe-match (find-match _first _rest)])
    (if maybe-match
        (list _first maybe-match)
        (search _rest))))

(define (common-letters string-pair)
  (for/fold ([accum '()]
             #:result (list->string (reverse accum)))
            ([i (in-string (first string-pair))]
             [j (in-string (second string-pair))])
    (if (eq? i j) (cons i accum) accum)))


(printf "Part 2 Solution: ~a\n" (common-letters (search input)))
