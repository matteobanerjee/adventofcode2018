#lang racket
;; devide and conquer algorithm operating on racket strings rather than linked lists
;; this is two orders of magnitude faster than the previous solution. The idea was
;; to use memoization to avoid re-computing merges but the merge procedure is so
;; fast now that memoization actually slowed down the algorithm by a lot.
(define input (file->string "./input"))

(define (opposite-polarity? a b)
  (and (not (char=? a b)) (char-ci=? a b)))

(define (merge str-1 str-2)
  ;; iterate backward through str-1 and forward to str-2 as soon as the characters
  ;; don't match up, return the concatenation of the remaining substrings
  ;; Iter 1:
  ;; a b c | C d e
  ;;     ^   ^
  ;; Iter 2:
  ;; a b c | C d e
  ;;   ^       ^
  ;; Return:
  ;; a b d e
  (define str-1-len (string-length str-1))
  (define str-2-len (string-length str-2))
  (let loop ([i (- str-1-len 1)]
             [j 0])
    (cond
      [(< i 0) (substring str-2 j)]
      [(>= j str-2-len) (substring str-1 0 (+ 1 i))]
      [(opposite-polarity? (string-ref str-1 i) (string-ref str-2 j))
       (loop (- i 1) (+ j 1))]
      [else (string-append (substring str-1 0 (+ 1 i)) (substring str-2 j))])))

(define (reduce str) ;; pass in memo table
  ;; memoize table
  (let loop ([s str])
    ;; lookup s in memo table first, restrict the size of strings in the memotable
    ;; since only small-ish strings are likely to be recomputed
    (let* ([len (string-length s)]
           [middle (floor (/ len 2))])
      (cond
        ;; This check is technically not required, it would be enough to subdivide
        ;; until all strings are either of length 1 or 2, but it's a nice optimization
        [(and (= len 2) (opposite-polarity? (string-ref s 0) (string-ref s 1)) "")]
        [(<= len 2) s]
        [else (merge (loop (substring s 0 middle))
                     (loop (substring s middle)))]))))

(time (string-length (reduce input)))


(define alphabet
  (filter char-alphabetic? (map integer->char (range 97 126))))

(define (remove-char-pair c-lowcase str)
  (let ([rx (regexp (string c-lowcase  #\| (char-upcase c-lowcase)))])
    (regexp-replace* rx str "")))

(define (brute-force-find-best-improvement str)
  ;; create mutable hash for memo
  (argmin identity
   (map
    (λ (letter)
      (string-length (reduce (remove-char-pair letter str))))
    alphabet)))

(printf "Part 2 Solution: ~a\n" (time (brute-force-find-best-improvement input)))
