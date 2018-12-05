#lang racket
(require racket/trace)

(define input (file->string "./input"))
(define (opposite-polarity? a b)
  (and (not (char=? a b)) (char-ci=? a b)))

(define (reduce-once char-list)
  (define (iter lst res)
    (let* ([_first (first lst)]
           [_rest (rest lst)]
           [_second (if (empty? _rest) #f (first _rest))])
      (cond
        [(not _second) (cons _first res)]
        [(opposite-polarity? _first _second) (iter (rest _rest) res)] ;; drop two
        [else (iter _rest (cons _first res))])))
  (reverse (iter char-list '())))

(define (reduce str)
  (define (iter char-list)
    (let ([reduction (reduce-once char-list)])
      (if (equal? reduction char-list)
          (list->string reduction)
          (iter reduction))))
  (iter (string->list str)))

(printf "Part 1 Solution: ~a\n" (string-length (reduce input)))
(define alphabet
  (set (filter char-alphabetic? (map (compose char-downcase integer->char) (range 126)))))

(define (brute-force-find-best-improvement)
  (for/fold ([best #f]
             [best-len +inf.0]
             #:result (cons  best best-len))
            ([letter (in-set alphabet)])
    ;; TODO
    ))
(printf "Part 2 Solution: ~a\n" (string-length (reduce input)))



