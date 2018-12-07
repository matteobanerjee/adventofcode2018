#lang racket

(define input (file->string "./input"))

(define (opposite-polarity? a b)
  (and (not (char=? a b)) (char-ci=? a b)))

(define (reduce-once char-list)
  (define (iter lst res)
    (cond
      [(empty? lst) res]
      [(empty? (rest lst)) (cons (first lst) res)]
      [else
       (if (opposite-polarity? (first lst) (second lst))
           (iter (drop lst 2) res) ;; skip next two chars
           (iter (rest lst) (cons (first lst) res)))]))
  (reverse (iter char-list '())))

(define (reduce str)
  (define (iter char-list)
    (let ([reduction (reduce-once char-list)])
      (if (equal? reduction char-list) reduction (iter reduction))))
  (list->string (iter (string->list str))))

(printf "Part 1 Solution: ~a\n" (time (string-length (reduce input))))

(define alphabet
  (filter char-alphabetic? (map integer->char (range 97 126))))

(define (remove-char-pair c-lowcase str)
  (let ([rx (regexp (string c-lowcase  #\| (char-upcase c-lowcase)))])
    (regexp-replace* rx str "")))

(define (brute-force-find-best-improvement str)
  (argmin identity
   (map
    (λ (letter)
      (string-length (reduce (remove-char-pair letter str))))
    alphabet)))

(printf "Part 2 Solution: ~a\n" (time (brute-force-find-best-improvement input)))


