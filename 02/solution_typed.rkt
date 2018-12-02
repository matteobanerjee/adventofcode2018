#lang typed/racket

(define input (file->lines "./input"))

;; ;; Part 1
(: char-counts (-> String (Immutable-HashTable Char Integer)))
(define (char-counts str)
  (for/fold ([counts : (Immutable-HashTable Char Integer) (hash)])
            ([char (in-string str)])
    ((inst hash-update Char Integer) counts char (curry + 1) (λ () 0))))

(: has-repeated-char (-> Integer String Boolean))
(define (has-repeated-char times str)
  ((inst ormap Integer Boolean) (curry = times) (hash-values (char-counts str))))

(: checksum (-> (Listof String) Integer))
(define (checksum list-of-strings)
  (* (count (curry has-repeated-char 2) list-of-strings)
     (count (curry has-repeated-char 3) list-of-strings)))

(printf "Part 1 Solution: ~a\n" (checksum input))

;; ;; Part 2

;; ;; naive, compare every string to every other string

(: differ-by-1? (-> String String Boolean))
(define (differ-by-1? s1 s2)
  ;; can't get #: result to work in typed racket so compute result by wrapping
  ;; the for comprehension
  (= 1
     (for/fold ([diff : Integer 0])
               ([c1 : Char (in-string s1)]
                [c2 : Char (in-string s2)])
       #:break (> diff 1)
       (if (eq? c1 c2) diff (+ diff 1)))))


(: find-match (-> String (Listof String) (U String False)))
(define (find-match str str-list)
  (findf (curry differ-by-1? str) str-list))

(: search (-> (Listof String) (U False (List String String))))
(define (search l)
  (let* ([_first (first l)]
         [_rest (rest l)]
         [maybe-match (find-match _first _rest)])
    (cond
      [maybe-match (list _first maybe-match)]
      [(empty? _rest) #f]
      [else (search _rest)])))

(: common-letters (-> (List String String) String))
(define (common-letters string-pair)
  (list->string
   (reverse
    (for/fold ([accum : (Listof Char) '()])
              ([i (in-string (first string-pair))]
               [j (in-string (second string-pair))])
      (if (eq? i j) (cons i accum) accum)))))


(printf "Part 2 Solution: ~a\n"
        (let ([result (search input)])
          (if result (common-letters result) "No match found")))

