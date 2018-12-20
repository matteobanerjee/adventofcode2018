;;; -*- Gerbil -*-
(import :std/misc/ports
	:std/srfi/14 ;; charsets
	:std/srfi/13 ;; strings
	:std/format)

(define input (read-file-string "./input"))

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
  (let loop ((i (- str-1-len 1))
             (j 0))
    (cond
      ((< i 0) (substring str-2 j str-2-len))
      ((>= j str-2-len) (substring str-1 0 (+ 1 i)))
      ((opposite-polarity? (string-ref str-1 i) (string-ref str-2 j))
       (loop (- i 1) (+ j 1)))
      (else (string-append (substring str-1 0 (+ 1 i)) (substring str-2 j str-2-len))))))

(define (reduce str)
  (let loop ((s str))
    (let* ((len (string-length s))
           (middle (floor (/ len 2))))
      (cond
        ;; This check is technically not required, it would be enough to subdivide
        ;; until all strings are either of length 1 or 2, but it's a nice optimization
        ((and (= len 2) (opposite-polarity? (string-ref s 0) (string-ref s 1)) ""))
        ((<= len 2) s)
        (else (merge (loop (substring s 0 middle))
                     (loop (substring s middle len))))))))

(define alphabet
  (filter
   (lambda (x) (and (char-alphabetic? x) (char-lower-case? x)))
   (char-set->list char-set:ascii)))

(define (remove-char-pair c-lowcase str)
  (string-filter (lambda (x) (not (char-ci=? x c-lowcase))) str))

(define (brute-force-find-best-improvement str)
  (foldl min +inf.0
   (map
    (lambda (letter)
      (string-length (reduce (remove-char-pair letter str))))
    alphabet)))

(define (main . args)
  (begin
    (printf "Part 2 Solution: ~a\n" (brute-force-find-best-improvement input))
    (printf "Part 1 solution: ~a\n" (string-length (reduce input)))))

(export main)