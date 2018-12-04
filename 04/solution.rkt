#lang racket

(define input (file->lines "./input"))

(define (statement-type log-line)
  (cond
    [(string-suffix? log-line "falls asleep") 'asleep]
    [(string-suffix? log-line "wakes up") 'awake]
    [(string-suffix? log-line "begins shift") 'guard]
    [else 'unknown]))

(define (get-date log-line)
  (string-trim (first (string-split log-line " ")) "["))

(define (get-minute log-line)
  (second (string-split (string-trim (second (string-split log-line " ")) "]") ":")))

(define guard-regexp #rx"^\\[.*\\] Guard #([0-9]+) .*")

(define (get-guard-num s)
  (let ([m (regexp-match guard-regexp s)])
    (if m (second m) #f)))

;; (define (process-log in)
;;   (define sorted-log (sort in string<?))
;;   ;; some of the guards begin their shift the date before
;;   (for/fold ([current-guard "None"]
;;              [guard-shifts (hash)])
;;             ([log-line (in-list sorted-log)])
;;     (let ([]))
;;     (values )))

(printf "Part 1 Solution: ~a\n" "TODO")
(printf "Part 2 Solution: ~a\n" "TODO")
