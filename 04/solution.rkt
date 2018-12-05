#lang racket

(define input (file->lines "./input"))

(define (statement-type log-line)
  (cond
    [(string-suffix? log-line "falls asleep") 'asleep]
    [(string-suffix? log-line "wakes up") 'awake]
    [(string-suffix? log-line "begins shift") 'guard]
    [else 'unknown]))

(define minute-regexp #rx"^\\[1518-..-.. ..:([0-9]+)\\]")
(define (get-minute s) (string->number (second (regexp-match minute-regexp s))))

(define guard-regexp #rx"^\\[.*\\] Guard #([0-9]+) .*")
(define (get-guard s) (second (regexp-match guard-regexp s)))

(define (process-log in)
  (define sorted-log (sort in string<?))
  (for/fold ([current-guard #f]
             [sleep-stats-hash (hash)]
             [range-start #f]
             #:result sleep-stats-hash)
            ([log-line (in-list sorted-log)])
    (let* ([type (statement-type log-line)]
           [minute (get-minute log-line)])
      (cond
        [(eq? type 'guard) (values (get-guard log-line) sleep-stats-hash #f)]
        [(eq? type 'asleep) (values current-guard sleep-stats-hash minute)]
        [else (values current-guard
                      (hash-update sleep-stats-hash
                                   current-guard
                                   (curry cons (range range-start minute))
                                   '())
                      #f)])
      )))


(define sleep-stats-hash (process-log input))

(define (occurrence-counts lst)
  (for/fold ([h (hash)])
            ([x (in-list lst)])
    (hash-update h x add1 0)))

(define (most-frequent-key occurrence-hash)
  (for/fold ([max-key #f]
             [max-val -1]
             #:result max-key)
            ([(k v) (in-hash occurrence-hash)])
    (if (> v max-val) (values k v) (values max-key max-val))))

;; Part 1
(define (sum-sleep sleep-ranges) (foldl + 0 (map length sleep-ranges)))
(define sleepiest-guard
  (first (argmax (compose sum-sleep cdr) (hash->list sleep-stats-hash))))

(define sleepiest-minute
  (most-frequent-key
   (occurrence-counts
    (flatten (hash-ref sleep-stats-hash sleepiest-guard)))))

(printf "Part 1 Solution: guard: ~a minute: ~a\n" sleepiest-guard sleepiest-minute)

;; count frequencies for (guard, minute) tuples and take highest
(define part-2-solution
  (let ([guard-minutes (for*/list
                           ([(k v) (in-hash sleep-stats-hash)]
                            [segment (in-list v)]
                            [minute (in-list segment)])
                         (list k minute))])
    (most-frequent-key (occurrence-counts guard-minutes))))

(printf "Part 2 Solution: guard: ~a minute: ~a\n"
        (first part-2-solution)
        (second part-2-solution))
