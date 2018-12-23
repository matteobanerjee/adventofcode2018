#lang racket

(define (mk-op proc a-imm? b-imm?)
  (λ (instruction registers)
    (let* ((a (vector-ref instruction 1))
           (b (vector-ref instruction 2))
           (c (vector-ref instruction 3))
           (a-val (if a-imm? a (vector-ref registers a)))
           (b-val (if b-imm? b (vector-ref registers b)))
           (val (proc a-val b-val))
           (after (vector-copy registers)))
      (begin
        (vector-set! after c val)
        after))))

(define (gt a b) (if (> a b) 1 0))
(define (eq-op a b) (if (= a b) 1 0))
(define op-hash (hash
             'addr (mk-op + #f #f)
             'addi (mk-op + #f #t)
             'mulr (mk-op * #f #f)
             'muli (mk-op * #f #t)
             'banr (mk-op bitwise-and #f #f)
             'bani (mk-op bitwise-and #f #t)
             'borr (mk-op bitwise-ior #f #f)
             'bori (mk-op bitwise-ior #f #t)
             'setr (mk-op (λ (a b) a) #f #f)
             'seti (mk-op (λ (a b) a) #t #f)
             'gtir (mk-op gt #t #f)
             'gtri (mk-op gt #f #t)
             'gtrr (mk-op gt #f #f)
             'eqir (mk-op eq-op #t #f)
             'eqri (mk-op eq-op #f #t)
             'eqrr (mk-op eq-op #f #f)))

(define ops (hash->list op-hash))
;; apply all ops to before, ignores the opcode
(define-struct op-result (op-name result op-code) #:transparent)

(define (apply-ops instruction before)
  (map (λ (kv) (op-result (car kv) ((cdr kv) instruction before)
                          (vector-ref instruction 0))) ops))

(define register-regex
  (pregexp "([[:digit:]]), ([[:digit:]]), ([[:digit:]]), ([[:digit:]])"))

;; parse before and after lines
(define (read-register l)
  (list->vector (map string->number (cdr (regexp-match register-regex l)))))

;; parse instruction input line
(define (read-instruction l)
  (list->vector (map string->number (string-split l " "))))

;; split input into 3 line chunks
(define (chunk3 l)
  (let loop ((l l) (res '()))
    (if (null? l)
      (reverse res)
      (loop (drop l 3) (cons (take l 3) res)))))

;; read input for the first part
(define input (filter (λ (s) (> (string-length s) 0)) (file->lines "./input")))

;; record for the parsed input
(define-struct observed-instruction (before instruction after) #:transparent)

;; parse input file into our records
(define observed-instructions
  (map (λ (chunk)
         (let ([before (first chunk)]
               [instruction (second chunk)]
               [after (third chunk)])
           (observed-instruction
            (read-register before)
            (read-instruction instruction)
            (read-register after))))
       (chunk3 input)))

;; list of ops matching a single observed instruction
(define (matching-ops obs-inst)
  (define expected (observed-instruction-after obs-inst))
  (filter (λ (op-res) (equal? expected (op-result-result op-res)))
	  (apply-ops (observed-instruction-instruction obs-inst)
                     (observed-instruction-before obs-inst))))

(define (part-1-solution)
  (length
   (filter
    (λ (x) (>= (length x) 3))
    (map matching-ops observed-instructions))))

(printf "Part 1 solution: ~a\n" (part-1-solution))


(define (determine-ops matched-ops)
  (remove-duplicates
   (map (λ (x) (cons (op-result-op-name (car x)) (op-result-op-code (car x))))
        (filter (λ (x) (= 1 (length x))) matched-ops))))


(define (remove-known-op-codes matched-ops known-opcodes)
  (define (filter-one op-reses)
    (filter-not
     (λ (op-res) (assq (op-result-op-name op-res) known-opcodes))
     op-reses))
  (filter-not null? (map filter-one matched-ops)))

(define (infer-op-codes)
  (let loop ((unknown-ops (map matching-ops observed-instructions))
	     (known-ops '()))
    (if (= (length known-ops) 16)
        (map (λ (p) (cons (cdr p) (car p))) known-ops)  ;; op-code->name mapping
        (let ((new-known (append (determine-ops unknown-ops) known-ops)))
          (loop (remove-known-op-codes unknown-ops new-known) new-known)))))

(define op-code->name (make-hash (infer-op-codes)))

(define (apply-instruction state ins)
  (define op-code (vector-ref ins 0))
  ((hash-ref op-hash (hash-ref op-code->name op-code)) ins state))

(define (execute-program)
  (define instructions (map read-instruction (file->lines "./input2")))
  (for/fold ([s (vector 0 0 0 0)])
            ([ins (in-list instructions)])
    (apply-instruction s ins)))

(printf "Part 2 solution: ~a\n" (vector-ref (execute-program) 0))
