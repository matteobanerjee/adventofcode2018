#lang racket

(define input (file->lines "./input"))

(define (lines->edge-list l)
  (define line-regexp #rx"Step ([A-Z]) must be finished before step ([A-Z])")
  (map (λ (x) (rest (regexp-match line-regexp x))) l))

(define (sort-str-l x) (sort x string<?))
(define (part-1-solution in)
  (define input-edge-list (lines->edge-list input))
  (define child->parent-map (make-hash))
  (define parent->child-map (make-hash))

  (for ([edge (in-list input-edge-list)])
    (hash-update! parent->child-map (first edge) (λ (x) (cons (second edge) x)) '())
    (hash-update! child->parent-map (second edge) (λ (x) (cons (first edge) x)) '()))

  ;; nodes that don't have parents
  (define roots (remove* (hash-keys child->parent-map) (hash-keys parent->child-map)))
  (define (visitable? visited x)
    (let ([parents (hash-ref child->parent-map x)])
      (andmap (λ (p) (member p visited)) parents)))
  (let loop ([visited '()]
             [to-visit (sort-str-l roots)])
    (if (empty? to-visit)
        (reverse visited)
        (let* ([next (car to-visit)]
               [_rest (cdr to-visit)]
               [new-visited (cons next visited)]
               [children (hash-ref parent->child-map next '())]
               [visitable-children (filter (curry visitable? new-visited) children)])
          (loop new-visited (sort-str-l (append visitable-children _rest)))))))

(printf "Part 1 Solution: ~a\n" (string-join (part-1-solution input) ""))

;; Part 2
;; (struct worker ( done-time))
;; (define (part-2-solution root num-workers)
;;   (let loop ([tick -1]
;;              [workers (build-list num-workers (λ (x) #f))]
;;              [])))
