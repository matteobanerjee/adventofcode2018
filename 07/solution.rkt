#lang racket

(struct node (label children) #:transparent)
(define (sort-nodes n) (sort n string<? #:key node-label))
(define (node-eq? n1 n2) (equal? (node-label n1) (node-label n2)))

(define input (file->lines "./input"))

;; (: line->edge-list (-> (Listof String) (Listof (List String String))))
(define (lines->edge-list l)
  (define line-regexp #rx"Step ([A-Z]) must be finished before step ([A-Z])")
  (map (λ (x) (rest (regexp-match line-regexp x))) l))

(define (tree-from-edge-list l)
  (define root-label "") ;; "" is our entrypoint because it formats nicely
  (define parent-label-map (make-hash))
  (define edge-map (make-hash))
  (for ([edge (in-list l)])
    (hash-update! edge-map (first edge) (λ (x) (cons (second edge) x)) '())
    (hash-update! parent-label-map (second edge) (λ (x) (cons (first edge) x)) '()))

  (define roots
    (remove*
     (remove-duplicates (flatten (hash-values edge-map)))
     (hash-keys edge-map)))

  (hash-set! edge-map root-label roots)

  (define nodes (make-hash))

  (define (create-node label)
    (define (get-or-create-node child-nodes)
      (hash-ref! nodes label (λ () (node label child-nodes))))
    (let ([child-labels (hash-ref edge-map label (λ () '()))])
      (if (empty? child-labels)
          (get-or-create-node '())
          (get-or-create-node (map create-node child-labels)))))
  (create-node root-label)
  (define parent-map (for/hash ([(k v) (in-hash parent-label-map)])
                       (values k (map (curry hash-ref nodes) v))))
  (values (hash-ref nodes root-label) parent-map))


(define (traverse root parent-map)
    (define (visitable? visited-labels x)
      (define parent-labels (map node-label (hash-ref parent-map (node-label x) '())))
      (andmap (λ (l) (member l visited-labels)) parent-labels))
  (let loop ([visited-labels '()] [stack (list root)])
    (if (empty? stack)
        (reverse visited-labels) ;; return in visit order
        (let* ([n (first stack)]
               [new-visited (cons (node-label n) visited-labels)]
               [visitable-children (filter (curry visitable? new-visited)
                                           (node-children n))])
          (loop
           new-visited
           (sort-nodes (append visitable-children (rest stack))))))))

(printf "Part 1 Solution: ~a\n" (string-join (call-with-values (λ () (tree-from-edge-list (lines->edge-list input))) traverse) ""))

;; Part 2
(struct worker ( done-time))
;; TODO
;; (define (part-2-solution root num-workers)
;;   (let loop ([tick -1]
;;              [workers (build-list num-workers (λ (x) #f))]
;;              [])))
