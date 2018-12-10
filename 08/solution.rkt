#lang racket

(struct node (children meta) #:transparent)

(define input (map string->number (string-split (file->string "./input"))))

(define (tree-from-input in)
  (let ([num-children (first in)]
        [num-meta (second in)]
        [_rest (rest (rest in))])
    (if (zero? num-children)
        (cons (node '() (take _rest num-meta)) (drop _rest num-meta))
        (for/fold ([children '()]
                   [rem _rest]
                   #:result (cons (node (reverse children) (take rem num-meta))
                                  (drop rem num-meta)))
                  ([i (in-range 0 num-children)])
          (let ([loop-res (tree-from-input rem)])
            (values (cons (first loop-res) children) (cdr loop-res)))))))

(define tree (first (tree-from-input input)))

(define sum (curry foldl + 0))

(define (sum-meta t)
  (+ (sum (node-meta t))
     (if (empty? (node-children t))
         0
         (sum (map sum-meta (node-children t))))))


(printf "Part 1 solution: ~a\n" (sum-meta tree))

(define (part-2-sum-meta t)
  (define children (node-children t))
  (define num-children (length children))
  (define (index-filter x) (if (> x num-children) #f (list-ref children (sub1 x))))
  (if (zero? num-children)
      (sum (node-meta t))
      (sum (map part-2-sum-meta (filter-map index-filter (node-meta t))))))

(printf "Part 2 solution: ~a\n" (part-2-sum-meta tree))
