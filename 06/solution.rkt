#lang racket

(define input (file->lines "./input"))

(struct point (x y) #:transparent)

(define (manhattan-dist p1 p2)
  (+ (abs (- (point-x p1) (point-x p2)))
     (abs (- (point-y p1) (point-y p2)))))

(define input-points
  (for/list ([l (in-list input)])
    (let ([split (map string->number (string-split l ", "))])
      (point (first split) (second split)))))


;; count occurrences of elements in a list, uses a mutable has because it's fast
(define (counts l)
  (define h (make-hash))
  (for ([e (in-list l)])
    (hash-update! h e add1 0))
  h)

;; similar to argmin, but returns false if the value is not unique
(define (argmin-unique proc l)
  (for/fold ([cur-min +inf.0]
             [cur-min-element #f]
             [uniq #f]
             #:result (and uniq cur-min-element))
            ([element (in-list l)])
    (let ([v (apply proc (list element))])
      (cond
        [(< v cur-min) (values v element #t)]
        [(= v cur-min) (values cur-min #f #f)]
        [else (values cur-min cur-min-element uniq)]))))

(define (part-1-solution points)
  ;; grid is infinte, but the search area is just the bounding box around all the points
  (let ([min-x (argmin identity (map point-x points))]
        [min-y (argmin identity (map point-y points))]
        [max-x (argmax identity (map point-x points))]
        [max-y (argmax identity (map point-y points))])
    (define (nearest-point p) (argmin-unique (curry manhattan-dist p) points))
    ;; this could be more elegant
    (define boundary-points
      (set-union
       (map (λ (x) (point x max-y)) (range min-x (+ 1 max-x)))
       (map (λ (x) (point x min-y)) (range min-x (+ 1 max-x)))
       (map (λ (y) (point min-x y)) (range min-y (+ 1 max-y)))
       (map (λ (y) (point max-x y)) (range min-y (+ 1 max-y)))))
    ;; all the points that are nearest neighbors of boundary points will have
    ;; infite area (using the manhattan distance)
    (define infinite-area-points
      (for/set ([p (in-set boundary-points)]) (nearest-point p)))
    (define areas
      (counts (for*/list ([x (in-range min-x (+ 1 max-x))]
                          [y (in-range min-y (+ 1 max-y))])
                (nearest-point (point x y)))))

    (hash-remove! areas #f)
    (set-for-each infinite-area-points (curry hash-remove! areas))
    (cdr (argmax cdr (hash->list areas)))))


(printf "Part 1 Solution: ~a\n" (part-1-solution input-points))

