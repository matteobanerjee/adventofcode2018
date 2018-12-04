#lang racket

(define input (file->lines "./input"))

(struct point (x y) #:transparent)
;; TODO validate tr > bl
(struct bounding-box (bottom-left top-right name) #:transparent)

(define spec-regexp #rx"(^.*) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)")

(define (spec-to-bb spec-str)
  (define parsed (regexp-match spec-regexp spec-str))
  (define name (second parsed))
  (define bb-spec (map string->number (rest (rest parsed))))
  ;; Note: grid is inverted so i can pretend these are on a normal x-y grid
  (let* ([bottom-left (point (list-ref bb-spec 0) (list-ref bb-spec 1))]
         [dims (list (list-ref bb-spec 2) (list-ref bb-spec 3))])
    (bounding-box
     bottom-left
     (point (+ (point-x bottom-left) (first dims))
            (+ (point-y bottom-left) (second dims)))
     name)))

;; Part 1
(define (bb-contains bb pt)
  (let ([x (point-x pt)]
        [y (point-y pt)]
        [bl (bounding-box-bottom-left bb)]
        [tr (bounding-box-top-right bb)])
    (and
     (>= x (point-x bl))
     (>= y (point-y bl))
     (<= x (point-x tr))
     (<= y (point-y tr)))))

;; avoid double counting boundary points by checking the 1x1 area originating at
;; at the point is fully contained in the bounding box
(define (bb-contains-1x1-area bb bottom-left-pt)
  (define (pt-plus-one pt)
    (point (+ 1 (point-x pt)) (+ 1 (point-y pt))))
  (and
   (bb-contains bb bottom-left-pt)
   (bb-contains bb (pt-plus-one bottom-left-pt))))

(define (get-containing-bbs bounding-boxes pt)
  (filter (λ (bb) (bb-contains-1x1-area bb pt)) bounding-boxes))

(define (set-add-all s l)
  (foldl (λ (v accum) (set-add accum v)) s l))

(define (find-overlap-set-2 grid-len grid-height bounding-boxes)
  ;; compute solutions to 1 and 2 in one pass
  (for*/fold ([num-overlapping 0]
              [bbs-with-overlap (set)])
             ([x (in-range 0 grid-len)]
              [y (in-range 0 grid-height)])
    (let* ([pt (point x y)]
           [containing-bbs (get-containing-bbs bounding-boxes pt)])
      (if (> (length containing-bbs) 1)
          (values (+ num-overlapping 1) (set-add-all bbs-with-overlap containing-bbs))
          (values num-overlapping bbs-with-overlap)))))

(define input-bounding-boxes (map spec-to-bb input))

(define-values (part-1-solution part-2-bounding-boxes-with-overlap)
  (find-overlap-set-2 1000 1000 input-bounding-boxes))

(printf "Part 1 Solution2: ~a\n" part-1-solution)

(printf "Part 2 Solution: ~a\n"
        (filter-not
         (curry set-member? part-2-bounding-boxes-with-overlap)
         input-bounding-boxes))
