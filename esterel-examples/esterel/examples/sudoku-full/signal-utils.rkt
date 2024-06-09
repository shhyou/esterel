#lang racket/base

(require racket/set
         racket/hash
         esterel/full)

(provide (all-defined-out))

(define (vector-ref/fn v i)
  (if (vector? v)
      (vector-ref v i)
      (v i)))

(define (vector2-ref/fn v i j)
  (if (vector? v)
      (vector-ref (vector-ref v i) j)
      (v i j)))

(define (set->singleton-count ds)
  (for/hash ([d (in-set ds)])
    (values d 1)))

(define empty-set (set))

(define empty-board
  (vector->immutable-vector
   (make-vector 9
                (vector->immutable-vector
                 (make-vector 9 empty-set)))))

(define (make-mutable-empty-board)
  (build-vector 9
                (λ (i)
                  (build-vector 9 (λ (j) empty-set)))))

(define (merge-board brd1 brd2)
  (for/vector #:length 9 ([i (in-range 9)])
    (for/vector #:length 9 ([j (in-range 9)])
      (set-union (vector2-ref/fn brd1 i j)
                 (vector2-ref/fn brd2 i j)))))

(define (emit-board-digits brd i* j* ds)
  (emit brd (λ (i j)
              (if (and (= i i*) (= j j*))
                  ds
                  empty-set))))

(define (merge-house h1 h2)
  (for/vector #:length 9 ([k (in-range 9)])
    (hash-union (vector-ref/fn h1 k)
                (vector-ref/fn h2 k)
                #:combine +)))

(define (merge-box b1 b2)
  (for/vector #:length 3 ([i (in-range 3)])
    (for/vector #:length 3 ([j (in-range 3)])
      (hash-union (vector2-ref/fn b1 i j)
                  (vector2-ref/fn b2 i j)
                  #:combine +))))
