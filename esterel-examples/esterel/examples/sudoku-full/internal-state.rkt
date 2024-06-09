#lang racket/base

(require esterel/kernel
         "signal-utils.rkt")

(provide S-reject
         S-board-dst
         S-deduction-cannot-dst
         S-board-avails
         S-row-digits
         S-col-digits
         S-box-digits
         emit-house-digits
         emit-box-digits
         no-occurrence?)

(define-signal
  S-board-dst #:init empty-board #:combine merge-board)

(define-signal
  S-deduction-cannot-dst #:init empty-board #:combine merge-board
  S-board-avails #:single)

(define-signal
  S-reject #:single)

(define-signal
  S-row-digits
  #:init (vector->immutable-vector (make-vector 9 #hash()))
  #:combine merge-house

  S-col-digits
  #:init (vector->immutable-vector (make-vector 9 #hash()))
  #:combine merge-house

  S-box-digits
  #:init (vector->immutable-vector
          (make-vector 3 (vector->immutable-vector
                          (make-vector 3 #hash()))))
  #:combine merge-box)

(define (no-occurrence? hrow hcol hbox i j d)
  (and (= 0 (hash-ref (vector-ref/fn hrow i) d 0))
       (= 0 (hash-ref (vector-ref/fn hcol j) d 0))
       (= 0 (hash-ref (vector2-ref/fn hbox (quotient i 3) (quotient j 3)) d 0))))

(define (emit-house-digits S-row/col k* cnt)
  (emit S-row/col (λ (k)
                    (if (= k k*)
                        cnt
                        #hash()))))

(define (emit-box-digits i* j* cnt)
  (emit S-box-digits (λ (i j)
                       (if (and (= i i*) (= j j*))
                           cnt
                           #hash()))))
