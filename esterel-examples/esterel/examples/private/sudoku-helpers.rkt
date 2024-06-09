#lang racket/base
(module+ test (require rackunit))
(require racket/vector racket/match racket/sequence
         (for-syntax racket/base))

(provide compute-cols/rows/boxes
         compute-houses
         ij->square
         same-house?
         in-house)

(define (compute-houses cells size)
  (define-values (cols rows boxes)
    (compute-cols/rows/boxes cells size))
  (vector-append cols rows boxes))

(define (compute-cols/rows/boxes cells size)
  (define cols
    (for/vector ([x (in-range size)])
      (for/vector ([(y _) (in-house size 'col 0 x)])
        (hash-ref cells (cons x y)))))
  (define rows
    (for/vector ([y (in-range size)])
      (for/vector ([(_ x) (in-house size 'row y 0)])
        (hash-ref cells (cons x y)))))
  (define boxes
    (for/vector ([corner (in-list (get-square-corners size))])
      (for/vector ([(y x) (in-house size 'box (cdr corner) (car corner))])
        (hash-ref cells (cons x y)))))
  (values cols rows boxes))

(define (get-square-corners size)
  (for/list ([i (in-range size)])
    (define sx (* (sqrt size) (modulo i (sqrt size))))
    (define sy (* (sqrt size) (quotient i (sqrt size))))
    (cons sx sy)))
(module+ test
  (check-equal?
   (get-square-corners 4)
   (list (cons 0 0) (cons 2 0) (cons 0 2) (cons 2 2))))

(define (ij->square size i j)
  (+ (* (quotient j (sqrt size)) (sqrt size))
     (quotient i (sqrt size))))
(module+ test
  (check-equal? (ij->square 4 0 0) 0)
  (check-equal? (ij->square 4 1 1) 0)
  (check-equal? (ij->square 4 0 1) 0)
  (check-equal? (ij->square 4 1 0) 0)
  (check-equal? (ij->square 4 2 0) 1)
  (check-equal? (ij->square 4 3 0) 1)
  (check-equal? (ij->square 4 2 1) 1)
  (check-equal? (ij->square 4 3 1) 1)
  (check-equal? (ij->square 4 0 2) 2)
  (check-equal? (ij->square 4 0 3) 2)
  (check-equal? (ij->square 4 1 2) 2)
  (check-equal? (ij->square 4 1 3) 2)
  (check-equal? (ij->square 4 3 3) 3))



(define (same-house? size house-type i j i* j*)
  (define board-length (sqrt size))
  (or (and (or (not house-type) (eq? house-type 'row)) (= i i*))
      (and (or (not house-type) (eq? house-type 'col)) (= j j*))
      (and (or (not house-type) (eq? house-type 'box))
           (= (quotient i board-length) (quotient i* board-length))
           (= (quotient j board-length) (quotient j* board-length)))))

(define (make-in-house-pos->element size house-type i j)
  (unless (memq house-type '(row col box))
    (raise-argument-error 'in-house
                          "(or/c 'row 'col 'box)"
                          2
                          i
                          j
                          house-type))
  (define board-length (sqrt size))
  (match house-type
    ['row (λ (k) (values i k))]
    ['col (λ (k) (values k j))]
    ['box
     (define i0 (* board-length (quotient i board-length)))
     (define j0 (* board-length (quotient j board-length)))
     (λ (k) (values (+ i0 (quotient k board-length)) (+ j0 (modulo k board-length))))]))

(define-sequence-syntax in-house
  (λ () #'in-house/proc)
  (λ (stx)
    (syntax-case stx ()
      [[(i* j*) (_ size-expr house-type-expr i-expr j-expr)]
       (syntax/loc stx
         [(i* j*)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(size) size-expr]
            [(pos->element) (make-in-house-pos->element size-expr house-type-expr i-expr j-expr)])
           ;; outer-defn-or-expr
           (begin)
           ;; ([loop-id loop-expr] ...)
           ([k 0])
           ;; pos-guard
           (< k size)
           ;; ([(inner-id ...) inner-expr] ...)
           ([(i* j*) (pos->element k)])
           ;; pre-guard
           #t
           ;; post-guard
           #t
           ;; loop-arg ...
           ((add1 k)))])]
      [_ #f])))

(define (in-house/proc size house-type i j)
  (make-do-sequence
   (λ ()
     (initiate-sequence
      #:init-pos 0 #:next-pos add1
      #:pos->element (make-in-house-pos->element size house-type i j)
      #:continue-with-pos? (λ (k) (< k size))))))

(define (transpose v)
  (define w (vector-length (vector-ref v 0)))
  (for/vector ([i (in-range (vector-length v))])
    (for/vector ([j (in-range w)])
      (vector-ref (vector-ref v j) i))))
(module+ test
  (check-equal? (transpose #(#(1 2 3 4)
                             #(a b c d)
                             #(9 8 7 6)
                             #(x y z w)))
                #(#(1 a 9 x)
                  #(2 b 8 y)
                  #(3 c 7 z)
                  #(4 d 6 w))))
