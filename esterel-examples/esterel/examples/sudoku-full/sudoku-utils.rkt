#lang racket/base

(require racket/match
         racket/sequence
         (for-syntax racket/base))

(provide same-house?
         in-house
         all-houses)

(define (same-house? house-type i j i* j*)
  (or (and (or (not house-type) (eq? house-type 'row)) (= i i*))
      (and (or (not house-type) (eq? house-type 'col)) (= j j*))
      (and (or (not house-type) (eq? house-type 'box))
           (= (quotient i 3) (quotient i* 3))
           (= (quotient j 3) (quotient j* 3)))))

(define (make-in-house-pos->element house-type i j)
  (unless (memq house-type '(row col box))
    (raise-argument-error 'in-house
                          "(or/c 'row 'col 'box)"
                          2
                          i
                          j
                          house-type))
  (match house-type
    ['row (λ (k) (values i k))]
    ['col (λ (k) (values k j))]
    ['box
     (define i0 (* 3 (quotient i 3)))
     (define j0 (* 3 (quotient j 3)))
     (λ (k) (values (+ i0 (quotient k 3)) (+ j0 (modulo k 3))))]))

(define (in-house/proc house-type i j)
  (make-do-sequence
   (λ ()
     (initiate-sequence
      #:init-pos 0 #:next-pos add1
      #:pos->element (make-in-house-pos->element house-type i j)
      #:continue-with-pos? (λ (k) (< k 9))))))

(define-sequence-syntax in-house
  (λ () #'in-house/proc)
  (λ (stx)
    (syntax-case stx ()
      [[(i* j*) (_ house-type-expr i-expr j-expr)]
       (syntax/loc stx
         [(i* j*)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(pos->element) (make-in-house-pos->element house-type-expr i-expr j-expr)])
           ;; outer-defn-or-expr
           (begin)
           ;; ([loop-id loop-expr] ...)
           ([k 0])
           ;; pos-guard
           (< k 9)
           ;; ([(inner-id ...) inner-expr] ...)
           ([(i* j*) (pos->element k)])
           ;; pre-guard
           #t
           ;; post-guard
           #t
           ;; loop-arg ...
           ((add1 k)))])]
      [_ #f])))

(define all-houses
  (append
   ;; rows
   (for/list ([i (in-range 9)])
     (for/list ([j (in-range 9)])
       (cons i j)))

   ;; columns
   (for/list ([j (in-range 9)])
     (for/list ([i (in-range 9)])
       (cons i j)))

   ;; 3x3 subboards
   (for/list ([i (in-range 9)])
     (define i0 (* 3 (modulo i 3)))
     (define j0 (* 3 (quotient i 3)))
     (for/list ([j (in-range 9)])
       (cons (+ i0 (modulo j 3))
             (+ j0 (quotient (* j 3) 9)))))))
