#lang racket/base

(require racket/set
         racket/vector
         esterel/kernel
         "internal-state.rkt"
         "signal-utils.rkt")

(provide run-occurrence-counter
         run-inconsistency-detector)

;; occurrence observer
(define (run-occurrence-counter)
  (define brd
    (signal-value S-board-dst
                  #:can (set S-row-digits S-col-digits S-box-digits S-board-avails)))
  (for* ([i (in-range 9)]
         [j (in-range 9)])
    (define ds (vector2-ref/fn brd i j))
    (define cnt (set->singleton-count ds))
    (emit-house-digits S-row-digits i cnt)
    (emit-house-digits S-col-digits j cnt)
    (emit-box-digits (quotient i 3) (quotient j 3) cnt))

  (define dcannot (signal-value S-deduction-cannot-dst #:can (set S-board-avails)))
  (define hrow (signal-value S-row-digits #:can (set S-board-avails)))
  (define hcol (signal-value S-col-digits #:can (set S-board-avails)))
  (define hbox (signal-value S-box-digits #:can (set S-board-avails)))
  (emit S-board-avails
        (for/vector #:length 9 ([i (in-range 9)])
          (for/vector #:length 9 ([j (in-range 9)])
            (cond
              [(not (set-empty? (vector2-ref/fn brd i j)))
               (list (set-first (vector2-ref/fn brd i j)))]
              [else
               ;; reverse order seems to be worse and take more time
               ;; this let us run more instants
               (for/list ([d (in-range 9 0 -1)]
                          #:when
                          (and (not (set-member? (vector2-ref/fn dcannot i j) d))
                               (no-occurrence? hrow hcol hbox i j d)))
                 d)]))))
  (pause)
  (run-occurrence-counter))

;; inconsistency observer
(define (run-inconsistency-detector)
  (define brd (signal-value S-board-dst #:can (set S-reject)))

  ;; multiple-"must" caused by deductions
  (define reject-by-multiple-must
    (for*/or ([i (in-range 9)]
              [j (in-range 9)])
      (define ds (vector2-ref/fn brd i j))
      (and (> (set-count ds) 1)
           (list (hash 'reason 'multiple-digits-filled 'at (cons i j) 'digit ds)))))

  ;; the "cannots" inconsistency caused by deductions
  (define dcannot (signal-value S-deduction-cannot-dst #:can (set S-reject)))
  (define reject-by-dcannot
    (for*/or ([i (in-range 9)]
              [j (in-range 9)])
      (define ds (vector2-ref/fn brd i j))
      (and (not (set-empty? (set-intersect ds (vector2-ref/fn dcannot i j))))
           (list (hash 'reason 'deducted-cannot
                       'at (cons i j)
                       'digits ds
                       'cannot (vector2-ref/fn dcannot i j))))))

  ;; inconsistency caused by duplicate numbers in a house caused by deductions
  (define hrow (signal-value S-row-digits #:can (set S-reject)))
  (define hcol (signal-value S-col-digits #:can (set S-reject)))
  (define hbox (signal-value S-box-digits #:can (set S-reject)))
  (define reject-by-inconsistent
    (for*/or ([hsh (in-vector (apply vector-append hrow hcol (vector->list hbox)))]
              [cnt (in-hash-values hsh)])
      (and (> cnt 1) (list (hash 'reason 'duplicate-digits-in-house 'occurrences hsh)))))

  ;; some cells have no available digits
  (define avails-brd (signal-value S-board-avails #:can (set S-reject)))
  (define reject-by-exhaustion
    (for*/or ([i (in-range 9)]
              [j (in-range 9)])
      (and (null? (vector2-ref/fn avails-brd i j))
           (list (hash 'reason 'no-avail-digits 'at (cons i j))))))

  (emit S-reject
        (let ([reason (append (or reject-by-multiple-must '())
                              (or reject-by-dcannot '())
                              (or reject-by-inconsistent '())
                              (or reject-by-exhaustion '()))])
          (and (not (null? reason)) reason)))
  (pause)
  (run-inconsistency-detector))
