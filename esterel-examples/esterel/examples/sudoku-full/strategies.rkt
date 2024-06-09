#lang racket/base

(require racket/set
         racket/hash
         esterel/full
         "../private/sudoku-helpers.rkt"
         "signal-utils.rkt"
         "controller.rkt")

(provide sudoku-strategies/list
         default-sudoku-strategy-names
         all-sudoku-strategy-names
         sudoku-strategies
         naked-single
         hidden-single
         naked-pair
         locked-candidates
         run-strategies)

(define-signal
  S-deduction-cannot-src #:init empty-board #:combine merge-board)


;; strategies
(define (run-strategies strategies)
  (cond
    [(null? strategies)
     (emit S-deduction-cannot-dst empty-board)]
    [else
     (loop
      (with-trap T-reset
        (for/par ([a-thread (in-list (cons (make-strategy-controller T-reset)
                                           strategies))])
          (a-thread)))
      (pause))]))

(define (make-strategy-controller T-reset)
  (define (strategy-controller-loop)
    (emit S-deduction-cannot-src empty-board)
    (emit S-deduction-cannot-dst empty-board)
    (when (signal-value S-restart? #:can (set S-deduction-cannot-dst))
      (printf "strategies: reset!\n")
      (emit S-deduction-cannot-dst empty-board)
      (exit-trap T-reset))
    (emit S-deduction-cannot-dst (signal-value S-deduction-cannot-src #:can empty-set))
    (pause)
    (strategy-controller-loop))
  strategy-controller-loop)


;; SudokuNakedSingle
(define (naked-single)
  (let naked-single-loop ([naked-single-brd empty-board])
    (emit S-board-dst naked-single-brd)
    (define avails-brd (signal-value S-board-avails #:can (set S-has-deduction?)))
    (define curr-naked-single-brd
      (for/vector #:length 9 ([i (in-range 9)])
        (for/vector #:length 9 ([j (in-range 9)])
          (define avail-ds (vector2-ref/fn avails-brd i j))
          (cond [(= 1 (length avail-ds)) (set (car avail-ds))]
                [else empty-set]))))
    (define new-naked-single-brd
      (merge-board naked-single-brd curr-naked-single-brd))
    ;; Should be unnecessary; already detected in the controller by comparing S-board-dst
    (unless (equal? new-naked-single-brd naked-single-brd)
      (emit S-has-deduction? #t))
    (pause)
    (naked-single-loop new-naked-single-brd)))


;; SudokuHiddenSingle
(define (hidden-single)
  (let hidden-single-loop ([hidden-single-brd empty-board])
    (emit S-board-dst hidden-single-brd)
    (define avails-brd (signal-value S-board-avails #:can (set S-has-deduction?)))
    (define curr-hidden-single-brd
      (for/vector #:length 9 ([i (in-range 9)])
        (for/vector #:length 9 ([j (in-range 9)])
          (define avail-ds (list->set (vector2-ref/fn avails-brd i j)))
          (apply set-union
                 (for/list ([house-type (in-list '(row col box))])
                   (define avail-ds-of-other-cells
                     (for*/list ([(i* j*) (in-house 9 house-type i j)]
                                 #:when (not (and (= i i*) (= j j*))))
                       (list->set (vector2-ref/fn avails-brd i* j*))))
                   (set-subtract avail-ds (apply set-union avail-ds-of-other-cells)))))))
    (define new-hidden-single-brd
      (merge-board hidden-single-brd curr-hidden-single-brd))
    ;; Should be unnecessary; already detected in the controller by comparing S-board-dst
    (unless (equal? new-hidden-single-brd hidden-single-brd)
      (emit S-has-deduction? #t))
    (pause)
    (hidden-single-loop new-hidden-single-brd)))


;; SudokuNakedPair
(define (naked-pair)
  (let naked-pair-loop ([naked-pair-brd empty-board])
    (define avails-brd (signal-value S-board-avails #:can (set S-has-deduction?)))
    (define curr-naked-pair-brd (make-mutable-empty-board))
    (for* ([house-type (in-list '(row col box))]
           [i (in-range 9)]
           [j (in-range 9)]
           #:when (= 2 (length (vector2-ref/fn avails-brd i j)))
           [(i* j*) (in-house 9 house-type i j)]
           #:when (not (and (= i i*) (= j j*))))
      (define avail-ds1 (list->set (vector2-ref/fn avails-brd i j)))
      (define avail-ds2 (list->set (vector2-ref/fn avails-brd i* j*)))
      (when (and (= 2 (set-count avail-ds1)) (equal? avail-ds1 avail-ds2))
        ;; (printf "naked pair [~a,~a] and [~a,~a]: uses ~a\n" i j i* j* avail-ds1)
        (define cannot-ds
          (set-subtract (list->set (build-list 9 add1)) avail-ds1))
        (for* ([(i** j**) (in-house 9 house-type i j)]
               #:when (and (not (and (= i i**) (= j j**)))
                           (not (and (= i* i**) (= j* j**)))))
          (vector-set! (vector-ref curr-naked-pair-brd i**) j**
                       (set-union avail-ds1
                                  (vector2-ref/fn curr-naked-pair-brd i** j**))))))
    (define new-naked-pair-brd
      (merge-board naked-pair-brd curr-naked-pair-brd))
    (unless (equal? new-naked-pair-brd naked-pair-brd)
      (emit S-has-deduction? #t))
    (pause)
    (emit S-deduction-cannot-src new-naked-pair-brd)
    (naked-pair-loop new-naked-pair-brd)))


;; SudokuHiddenPair
(define (hidden-pair)
  (let hidden-pair-loop ([hidden-pair-board empty-board])
    (define avails-brd (signal-value S-board-avails #:can (set S-has-deduction?)))
    (define curr-hidden-pair-board (make-mutable-empty-board))
    (for* ([house-type (in-list '(row col box))]
           [i (in-range 9)]
           [j (in-range 9)]
           [(i* j*) (in-house 9 house-type i j)]
           #:when (or (< i i*) (and (= i i*) (< j j*))))
      (define avail-ds
        (set-intersect (list->set (vector2-ref/fn avails-brd i j))
                       (list->set (vector2-ref/fn avails-brd i* j*))))
      (define avail-ds-of-other-cells
        (for*/list ([(i** j**) (in-house 9 house-type i j)]
                    #:when (and (not (and (= i i**) (= j j**)))
                                (not (and (= i* i**) (= j* j**)))))
          (list->set (vector2-ref/fn avails-brd i** j**))))
      (define remaining-avail-ds
        (set-subtract avail-ds (apply set-union avail-ds-of-other-cells)))
      (when (>= (set-count remaining-avail-ds) 2)
        ;; (printf "hidden pair [~a,~a] and [~a,~a]: uses ~a\n" i j i* j* remaining-avail-ds)
        (for ([(i** j**) (in-house 9 house-type i j)]
              #:when (and (not (and (= i i**) (= j j**)))
                          (not (and (= i* i**) (= j* j**)))))
          (vector-set! (vector-ref curr-hidden-pair-board i**) j**
                       (set-union remaining-avail-ds
                                  (vector2-ref/fn curr-hidden-pair-board i** j**))))))
    (define new-hidden-pair-board
      (merge-board hidden-pair-board curr-hidden-pair-board))
    (unless (equal? new-hidden-pair-board hidden-pair-board)
      (emit S-has-deduction? #t))
    (pause)
    (emit S-deduction-cannot-src new-hidden-pair-board)
    (hidden-pair-loop new-hidden-pair-board)))


;; Locked Candidates (or Intersection Removal)
(define (locked-candidates)
  (let locked-candidates-loop ([locked-candidates-brd empty-board])
    (define avails-brd (signal-value S-board-avails #:can (set S-has-deduction?)))
    (define curr-locked-candidates-brd (make-mutable-empty-board))
    (for* ([i0 (in-range 0 9 3)]
           [j0 (in-range 0 9 3)]
           [other-house-type (in-list '(row col))]
           [k (in-range 3)])
      (define i (+ i0 k))
      (define j (+ j0 k))
      (define avail-ds-cnt
        (apply hash-union #:combine +
               (for/list ([(i* j*) (in-house 9 other-house-type i j)]
                          #:when (same-house? 9 'box i j i* j*))
                 (set->singleton-count
                  (list->set
                   (vector2-ref/fn avails-brd i j))))))
      (define candidate-ds
        (for/set ([(d cnt) (in-hash avail-ds-cnt)]
                  #:when (> cnt 1))
          d))
      (for ([houses-type (in-list `((box . ,other-house-type) (,other-house-type . box)))])
        (define avail-ds-of-other-cells
          (apply set-union
                 (for/list ([(i* j*) (in-house 9 (car houses-type) i j)]
                            #:when (and (not (same-house? 9 (cdr houses-type) i j i* j*))))
                   (list->set (vector2-ref/fn avails-brd i* j*)))))
        (define locked-ds
          (set-subtract candidate-ds avail-ds-of-other-cells))
        (when (not (set-empty? locked-ds))
          ;; (printf "locked: ~s\n" locked-ds)
          (for ([(i* j*) (in-house 9 (cdr houses-type) i j)]
                #:when (and (not (same-house? 9 (car houses-type) i j i* j*))))
            (vector-set! (vector-ref curr-locked-candidates-brd i*) j*
                         (set-union locked-ds
                                    (vector2-ref/fn curr-locked-candidates-brd i* j*)))))))
    (define new-locked-candidates-brd
      (merge-board locked-candidates-brd curr-locked-candidates-brd))
    (unless (equal? new-locked-candidates-brd locked-candidates-brd)
      (emit S-has-deduction? #t))
    (pause)
    (emit S-deduction-cannot-src new-locked-candidates-brd)
    (locked-candidates-loop new-locked-candidates-brd)))

(define sudoku-strategies/list
  (list (cons "naked-single" naked-single)
        (cons "hidden-single" hidden-single)
        (cons "naked-pair" naked-pair)
        (cons "hidden-pair" hidden-pair)
        (cons "locked-candidates" locked-candidates)))

(define default-sudoku-strategy-names
  '("naked-single" "hidden-single" "naked-pair" "locked-candidates"))

(define all-sudoku-strategy-names
  (map car sudoku-strategies/list))

(define sudoku-strategies
  (for/hash ([name-strategy (in-list sudoku-strategies/list)])
    (values (car name-strategy) (cdr name-strategy))))
