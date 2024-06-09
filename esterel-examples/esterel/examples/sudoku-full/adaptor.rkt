#lang racket/base

(require racket/set
         "boards.rkt")

(provide (except-out (all-defined-out)
                     empty-set))

(define empty-set (set))

(define (assoc-board->generic-board assoc-board)
  (for/vector #:length 9 ([i (in-range 9)])
    (for/vector #:length 9 ([j (in-range 9)])
      (define d (assoc (cons i j) assoc-board))
      (if d (set (cdr d)) empty-set))))

(define (generic-board->assoc-board generic-board)
  (for*/list ([i (in-range 9)]
              [j (in-range 9)]
              #:when (not (set-empty? (vector-ref (vector-ref generic-board i) j))))
    (cons (cons i j)
          (set-first (vector-ref (vector-ref generic-board i) j)))))

(define all-racket-boards
  (map assoc-board->generic-board all-boards))

(module+ test
  (require "boards.rkt" rackunit)

  (check-equal? (map generic-board->assoc-board all-racket-boards)
                all-boards)

  (check-equal?
   (assoc-board->generic-board web-board-1)
   `#(#(,(set) ,(set) ,(set) ,(set 2)  ,(set 6)  ,(set)  ,(set 7) ,(set)  ,(set 1))
      #( ,(set 6)  ,(set 8) ,(set) ,(set)  ,(set 7) ,(set) ,(set)  ,(set 9) ,(set))
      #( ,(set 1)  ,(set 9) ,(set) ,(set) ,(set)  ,(set 4)  ,(set 5) ,(set) ,(set))
      #( ,(set 8)  ,(set 2) ,(set)  ,(set 1) ,(set) ,(set) ,(set)  ,(set 4) ,(set))
      #(,(set) ,(set)  ,(set 4)  ,(set 6) ,(set)  ,(set 2)  ,(set 9) ,(set) ,(set))
      #(,(set)  ,(set 5) ,(set) ,(set) ,(set)  ,(set 3) ,(set)  ,(set 2)  ,(set 8))
      #(,(set) ,(set)  ,(set 9)  ,(set 3) ,(set) ,(set) ,(set)  ,(set 7)  ,(set 4))
      #(,(set)  ,(set 4) ,(set) ,(set)  ,(set 5) ,(set) ,(set)  ,(set 3)  ,(set 6))
      #(,(set 7) ,(set)  ,(set 3) ,(set)  ,(set 1)  ,(set 8) ,(set) ,(set) ,(set)))))
