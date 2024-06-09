#lang racket/base

(require racket/set
         esterel/kernel
         "signal-utils.rkt"
         "internal-state.rkt")

(provide S-board-src
         S-restart?
         S-has-deduction?

         S-board-dst
         S-board-avails
         S-deduction-cannot-dst

         S-bad-board
         S-ready

         run-sudoku-controller)

(define-signal
  S-board-src #:init empty-board #:combine merge-board)

(define-signal
  S-bad-board #:single
  S-ready #:single
  S-restart? #:init #f #:combine (λ (b1 b2) (or b1 b2))
  S-has-deduction? #:init #f #:combine (λ (b1 b2) (or b1 b2)))

;; the controller
(define (run-sudoku-controller)
  (define brd
    (signal-value S-board-src
                  #:can (set S-restart? S-has-deduction?
                             S-board-dst S-bad-board S-ready)))
  (let loop ([brd brd])
    (emit S-restart? #f)
    (emit S-has-deduction? #f)
    (define restarted?
      (signal-value S-restart? #:can (set S-board-dst S-bad-board S-ready)))
    (define new-brd
      (if restarted?
          (signal-value S-board-src #:can (set S-board-dst S-bad-board S-ready))
          brd))
    (emit S-board-dst new-brd)
    (define has-progress?
      (signal-value S-has-deduction? #:can (set S-bad-board S-ready)))
    (define rejected?/reason
      (signal-value S-reject #:can (set S-bad-board S-ready)))
    (emit S-bad-board rejected?/reason)
    (emit S-ready (or (and (not restarted?) rejected?/reason #t)
                      (not has-progress?)))
    (pause)
    (loop new-brd)))
