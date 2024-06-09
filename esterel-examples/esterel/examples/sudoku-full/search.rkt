#lang racket/base

(require racket/set
         esterel/full
         "visualize.rkt"
         "signal-utils.rkt"
         "controller.rkt")

(provide S-solved
         S-failed
         run-searching-procedure)

(define-signal
  S-solved #:single
  S-failed)

;; the backtracking searcher
(define (run-searching-procedure T-terminate
                                 #:initial-board-for-drawing [init-brd #f]
                                 #:backtracking-search? [enable-searching? #t])
  (await #:immediate (signal-value S-ready #:can empty-set))
  (define brd (signal-value S-board-dst #:can empty-set))
  (when (signal-value S-bad-board #:can empty-set)
    (exit-trap T-terminate))
  (let search ([i 0] [j 0] [brd brd])
    (cond
      [(= i 9)
       (printf "search(~a,~a): solved\n" i j)
       (emit S-solved brd)
       (exit-trap T-terminate)]
      [(= j 9)
       (search (+ i 1) 0 brd)]
      [(not (set-empty? (vector2-ref/fn brd i j)))
       (printf "search(~a,~a): already filled with ~a\n"
               i j
               (vector2-ref/fn brd i j))
       (search i (+ j 1) brd)]
      ;; When backtracking search is not enabled, enter stuttering mode.
      [(not enable-searching?)
       (pause)
       (search i j (signal-value S-board-dst #:can empty-set))]
      ;; Backtracking search (or, guessing). enable-searching? must be #t.
      [else
       (define avails-brd (signal-value S-board-avails #:can empty-set))
       (for ([d (in-list (vector2-ref/fn avails-brd i j))])
         ;; Must pause: in the current instance, we have been querying the value
         ;; of hrow, hcol, hbox, and perhaps S-bad-board, S-ready, S-board-dst, etc.
         (pause)
         (printf "search(~a,~a): filling in ~a; avl digits: ~a; awaiting for S-ready\n"
                 i j
                 d
                 (vector2-ref/fn avails-brd i j))
         (displayln
          (draw-board brd
                      init-brd
                      #:color-coord (cons (cons i j) d)))
         (emit S-board-src brd)
         (emit-board-digits S-board-src i j (set d))
         (emit S-restart? #t)
         ;; w/wo #:immediate should not matter because S-ready looks for #:pre
         (await #:immediate (signal-value S-ready #:can empty-set))
         (cond
           [(signal-value S-bad-board #:can empty-set)
            => (λ (reason)
                 (printf "search(~a,~a): ~a rejected due to ~a\n" i j d reason))]
           [else
            (search i (+ j 1) (signal-value S-board-dst #:can empty-set))]))
       (printf "search(~a,~a): exhausted\n" i j)]))
  (printf "search failed\n")
  (emit S-failed)
  (exit-trap T-terminate))
