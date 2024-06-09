#!/usr/bin/env racket
#lang racket/base

(require racket/set
         esterel/full
         "visualize.rkt"
         "signal-utils.rkt"
         "rules.rkt"
         "controller.rkt"
         "search.rkt"
         "strategies.rkt")

(provide make-sudoku)

(define (make-sudoku init-brd
                     #:strategies [strategies (map cdr sudoku-strategies/list)]
                     #:enable-searching? [enable-searching? #t])
  (esterel
   #:pre 1
   (emit S-board-src init-brd)
   (with-trap T-terminate
     (par
      ;; Sudoku board
      (run-occurrence-counter)
      (run-inconsistency-detector)
      (run-sudoku-controller)
      ;; Search: guesses with backtracking
      (run-searching-procedure T-terminate
                               #:initial-board-for-drawing init-brd
                               #:backtracking-search? enable-searching?)
      ;; Strategies
      (run-strategies strategies)))))

(module+ main
  (require "boards.rkt"
           "adaptor.rkt"
           racket/pretty
           racket/port
           racket/cmdline)

  (port-count-lines! (current-output-port))
  (pretty-print-columns 100)

  (define print-every-instant? (make-parameter #f))

  (define print-board-names? (make-parameter #f))
  (define exit-if-print-board-names? (make-parameter #t))
  (define sudoku-board-name (make-parameter "web-board-1"))

  (define print-strategy-names? (make-parameter #f))
  (define exist-if-print-strategy-names? (make-parameter #t))
  (define enabled-sudoku-strategies (make-parameter #f))

  (define enable-searching? (make-parameter #t))

  (define output-port (make-parameter (current-output-port)))

  (command-line
   #:once-any
   ["--disable-searching" "Disable the searching procedure."
                          (enable-searching? #f)]
   ["--enable-searching" "Enable the searching procedure. This is the default."
                         (enable-searching? #t)]
   #:once-each
   ["--list" "Print available Sudoku boards"
             (print-board-names? #t)]
   ["--board" name "Solve the chosen Sudoku board"
              (sudoku-board-name name)
              (exit-if-print-board-names? #f)]
   #:once-any
   ["--print-board" "Print the board for every instant"
                    (print-every-instant? 'board)]
   ["--print-all" "Print the signals for every instant"
                  (print-every-instant? #t)]
   #:once-each
   ["--color" when
              "Show colored board in text mode. <when> can be one of always, never, or auto."
              (unless (member when '("always" "never" "auto"))
                (error 'racket-sudoku.rkt
                       "--color must be followed by always, never, or auto. Given: ~a."
                       when))
              (text-has-color (string->symbol when))]
   ["--disable-logs" "Do not print the logs from the backtracking searcher"
                     (output-port (open-output-nowhere))]
   ["--list-strategies" "Print available Sudoku strategies"
                        (print-strategy-names? #t)]
   #:multi
   ["--strategy" name
                 "Enable the named Sudoku strategy"
                 (enabled-sudoku-strategies
                  (cons name (or (enabled-sudoku-strategies) '())))
                 (exist-if-print-strategy-names? #f)]
   ["--no-strategies" "Disable all Sudoku strategies"
                      (enabled-sudoku-strategies '())
                      (exist-if-print-strategy-names? #f)])

  (define (print-all-sudoku-board-names)
    (printf "Available Sudoku boards:\n")
    (for ([board-name (in-list (map car all-sudoku-boards/list))])
      (printf "    ~a\n" board-name)))

  (define (print-all-strategy-names)
    (printf "Available Sudoku strategies:\n")
    (for ([strategy-name (in-list all-sudoku-strategy-names)])
      (printf "    ~a\n" strategy-name)))

  (define assoc-board
    (hash-ref all-sudoku-boards (sudoku-board-name) #f))
  (when (not assoc-board)
    (printf "No Sudoku board named ~a\n" (sudoku-board-name))
    (print-all-sudoku-board-names)
    (exit 1))
  (when (print-board-names?)
    (print-all-sudoku-board-names)
    (when (exit-if-print-board-names?)
      (exit 0)))

  (define selected-strategies-in-names
    (cond [(enabled-sudoku-strategies) => reverse]
          [else default-sudoku-strategy-names]))
  (for ([strategy-in-name (in-list selected-strategies-in-names)])
    (unless (hash-has-key? sudoku-strategies strategy-in-name)
      (printf "No Sudoku strategy named ~a\n" strategy-in-name)
      (print-all-strategy-names)
      (exit 1)))
  (when (print-strategy-names?)
    (print-all-strategy-names)
    (when (exist-if-print-strategy-names?)
      (exit 0)))

  (define init-brd
    (assoc-board->generic-board assoc-board))
  (define selected-strategies
    (for/list ([strategy-in-name (in-list selected-strategies-in-names)])
      (hash-ref sudoku-strategies strategy-in-name)))

  (printf "Solving ~a with ~a"
          (sudoku-board-name)
          (if (null? selected-strategies-in-names) "no strategy" "strategies:"))
  (for ([strategy-in-name (in-list selected-strategies-in-names)])
    (printf " ~a" strategy-in-name))
  (newline)
  (displayln (draw-board (make-vector 9 (make-vector 9 empty-set)) init-brd))

  (define sudoku
    (parameterize ([current-output-port (output-port)])
      (make-sudoku init-brd
                   #:strategies selected-strategies
                   #:enable-searching? (enable-searching?))))

  (define sgls-final
    (for/fold ([sgls (react! sudoku)])
              ([i (in-range (* (* 15 60) 100))] ;; Run for ~15mins
               #:break (or (hash-ref sgls S-solved #f) (hash-ref sgls S-failed #f)))
      (parameterize ([current-output-port (if (print-every-instant?)
                                              (current-output-port)
                                              (output-port))])
        (printf "============================== ~a ==============================\n" i))
      (define sgls (react! sudoku))
      (when (or (print-every-instant?) (hash-ref sgls S-failed #f))
        (define curr-brd (hash-ref sgls S-board-dst))
        (when (and (memq (print-every-instant?) '(#t board))
                   curr-brd
                   (not (hash-ref sgls S-solved #f)))
          (displayln (draw-board curr-brd init-brd)))
        (when (or (eq? #t (print-every-instant?)) (hash-ref sgls S-failed #f))
          (pretty-print sgls)))
      sgls))

  (cond
    [(hash-ref sgls-final S-solved #f)
     (printf "Solution:\n")
     (displayln (draw-board (hash-ref sgls-final S-solved #f) init-brd))]
    [(hash-ref sgls-final S-failed #f)
     (printf "Failed to solve Sudoku board\n")
     (exit 1)])
  )
