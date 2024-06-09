#lang racket/base

(require '#%terminal ffi/unsafe/port
         racket/port
         racket/set
         2htdp/image)

(provide empty-cell
         cell
         text-has-color
         text-cell
         draw-board
         draw-board/text
         draw-board/2htdp-image)

(define empty-cell
  (frame (empty-scene 30 30)))

(define (cell digit/s color)
  (define texts
    (for/list ([a-digit (in-list (if (list? digit/s) digit/s (list digit/s)))])
      (text (format "~a" a-digit) 20 color)))
  (apply overlay
         (append texts (list empty-cell))))

;; https://github.com/racket/racket/blob/4b9f5a8715cba1d8214d75e0f39ef6b989780786/racket/collects/setup/private/setup-fprintf.rkt#L19-L28
(define terminal-available?
  (let ()
    (define op (current-output-port))
    (define ip (current-input-port))
    (define in-fd (unsafe-port->file-descriptor ip))
    (define out-fd (unsafe-port->file-descriptor op))
    (and in-fd
         out-fd
         (terminal-init in-fd out-fd))))

(define text-has-color
  (make-parameter
   'auto
   (λ (c)
     (unless (memq c '(always never auto))
       (raise-argument-error 'text-has-color
                             "(or/c 'always 'never 'auto)"
                             c))
     c)))

(define (text-cell digit color)
  (define has-color (text-has-color))
  (cond
    [(if (eq? has-color 'auto)
         terminal-available?
         (eq? has-color 'always))
     (format "\u1b[~am~a\u1b[m" color digit)]
    [else (format "~a" digit)]))

(define (draw-board solved-board initial-board
                    #:color-coord [color-coord #f])
  (cond
    [(port-writes-special? (current-output-port))
     (draw-board/2htdp-image solved-board initial-board color-coord)]
    [else
     (draw-board/text solved-board initial-board color-coord)]))

(define (draw-board/text solved-board initial-board color-coord)
  (with-output-to-string
    (λ ()
      (for ([i (in-range 9)])
        (for ([j (in-range 9)])
          (define ds
            (if initial-board
                (vector-ref (vector-ref initial-board i) j)
                (set)))
          (cond [(and color-coord
                      (equal? (car color-coord) (cons i j)))
                 (write-string (text-cell (cdr color-coord) "1;31"))]
                [(not (set-empty? ds))
                 (write-string (text-cell (set-first ds) "1;37"))]
                [(vector-ref (vector-ref solved-board i) j)
                 => (λ (digits)
                      (cond [(set-empty? digits) (write-char #\space)]
                            [(> (set-count digits) 1) (write-string (text-cell #\* "1;35"))]
                            [else (write-string (text-cell (set-first digits) "1;36"))]))]
                [else
                 (write-char #\space)])
          (when (or (= j 2) (= j 5))
            (write-char #\|)))
        (write-char #\newline)
        (when (or (= i 2) (= i 5))
          (write-string "---+---+---\n"))))))

(define (draw-board/2htdp-image solved-board initial-board color-coord)
  (apply above
         (for/list ([i (in-range 9)])
           (define cells-img
             (for/list ([j (in-range 9)])
               (define ds
                 (if initial-board
                     (vector-ref (vector-ref initial-board i) j)
                     (set)))
               (define cell-img
                 (cond [(and color-coord
                             (equal? (car color-coord) (cons i j)))
                        (cell (cdr color-coord) "red")]
                       [(not (set-empty? ds))
                        (cell (set-first ds) "black")]
                       [(vector-ref (vector-ref solved-board i) j)
                        => (λ (digits)
                             (cond [(set-empty? digits) empty-cell]
                                   [(> (set-count digits) 1)
                                    (cell (set->list digits) "violet")]
                                   [else (cell (set-first digits) "blue")]))]
                       [else
                        empty-cell]))
               (if (or (= j 2) (= j 5))
                   (beside cell-img (rectangle 2 30 "solid" "black"))
                   cell-img)))
           (define row-img
             (apply beside cells-img))
           (if (or (= i 2) (= i 5))
               (above row-img
                      (apply beside
                             (for/list ([j (in-range 9)])
                               (rectangle 30 2 "solid" "black"))))
               row-img))))
