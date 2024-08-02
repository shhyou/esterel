#lang racket
(require pict racket/gui/base)

(provide sudoku-gui
         ht->pict)

(define (add-lines size p)
  (define w (pict-width p))
  (define h (pict-width p))
  (cc-superimpose
   (dc
    (λ (dc dx dy)
      (define (draw-lines heavy?)
        (for ([n (in-range 1 size)])
          (when (equal? heavy? (zero? (modulo n (sqrt size))))
            (send dc set-pen
                  (if heavy? "black" "gray")
                  (if heavy? 3 1)
                  'solid)
            (send dc draw-line
                  (+ dx (* n (/ w size)))
                  dy
                  (+ dx (* n (/ w size)))
                  (+ dy h))
            (send dc draw-line
                  dx
                  (+ dy (* n (/ h size)))
                  (+ dx w)
                  (+ dy (* n (/ h size)))))))
      (define pen (send dc get-pen))
      (draw-lines #f)
      (draw-lines #t)
      (send dc set-pen pen))
    w h)
   p))

(define (ht->pict size ht)
  (for/fold ([p (add-lines size (blank 400 400))])
            ([i (in-range size)])
    (for/fold ([p p])
              ([j (in-range size)])
      (define-values (must-be cannot-be pre-cannot-be)
        (match (hash-ref ht (cons i j))
          [(list must-be cannot-be pre-cannot-be)
           (values must-be cannot-be pre-cannot-be)]
          [(cons must-be cannot-be) (values must-be cannot-be #f)]))
      (define w (/ (pict-width p) size))
      (define h (/ (pict-height p) size))
      (define x (* i w))
      (define y (* j h))
      (define cannot-be-p
        (cannot-picts->pict
         (cannots->cannot-picts size cannot-be)))
      (define pre-cannot-be-p
        (and pre-cannot-be
             (cannot-picts->pict
              (cannots->cannot-picts size pre-cannot-be))))
      (define cell-p
        (vc-append
         (cond
           [must-be
            =>
            (λ (n) (text (~a n)))]
           [else (ghost (text "0"))])
         (scale cannot-be-p 1/2)
         (if pre-cannot-be-p (scale pre-cannot-be-p 1/2) (blank))))
      (define margin .05)
      (define fit-to-cell
        (scale-to-fit cell-p
                      (* (- 1 margin margin) w)
                      (* (- 1 margin margin) h)))
      (pin-over p
                (+ x (/ w 2) (- (/ (pict-width fit-to-cell) 2)))
                (+ y (/ h 2) (- (/ (pict-height fit-to-cell) 2)))
                fit-to-cell))))

(define (cannots->cannot-picts size cannots)
  (for/list ([i (in-inclusive-range 1 size)])
    (cellophane
     (colorize
      (text (~a i))
      (if (set-member? cannots i)
          "red"
          "forestgreen"))
     .4)))

(define (cannot-picts->pict l)
  (let loop ([l l])
    (cond
      [(< (length l) 5) (apply hbl-append l)]
      [else (vc-append (apply hbl-append (take l 5))
                       (loop (drop l 5)))])))

(define (sudoku-gui size step)
  (define ht (step))
  (define the-pict (ht->pict size ht))
  (define steps 0)

  (define (draw c dc)
    (define-values (cw ch) (send c get-client-size))
    (define sp (scale-to-fit the-pict cw ch))
    (draw-pict sp
               dc
               (- (/ cw 2) (/ (pict-width sp) 2))
               (- (/ ch 2) (/ (pict-height sp) 2))))

  (define (solved)
    (for/sum ([(k v) (in-hash ht)])
      (if (car v) 1 0)))
  (define (calc-msg)
    (~a
     (solved)
     " cell"
     (if (= (solved) 1) "" "s")
     " solved in "
     steps
     " step"
     (if (= steps 1) "" "s")))

  (define (button-callback _1 _2)
    (set! steps (+ steps 1))
    (define next-ht (step))
    (define nothing-changed? (equal? next-ht ht))
    (set! ht next-ht)
    (define msg (calc-msg))
    (when nothing-changed?
      (set! msg (string-append msg " (but nothing changed)")))
    (send m set-label msg)
    (set! ht next-ht)
    (set! the-pict (ht->pict size ht))
    (when (= (solved) (* size size))
      (send b enable #f))
    (send c refresh))

  (define f (new frame% [label ""] [width 400] [height 400]))
  (define c (new canvas% [parent f] [paint-callback draw]))
  (define hp (new horizontal-panel% [parent f] [stretchable-height #f]))
  (define b (new button% [label "step"] [callback button-callback] [parent hp]))
  (define m (new message% [parent hp] [label (calc-msg)] [stretchable-width #t]))
  (send f show #t))

(module+ main
  (define size 9)
  (sudoku-gui size
              (λ ()
                (for*/hash ([i (in-range size)]
                            [j (in-range size)])
                  (values (cons i j)
                          (cons (and (zero? (random 10))
                                     (random size))
                                (set 1 2 3)))))))
