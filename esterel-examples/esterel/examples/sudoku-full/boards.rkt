#lang racket/base

(provide (all-defined-out))

#|
;; Conversion code; see the formats at
;; https://github.com/manuel-serrano/icfp2024-sudoku/blob/0b8d72333105/boards9x9.mjs
(require racket/pretty)
(define (convert b)
  (define strs (regexp-split (regexp-quote "\n") b))
  (pretty-print
   `(list
     ,@(for*/list ([(str i) (in-indexed strs)]
                   [j (in-range 9)]
                   [ch (in-value (string-ref str j))]
                   #:when (not (char=? #\. ch)))
         `(cons (cons ,i ,j) ,(- (char->integer ch) 48))))))
|#

(define paper-board-1
  ;; The first Sudoku board presented in Serrano and Findler's ICFP 2024 functional pearl
  (list (cons (cons 0 2) 5)
        (cons (cons 0 6) 4)
        
        (cons (cons 1 3) 1)
        (cons (cons 1 5) 3)
        
        (cons (cons 2 0) 4)
        (cons (cons 2 8) 9)
        
        (cons (cons 3 1) 6)
        (cons (cons 3 7) 2)
        
        (cons (cons 4 3) 9)
        (cons (cons 4 4) 2)
        (cons (cons 4 5) 5)
        
        (cons (cons 5 0) 3)
        (cons (cons 5 8) 1)
        
        (cons (cons 6 2) 2)
        (cons (cons 6 4) 5)
        (cons (cons 6 6) 9)
        
        (cons (cons 7 3) 3)
        (cons (cons 7 5) 4)
        
        (cons (cons 8 1) 7)
        (cons (cons 8 4) 6)
        (cons (cons 8 7) 3)))


(define paper-board-2
  ;; The second Sudoku board presented in Serrano and Findler's ICFP 2024 functional pearl
  (list (cons (cons 0 3) 9)
        (cons (cons 0 4) 5)
        (cons (cons 0 5) 2)
        
        (cons (cons 2 1) 6)
        (cons (cons 2 2) 1)
        (cons (cons 2 4) 4)
        (cons (cons 2 6) 5)
        (cons (cons 2 7) 2)
        
        (cons (cons 3 3) 1)
        (cons (cons 3 4) 3)
        (cons (cons 3 5) 6)
        
        (cons (cons 4 0) 1)
        (cons (cons 4 4) 2)
        (cons (cons 4 8) 7)
        
        (cons (cons 5 1) 8)
        (cons (cons 5 7) 6)
        
        (cons (cons 6 0) 2)
        (cons (cons 6 2) 3)
        (cons (cons 6 3) 8)
        (cons (cons 6 5) 7)
        (cons (cons 6 6) 4)
        (cons (cons 6 8) 1)
        
        (cons (cons 7 0) 5)
        (cons (cons 7 8) 3)
        
        (cons (cons 8 4) 9)))

(define web-board-1
  ;; wildcatjan17 from https://sandiway.arizona.edu/sudoku/examples.html
  (list (cons (cons 0 3) 2)
        (cons (cons 0 4) 6)
        (cons (cons 0 6) 7)
        (cons (cons 0 8) 1)

        (cons (cons 1 0) 6)
        (cons (cons 1 1) 8)
        (cons (cons 1 4) 7)
        (cons (cons 1 7) 9)

        (cons (cons 2 0) 1)
        (cons (cons 2 1) 9)
        (cons (cons 2 5) 4)
        (cons (cons 2 6) 5)

        (cons (cons 3 0) 8)
        (cons (cons 3 1) 2)
        (cons (cons 3 3) 1)
        (cons (cons 3 7) 4)

        (cons (cons 4 2) 4)
        (cons (cons 4 3) 6)
        (cons (cons 4 5) 2)
        (cons (cons 4 6) 9)

        (cons (cons 5 1) 5)
        (cons (cons 5 5) 3)
        (cons (cons 5 7) 2)
        (cons (cons 5 8) 8)

        (cons (cons 6 2) 9)
        (cons (cons 6 3) 3)
        (cons (cons 6 7) 7)
        (cons (cons 6 8) 4)

        (cons (cons 7 1) 4)
        (cons (cons 7 4) 5)
        (cons (cons 7 7) 3)
        (cons (cons 7 8) 6)

        (cons (cons 8 0) 7)
        (cons (cons 8 2) 3)
        (cons (cons 8 4) 1)
        (cons (cons 8 5) 8)))

(define web-board-1-dense
  (list (cons (cons 0 1) 3)
        (cons (cons 0 2) 5)
        (cons (cons 0 3) 2)
        (cons (cons 0 4) 6)
        (cons (cons 0 5) 9)
        (cons (cons 0 6) 7)
        (cons (cons 0 7) 8)

        (cons (cons 1 1) 8)
        (cons (cons 1 2) 2)
        (cons (cons 1 3) 5)
        (cons (cons 1 4) 7)
        (cons (cons 1 5) 1)
        (cons (cons 1 6) 4)
        (cons (cons 1 7) 9)

        (cons (cons 2 1) 9)
        (cons (cons 2 2) 7)
        (cons (cons 2 3) 8)
        (cons (cons 2 4) 3)
        (cons (cons 2 5) 4)
        (cons (cons 2 6) 5)
        (cons (cons 2 7) 6)

        (cons (cons 3 1) 2)
        (cons (cons 3 2) 6)
        (cons (cons 3 3) 1)
        (cons (cons 3 4) 9)
        (cons (cons 3 5) 5)
        (cons (cons 3 6) 3)
        (cons (cons 3 7) 4)

        (cons (cons 4 1) 7)
        (cons (cons 4 2) 4)
        (cons (cons 4 3) 6)
        (cons (cons 4 4) 8)
        (cons (cons 4 5) 2)
        (cons (cons 4 6) 9)
        (cons (cons 4 7) 1)

        (cons (cons 5 1) 5)
        (cons (cons 5 2) 1)
        (cons (cons 5 3) 7)
        (cons (cons 5 4) 4)
        (cons (cons 5 5) 3)
        (cons (cons 5 6) 6)
        (cons (cons 5 7) 2)

        (cons (cons 6 1) 1)
        (cons (cons 6 2) 9)
        (cons (cons 6 3) 3)
        (cons (cons 6 4) 2)
        (cons (cons 6 5) 6)
        (cons (cons 6 6) 8)
        (cons (cons 6 7) 7)

        (cons (cons 7 1) 4)
        (cons (cons 7 2) 8)
        (cons (cons 7 3) 9)
        (cons (cons 7 4) 5)
        (cons (cons 7 5) 7)
        (cons (cons 7 6) 1)
        (cons (cons 7 7) 3)

        (cons (cons 8 1) 6)
        (cons (cons 8 2) 3)
        (cons (cons 8 3) 4)
        (cons (cons 8 4) 1)
        (cons (cons 8 5) 8)
        (cons (cons 8 6) 2)
        (cons (cons 8 7) 5)
        ))

(define web-board-2
  ;; board:
  #|
  https://www.sudokuwiki.org/sudoku.htm?bd=40000003800200410000530024007060900
  4020000070600703090057008300003900400240000009
  |#
  ;; taken from https://www.sudokuwiki.org/Naked_Candidates
  (list
   (cons (cons 0 0) 4)
   (cons (cons 0 7) 3)
   (cons (cons 0 8) 8)

   (cons (cons 1 2) 2)
   (cons (cons 1 5) 4)
   (cons (cons 1 6) 1)

   (cons (cons 2 2) 5)
   (cons (cons 2 3) 3)
   (cons (cons 2 6) 2)
   (cons (cons 2 7) 4)

   (cons (cons 3 1) 7)
   (cons (cons 3 3) 6)
   (cons (cons 3 5) 9)
   (cons (cons 3 8) 4)

   (cons (cons 4 1) 2)
   (cons (cons 4 7) 7)

   (cons (cons 5 0) 6)
   (cons (cons 5 3) 7)
   (cons (cons 5 5) 3)
   (cons (cons 5 7) 9)

   (cons (cons 6 1) 5)
   (cons (cons 6 2) 7)
   (cons (cons 6 5) 8)
   (cons (cons 6 6) 3)

   (cons (cons 7 2) 3)
   (cons (cons 7 3) 9)
   (cons (cons 7 6) 4)

   (cons (cons 8 0) 2)
   (cons (cons 8 1) 4)
   (cons (cons 8 8) 9)))

(define web-board-2-dense
  ;; board:
  #|
  https://www.sudokuwiki.org/sudoku.htm?bd=40000093803209410009530024037060900
  4529001673604703090957008300003900400240030709
  |#
  ;; taken from https://www.sudokuwiki.org/Naked_Candidates
  (list
   (cons (cons 0 0) 4)
   (cons (cons 0 6) 9)
   (cons (cons 0 7) 3)
   (cons (cons 0 8) 8)

   (cons (cons 1 1) 3)
   (cons (cons 1 2) 2)
   (cons (cons 1 4) 9)
   (cons (cons 1 5) 4)
   (cons (cons 1 6) 1)

   (cons (cons 2 1) 9)
   (cons (cons 2 2) 5)
   (cons (cons 2 3) 3)
   (cons (cons 2 6) 2)
   (cons (cons 2 7) 4)

   (cons (cons 3 0) 3)
   (cons (cons 3 1) 7)
   (cons (cons 3 3) 6)
   (cons (cons 3 5) 9)
   (cons (cons 3 8) 4)

   (cons (cons 4 0) 5)
   (cons (cons 4 1) 2)
   (cons (cons 4 2) 9)
   (cons (cons 4 5) 1)
   (cons (cons 4 6) 6)
   (cons (cons 4 7) 7)
   (cons (cons 4 8) 3)

   (cons (cons 5 0) 6)
   (cons (cons 5 2) 4)
   (cons (cons 5 3) 7)
   (cons (cons 5 5) 3)
   (cons (cons 5 7) 9)

   (cons (cons 6 0) 9)
   (cons (cons 6 1) 5)
   (cons (cons 6 2) 7)
   (cons (cons 6 5) 8)
   (cons (cons 6 6) 3)

   (cons (cons 7 2) 3)
   (cons (cons 7 3) 9)
   (cons (cons 7 6) 4)

   (cons (cons 8 0) 2)
   (cons (cons 8 1) 4)
   (cons (cons 8 4) 3)
   (cons (cons 8 6) 7)
   (cons (cons 8 8) 9)))

(define esterel-package-simplest
  ;; Taken from rfindler/esterel, which is in turn from grantm/sudoku-exchange-puzzle-bank
  (list
   (cons (cons 0 1) 5)
   (cons (cons 0 3) 7)
   (cons (cons 0 5) 3)
   (cons (cons 0 7) 6)

   (cons (cons 1 2) 7)
   (cons (cons 1 6) 8)

   (cons (cons 2 3) 8)
   (cons (cons 2 4) 1)
   (cons (cons 2 5) 6)

   (cons (cons 3 4) 3)

   (cons (cons 4 2) 5)
   (cons (cons 4 6) 1)

   (cons (cons 5 0) 7)
   (cons (cons 5 1) 3)
   (cons (cons 5 4) 4)
   (cons (cons 5 7) 8)
   (cons (cons 5 8) 6)

   (cons (cons 6 0) 9)
   (cons (cons 6 2) 6)
   (cons (cons 6 6) 2)
   (cons (cons 6 8) 4)

   (cons (cons 7 0) 8)
   (cons (cons 7 1) 4)
   (cons (cons 7 3) 5)
   (cons (cons 7 4) 7)
   (cons (cons 7 5) 2)
   (cons (cons 7 7) 9)
   (cons (cons 7 8) 3)

   (cons (cons 8 3) 4)
   (cons (cons 8 5) 9)
   ))

(define esterel-package-medium
  ;; Taken from GH/rfindler/esterel, which is in turn from grantm/sudoku-exchange-puzzle-bank
  (list
   (cons (cons 0 0) 1)
   (cons (cons 0 7) 2)

   (cons (cons 1 0) 2)
   (cons (cons 1 3) 7)
   (cons (cons 1 4) 3)
   (cons (cons 1 7) 4)

   (cons (cons 2 0) 8)
   (cons (cons 2 3) 1)
   (cons (cons 2 6) 6)

   (cons (cons 3 2) 1)
   (cons (cons 3 3) 3)

   (cons (cons 5 5) 5)
   (cons (cons 5 6) 8)

   (cons (cons 6 2) 6)
   (cons (cons 6 5) 4)
   (cons (cons 6 8) 5)

   (cons (cons 7 1) 1)
   (cons (cons 7 4) 5)
   (cons (cons 7 5) 2)
   (cons (cons 7 8) 9)

   (cons (cons 8 1) 9)
   (cons (cons 8 8) 7)
   ))

(define esterel-package-hard
  ;; Taken from GH/rfindler/esterel, which is in turn from grantm/sudoku-exchange-puzzle-bank
  (list
   (cons (cons 1 0) 7)
   (cons (cons 1 4) 6)
   (cons (cons 1 8) 3)

   (cons (cons 2 2) 4)
   (cons (cons 2 3) 1)
   (cons (cons 2 5) 8)
   (cons (cons 2 6) 9)

   (cons (cons 3 3) 4)
   (cons (cons 3 5) 1)

   (cons (cons 4 1) 6)
   (cons (cons 4 4) 2)
   (cons (cons 4 7) 9)

   (cons (cons 6 2) 1)
   (cons (cons 6 3) 5)
   (cons (cons 6 5) 4)
   (cons (cons 6 6) 8)

   (cons (cons 7 0) 9)
   (cons (cons 7 1) 7)
   (cons (cons 7 7) 2)
   (cons (cons 7 8) 6)
   ))

(define serrano-findler-icfp24-easy
  ;; Taken from GH/manuel-serrano/icfp2024-sudoku, which is in turn from https://sudoku.com
  (list
   (cons (cons 0 0) 3)
   (cons (cons 0 1) 4)
   (cons (cons 0 3) 9)
   (cons (cons 0 4) 7)

   (cons (cons 1 0) 1)
   (cons (cons 1 2) 9)
   (cons (cons 1 7) 4)

   (cons (cons 2 5) 1)
   (cons (cons 2 8) 5)

   (cons (cons 3 1) 7)
   (cons (cons 3 6) 6)
   (cons (cons 3 7) 5)

   (cons (cons 4 0) 9)
   (cons (cons 4 2) 6)
   (cons (cons 4 3) 5)
   (cons (cons 4 4) 8)
   (cons (cons 4 5) 7)
   (cons (cons 4 6) 4)
   (cons (cons 4 7) 2)

   (cons (cons 5 0) 2)
   (cons (cons 5 1) 5)
   (cons (cons 5 2) 3)
   (cons (cons 5 4) 9)
   (cons (cons 5 5) 4)
   (cons (cons 5 6) 1)
   (cons (cons 5 7) 8)

   (cons (cons 6 2) 2)
   (cons (cons 6 3) 1)
   (cons (cons 6 4) 5)
   (cons (cons 6 8) 4)

   (cons (cons 7 4) 6)
   (cons (cons 7 5) 9)
   (cons (cons 7 7) 7)
   (cons (cons 7 8) 2)

   (cons (cons 8 1) 3)
   (cons (cons 8 7) 9)))

(define serrano-findler-icfp24-medium
  ;; Taken from GH/manuel-serrano/icfp2024-sudoku, which is in turn from https://sudoku.com
  (list
   (cons (cons 0 5) 3)
   (cons (cons 0 7) 5)
   (cons (cons 0 8) 6)

   (cons (cons 1 1) 1)
   (cons (cons 1 5) 7)

   (cons (cons 2 3) 2)
   (cons (cons 2 4) 5)
   (cons (cons 2 7) 1)
   (cons (cons 2 8) 9)

   (cons (cons 3 0) 1)
   (cons (cons 3 1) 3)
   (cons (cons 3 2) 2)
   (cons (cons 3 8) 8)

   (cons (cons 4 0) 5)
   (cons (cons 4 3) 1)
   (cons (cons 4 4) 6)
   (cons (cons 4 6) 4)

   (cons (cons 5 0) 7)
   (cons (cons 5 3) 5)
   (cons (cons 5 5) 8)
   (cons (cons 5 6) 9)
   (cons (cons 5 8) 1)

   (cons (cons 6 3) 3)

   (cons (cons 7 2) 9)
   (cons (cons 7 3) 7)
   (cons (cons 7 8) 5)

   (cons (cons 8 4) 9)
   (cons (cons 8 5) 1)
   (cons (cons 8 7) 4)
   (cons (cons 8 8) 7)))

(define serrano-findler-icfp24-hard
  ;; Taken from GH/manuel-serrano/icfp2024-sudoku, which is in turn from https://sudoku.com
  (list
   (cons (cons 0 1) 3)
   (cons (cons 0 3) 6)
   (cons (cons 0 6) 4)

   (cons (cons 1 7) 6)

   (cons (cons 2 5) 9)
   (cons (cons 2 8) 8)

   (cons (cons 3 2) 1)
   (cons (cons 3 4) 2)
   (cons (cons 3 5) 6)
   (cons (cons 3 7) 4)

   (cons (cons 4 0) 3)
   (cons (cons 4 4) 5)
   (cons (cons 4 6) 7)

   (cons (cons 5 0) 2)
   (cons (cons 5 2) 6)
   (cons (cons 5 5) 3)
   (cons (cons 5 8) 1)

   (cons (cons 6 1) 8)
   (cons (cons 6 3) 1)
   (cons (cons 6 4) 9)

   (cons (cons 7 2) 5)
   (cons (cons 7 3) 3)
   (cons (cons 7 4) 4)
   (cons (cons 7 8) 7)

   (cons (cons 8 0) 4)
   (cons (cons 8 1) 2)
   (cons (cons 8 2) 7)
   (cons (cons 8 6) 9)))

(define serrano-findler-icfp24-expert
  ;; Taken from GH/manuel-serrano/icfp2024-sudoku, which is in turn from https://sudoku.com
  (list
   (cons (cons 0 0) 4)
   (cons (cons 0 3) 6)

   (cons (cons 1 2) 2)
   (cons (cons 1 4) 3)

   (cons (cons 2 5) 9)
   (cons (cons 2 6) 8)
   (cons (cons 2 7) 2)
   (cons (cons 2 8) 7)

   (cons (cons 3 0) 8)
   (cons (cons 3 3) 4)
   (cons (cons 3 4) 1)

   (cons (cons 4 0) 9)
   (cons (cons 4 8) 5)

   (cons (cons 5 1) 6)
   (cons (cons 5 7) 7)

   (cons (cons 6 1) 3)
   (cons (cons 6 6) 4)
   (cons (cons 6 8) 6)

   (cons (cons 7 4) 9)
   (cons (cons 7 5) 6)
   (cons (cons 7 6) 2)

   (cons (cons 8 1) 9)
   (cons (cons 8 7) 5)))

(define serrano-findler-icfp24-master
  ;; Taken from GH/manuel-serrano/icfp2024-sudoku, which is in turn from https://sudoku.com
  (list
   (cons (cons 0 2) 4)
   (cons (cons 0 3) 9)
   (cons (cons 0 4) 3)
   (cons (cons 0 7) 6)
   (cons (cons 0 8) 7)

   (cons (cons 1 6) 9)

   (cons (cons 2 0) 8)
   (cons (cons 2 3) 1)

   (cons (cons 3 4) 9)
   (cons (cons 3 6) 4)

   (cons (cons 4 2) 5)
   (cons (cons 4 6) 2)

   (cons (cons 5 1) 1)
   (cons (cons 5 3) 3)
   (cons (cons 5 7) 5)
   (cons (cons 5 8) 9)

   (cons (cons 6 2) 6)
   (cons (cons 6 4) 7)
   (cons (cons 6 7) 3)
   (cons (cons 6 8) 5)

   (cons (cons 7 7) 2)

   (cons (cons 8 1) 4)
   (cons (cons 8 5) 6)
   ))

(define serrano-findler-icfp24-supermaster
  ;; Taken from GH/manuel-serrano/icfp2024-sudoku, which is in turn from https://sudoku.com
  (list
   (cons (cons 0 1) 5)
   (cons (cons 0 4) 7)
   (cons (cons 0 7) 8)
   (cons (cons 0 8) 3)

   (cons (cons 1 2) 4)
   (cons (cons 1 7) 6)

   (cons (cons 2 4) 5)

   (cons (cons 3 0) 8)
   (cons (cons 3 1) 3)
   (cons (cons 3 3) 6)

   (cons (cons 4 3) 9)
   (cons (cons 4 6) 1)

   (cons (cons 6 0) 5)
   (cons (cons 6 2) 7)
   (cons (cons 6 6) 4)

   (cons (cons 7 3) 3)
   (cons (cons 7 5) 2)

   (cons (cons 8 0) 1)
   ))

(define all-sudoku-boards/list
  (list (cons "web-board-1" web-board-1)
        (cons "web-board-2" web-board-2)
        (cons "web-board-1-dense" web-board-1-dense)
        (cons "web-board-2-dense" web-board-2-dense)
        (cons "esterel-package-simplest" esterel-package-simplest)
        (cons "esterel-package-medium" esterel-package-medium)
        (cons "esterel-package-hard" esterel-package-hard)
        (cons "paper-board-1" paper-board-1)
        (cons "paper-board-2" paper-board-2)
        (cons "serrano-findler-icfp24-easy" serrano-findler-icfp24-easy)
        (cons "serrano-findler-icfp24-medium" serrano-findler-icfp24-medium)
        (cons "serrano-findler-icfp24-hard" serrano-findler-icfp24-hard)
        (cons "serrano-findler-icfp24-expert" serrano-findler-icfp24-expert)
        (cons "serrano-findler-icfp24-master" serrano-findler-icfp24-master)
        (cons "serrano-findler-icfp24-supermaster" serrano-findler-icfp24-supermaster)))

(define all-sudoku-boards
  (for/hash ([name-board (in-list all-sudoku-boards/list)])
    (values (car name-board) (cdr name-board))))

(define all-boards (map cdr all-sudoku-boards/list))
