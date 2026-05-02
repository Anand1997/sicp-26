#lang sicp
(#%require "ch2_2_conventional_interfaces.rkt") ; Import your custom module

;; Exercise 2.37 : Matrix multiplication
(define (dot-product v w)
  (accum + 0 (mmap * v w)))

(define (transpose mat)
  (accum-n cons nil mat))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (matrix-*-matrix m n)
  (map (lambda (row)
         (map (lambda (col)
                (dot-product row col))
              (transpose n)))
       m))
