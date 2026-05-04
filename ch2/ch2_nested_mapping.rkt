#lang sicp
;; Nested mapping => How to loop using map
(#%require "ch2_2_conventional_interfaces.rkt") ; Import your custom module

(define (enum-interval start end)
  (if (> start end)
      '()
      (cons start
            (enum-interval (+ start 1) end))))

;; nested loop
#|
(accum append nil
       (map (lambda (i) ;; proc1
              (map (lambda (j) (list i j)) ;; proc2
                   (enum-interval 1 (- i 1))))
            (enum-interval 1 n)))

i = 1 → ((1 1))
i = 2 → ((2 1) (2 2))
i = 3 → ((3 1) (3 2) (3 3))
|#

;; NOTE: the result of (proc x) should work with append
(define (flatmap proc seq)
  (accum append nil (map proc seq)))

;; example 1
(display
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (enum-interval 1 i)))
    (enum-interval 1 3))
  )

(newline)
;; example 2
(display
  (flatmap
    (lambda (x) (list x (+ x 1)))
    (enum-interval 1 3))
  )

;; subset generation
(define (subsets1 set)
  (if (null? set)
      (list nil)
      (let ((rest (subsets1 (cdr set))))
        (append rest
                (map (lambda (x)
                       (cons (car set) x))
                     rest)))))

(define (subsets s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (subset)
                 (list subset
                       (cons (car s) subset)))
               (subsets (cdr s)))))
