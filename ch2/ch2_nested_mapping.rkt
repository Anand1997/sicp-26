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
;; i.e it should return list to follow closure property
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
      (list nil) ;;  empty set
      (flatmap (lambda (subset)
                 (list subset
                       (cons (car s) subset)))
               (subsets (cdr s))))) ;; subset of smaller problem

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enum-interval 1 (- i 1))))
           (enum-interval 1 n)))

#| Exercise 2.41
  Write a procedure to find all ordered triples of distinct positive integers i, j, and k
  less than or equal to a given integer n that sum to a given integer s.
|#

#|
(define (triplate-sum-less-than-s n s)
  (define (triplate-sum triplate)
    (+ (car triplate)
       (cadr triplate)
       (caddr triplate)))
  (define (create-triplates n)
    (flatmap (lambda (i)
               (flatmap (lambda (j)
                          (map (lambda (k)
                                (list i j k))
                               (enum-interval (+ 1 j) n)))
                        (enum-interval (+ 1 i) n)))
             (enum-interval 1 n)))
  (filter (lambda (x) (< x s)) (map triplate-sum (create-triplates n))))
|#
(define (triple-sum-equal-s n s)
  (filter (lambda (triple)
            (= (+ (car triple)
                  (cadr triple)
                  (caddr triple))
               s))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k)
                                       (list i j k))
                                     (enum-interval (+ j 1) n)))
                              (enum-interval (+ i 1) n)))
                   (enum-interval 1 n))))
(newline)
(display
  (triple-sum-equal-s 6 10)
  )
