#lang sicp
(#%require "ch2_2_conventional_interfaces.rkt") ; Import your custom module

(define empty-board '())
;; take first and iterate over rest
(define (attack? q1 q2)
  (let ((row-q1 (car q1))
        (col-q1 (cadr q1))
        (row-q2 (car q2))
        (col-q2 (cadr q2)))
    (or (= row-q1 row-q2)
        (= (abs (- row-q1 row-q2))
           (abs (- col-q1 col-q2))))))

(define (safe? position)
  (define new-queen (car position))
  (define (iter rest)
    (cond ((null? rest) #t)
          ((attack? new-queen (car rest)) #f)
          (else (iter (cdr rest)))))
  (iter (cdr position)))

(define (adjoin-position row k rest-of-queens)
  (cons (list row k) rest-of-queens))

(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (position) (safe? position))
          (flatmap (lambda (rest-of-queens)
                     (map (lambda (new-row)
                            (adjoin-position new-row k rest-of-queens))
                          (enumerate-interval 1 board-size)))
                   (queens-cols (- k 1))))))
  (queens-cols board-size))

(queens 4)


(define (queens-slow board-size)
  (define (queens-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (position) (safe? position))
          (flatmap (lambda (new-row)
                     (map (lambda (rest-of-queens)
                            (adjoin-position new-row k rest-of-queens))
                          (queens-cols (- k 1))))
                   (enumerate-interval 1 board-size)))))
  (queens-cols board-size))
(queens-slow 4)
