#lang sicp

;; 2.2.1 Representing Sequences

;; List operations 
(define (list-ref iteams n) 
    (if (= n 0)
        (car iteams)
        (list-ref (cdr iteams) (- n 1))))

(define (length iteams)
    (if (null? iteams)
        0
        (+  1 (length (cdr iteams)))))

(define (length2 iteams)
  (define (iter cnt rest)
    (if (null? rest)
        cnt
        (iter (+ 1 cnt) (cdr rest))))
  (iter 0 iteams))

(define (append l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (append (cdr l1) l2))))

;; Exercise 2.17
(define (last-pair iteams)
  (define (iter rest prev)
    (if (null? (cdr rest))
        (cons prev (car rest))
        (iter (cdr rest) (car rest))))
  (iter iteams '()))

;; Exercise 2.18
(define (reverse iteams)
  (define (iter rest-iteams rev-iteams)
    (if (null? rest-iteams)
        rev-iteams
        (iter (cdr rest-iteams) (cons (car rest-iteams) rev-iteams))))
  (iter iteams '()))

;; Exercise 2.19 #TODO

;; Exercise 2.20
(define (filter predicate sequence)
  (cond ((null? sequence) '())   ; base case
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else
         (filter predicate (cdr sequence)))))

(define (same-parit p . rest)
  (define predicate (if (even? p) even? odd?))
  (filter predicate (cons p rest)))


;; Map over list
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;; Exercise 2.21
(define (square x) (* x x))
(define (square-list items)
  (map (square) items))

;; Exercise 2.22
(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (square-list-rev-3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons answer 
                    (square (car things))))))
  (iter items nil))

;; Exercise 2.23
(define (for-each proc items)
  (cond ((null? items) #t)
        (else
         (proc (car items))
         (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88 ))


;; 2.2.2  Hierarchical Structures

(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; Exercise 2.24 => box-and-point diagram practice

;; Exercise 2.25 => get 7
; (1 3 (5 7) 9) )
(define l1 (list 1 3 (list 5 7) 9 ))
(define l2 '(1 3 (5 7) 9))
(car (cdaddr l1))
;; (car (cdaddr l2))

;;((7))
(define l3 '((7)))
(caar l3)

;; (1 (2 (3 (4 (5 (6 7))))))
(define l4 '(1 (2 (3 (4 (5 (6 7) ))))))
(define l5 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(cadadr (cadadr (cadadr l4)))
(cadadr (cadadr (cadadr l5)))


;; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

;; Exercise 2.27 Deep Reverse #TODO
(define x2 (list (list 1 2) (list 3 4)))

(define (deep-reverse x)
  (cond ((null? x) '())
        ((not (pair? x)) x)
        (else (append (deep-reverse (cdr x))
                      (list (deep-reverse (car x)))))))

(reverse x2)
(deep-reverse x2)

;; Exercise 2.28
(define x3 (list (list 1 2) (list (list 7 9) 3 4)))
(define (fringe node)
  (cond ((null? node) '())
        ((not (pair? node)) (list node))
        (else (append (fringe (car node))
                      (fringe (cdr node))))))
(fringe x3)

;; Exercise 2.29 #TODO

;; MAP Over tree
(define (scale-tree1 tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree1 (car tree) factor)
                    (scale-tree1 (cdr tree) factor)))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;; Exercise 2.30a
(define (sq-tree1 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (sq-tree1 (car tree))
                    (sq-tree1 (cdr tree))))))

;; Exercise 2.30b
(define (sq-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (sq-tree2 sub-tree)
             (* sub-tree sub-tree)))
       tree))

;; Exercise 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (append x (list (car s))))
                          rest)))))



(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

;; Exercise 2.33
(define (acc-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(acc-map (lambda (x) (* x x))
         '(1 2 3))

(define (acc-append seq1 seq2)
  (accumulate cons seq2 seq1))

(acc-append '(1 2 4) '(7 8 6))

(define (acc-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(acc-length '(1 2 3 6))


(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; 1 + 3x + 5x3 + x5 at x = 2 
(horner-eval 2 (list 1 3 0 5 0 1))

;; (define x 2)
;; (1 3 0 5 0 1)
;; (0 1 2 3 4 5) => (map (lambda (n) (pow x n)) '(0 1 2 3 4 5))
