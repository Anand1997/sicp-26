#lang sicp
; Scratch file


;; Conventional Interface

;; map
(define (map proc seqs)
  (if (null? seqs)
      nil
      (cons (proc (car seqs))
            (map proc (cdr seqs)))))

;; reduce \ accumelate
(define (accum op init seqs)
  (if (null? seqs)
      init
      (op (car seqs)
          (accum op init (cdr seqs)))))

;; filter
(define (filter predicate seqs)
  (cond ((null? seqs) nil)
        ((predicate (car seqs))
         (cons (car seqs)
               (filter predicate (cdr seqs))))
        (else (filter predicate (cdr seqs)))))

(define (map-2 p seqs)
  ; x => current problem input
  ; y => smaller problem output
  (accum (lambda (x y) (cons (p x) y)) nil seqs))

(define (append seq1 seq2)
  (accum cons seq2 seq1))

(define (length-2 seqs)
  (accum (lambda (_ ans) (+ 1 ans)) 0 seqs))


;; Test
(define l1 '(1 2 3))
(define l2 '(5 7 9))
(define (sq x) (* x x))

(map-2 sq l1)
(append l1 l2)
(length-2 l1)
(length-2 l2)

; Extended Interfaces
(define (accum-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accum op init (map car seqs))
            (accum-n op init (map cdr seqs)))))

(define (mmap op . lists)
  (if (null? (car lists))
      nil
      (cons (apply op (map car lists))
            (apply mmap (cons op (map cdr lists))))))
(mmap + '(1 2 3) '(2 2 2))

;; Exercise 2.37 : Matrix multiplication
