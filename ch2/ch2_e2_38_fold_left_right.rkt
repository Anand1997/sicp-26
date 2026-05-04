#lang sicp
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; debg
(define dryrun-example (list 'x0 'x1 'x2))
(define (op a b) (list 'op a b))

;;Exerchise 2.39
(define (reverse sequence)
  (fold-right (lambda (x y)
                (append y (list x)))
              nil sequence))

(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;; test
(display (reverse dryrun-example))
(newline)
(display (reverse2 dryrun-example))
