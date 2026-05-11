#lang sicp
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

;; TODO : Move this to new file
(define (map-2 p seqs)
  ; x => current problem input
  ; y => smaller problem output
  (accum (lambda (x y) (cons (p x) y)) nil seqs))

(define (append seq1 seq2)
  (accum cons seq2 seq1))

(define (length-2 seqs)
  (accum (lambda (_ ans) (+ 1 ans)) 0 seqs))


;; Extended Interval

(define (enumerate-interval start end)
  (if (> start end)
      '()
      (cons start
            (enumerate-interval (+ start 1) end))))

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

(define (flatmap proc sequence)
  (accum append nil (map proc sequence)))

(#%provide map) ; Explicitly export the function
(#%provide accum) ; Explicitly export the function
(#%provide filter) ; Explicitly export the function
(#%provide enumerate-interval) ; Explicitly export the function
(#%provide accum-n) ; Explicitly export the function
(#%provide mmap) ; Explicitly export the function
(#%provide flatmap) ; Explicitly export the function
