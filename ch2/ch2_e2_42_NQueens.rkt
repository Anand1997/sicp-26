#lang sicp
(#%require "ch2_2_conventional_interfaces.rkt") ; Import your custom module

;;; ── Board representation ──────────────────────────────────────────────
;;;
;;; A board is a list of (row col) pairs, one per queen placed so far.
;;; The most-recently-placed queen (in column k) is at the HEAD of the list.

(define empty-board '())

;;; Adjoin a new queen at (new-row, col) to an existing set of positions.
(define (adjoin-position new-row col rest-of-queens)
  (cons (list new-row col) rest-of-queens))

;;; ── Selectors for a position ──────────────────────────────────────────

(define (position-row pos) (car pos))
(define (position-col pos) (cadr pos))

;;; ── Safety check ──────────────────────────────────────────────────────
;;;
;;; Two queens attack each other if they share a row OR a diagonal.
;;; (Column conflicts are impossible by construction — one queen per column.)

(define (attacks? q1 q2)
  (or (= (position-row q1) (position-row q2)) ; same row
      (= (abs (- (position-row q1) (position-row q2))) ; same diagonal
         (abs (- (position-col q1) (position-col q2))))))

;;; safe? checks whether the queen in column k is safe against all others.
;;; Note: the other queens are already guaranteed safe with each other.
(define (safe? k positions)
  (let ((new-queen (car positions)) ; most recently added
        (other-queens (cdr positions))) ; previously placed queens
    (define (safe-against-all? others)
      (cond ((null? others) #t)
            ((attacks? new-queen (car others)) #f)
            (else (safe-against-all? (cdr others)))))
    (safe-against-all? other-queens)))

;;; ── Main procedure ────────────────────────────────────────────────────

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 4)
