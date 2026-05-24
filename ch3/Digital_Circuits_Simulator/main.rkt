#lang sicp

;; Import the required functions from our circuit module
(#%require "circuit.rkt")

;; Define our test circuit (Half-Adder)
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;; Create Wires
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

;; Attach Probes
(probe 'sum sum)
(probe 'carry carry)

;; Wire the circuit together
(half-adder input-1 input-2 sum carry)

;; Start the simulation
(display "--- Setting Input-1 to 1 ---")
(newline)
(set-signal! input-1 1)
(propagate)

(display "--- Setting Input-2 to 1 ---")
(newline)
(set-signal! input-2 1)
(propagate)
