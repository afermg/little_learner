#!/usr/bin/racket
#lang racket
(define pie 3.14)

(define area-of-circle
  (lambda (r)
    (* pie
       (* r r))))

(define double-result-of-f
  (lambda (f)
    (lambda (z)
      (* 2 (f z)))))

(define abs
  (lambda (x)
    (cond
      ((< x 0) (- 0 x))
      (else x))))

(define silly-abs
  (lambda (x)
    (let ((x-is-negative (< x 0)))
      (cond
        (x-is-negative (- 0 x))
        (else x)))))


(define remainder
  (lambda (x n)
    (cond
      ((< x n) x)
      (else (remainder (- x n) n)))))

;; Sum without '+', using recursion instead
(define add
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (add n (sub1 m)))))))

