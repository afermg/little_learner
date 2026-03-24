#!/usr/bin/racket
#lang racket
(require malt)

(define sum
  (lambda (t)
    (summed t (sub1 (tlen t)) 0.0))
  )

(define summed
  (lambda (t i a)
    (cond
      ((zero? i) (+ (tref t 0) a))
      (else (summed t (sub1 i) (+ (tref t i) a))))
    ))


(define line
  (lambda (x)
    (lambda (theta)
      (+ (* (ref theta 0) x) (ref theta 1)))))

(define l2-loss
  (lambda (target)
    (lambda (xs ys)
      (lambda (theta)
        (let
            ([pred-ys ((target xs) theta)])
            (sum
             (sqr
              (- ys pred-ys)))
          )))))

;; Evaluate the cases
(let
    ([xs (tensor 2.0 1.0 4.0 3.0)]
     [ys (tensor 1.8 1.2 4.2 3.3)]
     ;; [theta (list 0.0 0.0)]
     [theta (list 0.9 0.0)])

    (((l2-loss line) xs ys) theta))
