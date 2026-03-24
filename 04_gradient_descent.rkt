#!/usr/bin/racket
#lang racket
(require malt)

(define revs 1000)
(define alpha 0.01)

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


(define revise
  (lambda (f revs theta)
    (cond
      ((zero? revs) theta)
      (else
       (revise f (sub1 revs) (f theta)))))
  )  ;; Evaluate the cases

;; Implement gradient descent
(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (big-theta)
               (map (lambda (p g)
                      (- p (* alpha g)))
                    big-theta
                    (gradient-of obj big-theta)))))
      (revise f revs theta))))

(let*
    (
     [xs (tensor 2.0 1.0 4.0 3.0)]
     [ys (tensor 1.8 1.2 4.2 3.3)]
     ;; [theta (list 0.0 0.0)]
     [theta (list 0.0 0.0)]
     )

  ;; Gradient list
  (gradient-of (lambda (theta) (sqr (ref theta 0))) (list 27.0))
  ;; '(54.0)

  ;; Gradient
  (gradient-of ((l2-loss line) xs ys) theta)

  ;; Revision by calling the fn multiple times
  (revise line revs theta)

  ;; Run gradient descent
  (gradient-descent
   ((l2-loss line) xs ys) theta)
  )
