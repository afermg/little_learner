#!/usr/bin/racket
#lang racket
(require malt)

(declare-hyper revs)
(declare-hyper a)

;; From 04_gradient_descent.rkt
(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (big-theta)
               (map (lambda (p g)
                      (- p (* alpha g)))
                    big-theta
                    (gradient-of obj big-theta)))))
      (revise f revs theta))))

(define l2-loss
  (lambda (target)
    (lambda (xs ys)
      (lambda (theta)
        (let ((pred-ys ((target xs) theta)))
          (sum (sqr (- ys pred-ys))))))))

;; As per the previous exercise
(let*
    (
     [line-xs (tensor 2.0 1.0 4.0 3.0)]
     [line-ys (tensor 1.8 1.2 4.2 3.3)]
     )

  (with-hypers
    ((revs 1000)
     (alpha 0.01))
    (gradient-descent
     ((l2-loss line) line-xs line-ys)
     (list 0.0 0.0)))
  )

;; New dataset: quadratic functions
;; General definition of quadratic fn
(define quad
  (lambda (t)
    (lambda (theta)
      (+ (* (ref theta 0) (sqr t))
         (+ (* (ref theta 1) t) (ref theta 2))))))

;; Objective function := ((expectant function) quad-xs quad-ys)
((quad 3.0) (list 4.5 2.1 7.8))

(define quad-xs (tensor -1.0 0.0 1.0 2.0 3.0))
(define quad-ys (tensor 2.55 2.1 4.35 10.2 18.25))

(with-hypers
  ((revs 1000)
   (alpha 0.001))
  (gradient-descent
   ((l2-loss quad) quad-xs quad-ys)
   (list 0.0 0.0 0.0)))

;; Output
;; '(1.4787394427094362 0.9928606519360353 2.0546423148479684)
  
(define plane
  (lambda (t)
    (lambda (theta)
      (+ (dot-product (ref theta 0) t) (ref theta 1)))))

(define plane-xs
  (tensor (tensor 1.0 2.05)
          (tensor 1.0 3.0)
          (tensor 2.0 2.0)
          (tensor 2.0 3.91)
          (tensor 3.0 6.13)
          (tensor 4.0 8.09)))

(define plane-ys
  (tensor 13.99
          15.99
          18.0
          22.4
          30.2
          37.94))

(define solution
  (with-hypers
    ((revs 1000)
     (alpha 0.001))
    (gradient-descent
    ((l2-loss plane) plane-xs plane-ys)
    (list (tensor 0.0 0.0) 0.0))))

((plane (tensor 2.0 3.91)) solution)

