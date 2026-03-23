#lang racket
(require malt)

;; (define line
;;   (lambda (x)
;;     (lambda (w b)
;;       (let ((y (+ (* w x) b)))
;;         y))))

(define line
  (lambda (x)
    (lambda (theta)
      (+ (* (ref theta 0) x) (ref theta 1)))))

((line 7.4) (list 1.0 0.0))
