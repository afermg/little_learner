#lang racket
(require malt)

(define rank
  (lambda (t)
    (cond
      ((scalar? t) 0)
      (else (add1 (rank (tref t 0)))))))

(define shape
  (lambda (t)
    (cond
      ((scalar? t) (list))
      (else (cons (tlen t) (shape (tref t 0)))))))

(define tensor-from-shape
  (lambda (shape)
        (cond
          ((< (length shape) 1)
           (apply tensor (for/list ([i (in-range (car shape))]) i)))
          (else
           (apply tensor (for/list ([_ (in-range (car shape))]) (tensor-from-shape (cdr shape)))
                  )))
          ))
