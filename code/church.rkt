#lang racket

(define zero (λ (s) (λ (z) z)))
(define succ (λ (n) (λ (s) (λ (z) (s ((n s) z))))))

(define (plus n m)
  (λ (s) (λ (z) ((m s) ((n s) z)))))

(define five (λ (s) (λ (z) (s (s (s (s (s z))))))))

(define (mult n m)
  (λ (s) (λ (z) ((m (n s)) z))))

(define (expt n m)
  (m n))

(define (zero? n)
  (λ (t) (λ (f) ((n (λ (_) f)) t))))

(define make-pair
  (λ (x) (λ (y) (λ (p) ((p x) y)))))

(define left
  (λ (p) (p (λ (x) (λ (y) x)))))
(define right
  (λ (p) (p (λ (x) (λ (y) y)))))