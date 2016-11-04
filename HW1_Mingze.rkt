#lang slideshow

(define (rem x y)
  (when (<= y 0) (error "The divisor should be positive."))
  (if (< x y)
      x
      (rem (- x y) y)))

;;helper function
(define (hn p n)
  (if (= n 1)
      p
      (hc-append 5 p (hn p (- n 1)))))

;;helper function
(define (vn p n) 
  (if (= n 1)
      p
      (vc-append 5 p (vn p (- n 1)))))

;;p is an Object and n is the repeat number
(define (grid p n) 
  (let ([line (hn p n)])
    (vn line n)))

;;p and q are Objects and n is the number
(define (alter p q n) 
  (if (= n 1)
      p
      (hc-append 5 p (alter q p (- n 1)))))

(define c (circle 10)) ;;easy for testing
(define r (rectangle 10 10)) ;; easy for testing