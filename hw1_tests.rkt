#lang racket
(define (reply s)
  (if (and (string? s)
           (>= (string-length s) 5)
           (equal? "hello" (substring s 0 5)))
      "hi!"
      "huh?"))

(define (reply-more s)
  (cond
   [(equal? "?" (substring s (- (string-length s) 1)))
    "I don't know"]
   [(equal? "hello" (substring s 0 5))
    "hi!"]
   [(equal? "goodbye" (substring s 0 7))
    "bye!"]
   [else "huh?"]))

(define (factorial n)
  (when (< n 0) (error "Factorial can’t handle negative inputs"))
  (if (= n 0) ;; test
      1 ;; if
      (* n (factorial (- n 1))))) ;; else

(define (gcd a b)
  (let ((r (remainder a b)))
    (if (= r 0) ;; test
        b ;; if
        (gcd b r)))) ;; else

(define (rem x y)
  (when (<= y 0) (error "The divisor should be positive."))
  (if (< x y)
      x
      (rem (- x y) y)))
