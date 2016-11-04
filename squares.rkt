#lang slideshow
(define c (circle 10))
(define r (rectangle 10 20))
(define (square n)
  (filled-rectangle n n))
(define s (square 10))

(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))

(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

(define (checkerboard p) ;p is a square/circle/...
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))
(define cb (checkerboard s))

(define (series mk) ;mk is a function with para number
  (hc-append 4 (mk 5) (mk 10) (mk 20)))

(define (rgb-series mk)
  (vc-append
   (series (lambda (size) (colorize (mk size) "red")))
   (series (lambda (size) (colorize (mk size) "green")))
   (series (lambda (size) (colorize (mk size) "blue")))))

(define (rgb-maker mk) ;(series (rgb-maker circle))
  (lambda (sz) ;rgb-maker takes a function and returns a new one 
    (vc-append 4 (colorize (mk sz) "red")
                 (colorize (mk sz) "green")
                 (colorize (mk sz) "blue"))))

(define (hn p n)
  (if (= n 1)
      p
      (hc-append 5 p (hn p (- n 1)))))
(define (vn p n)
  (if (= n 1)
      p
      (vc-append 5 p (vn p (- n 1)))))

(define (grid p n)
  (let ([line (hn p n)])
    (vn line n)))