#lang racket
(define (my-map2 f lst)
  (define (iter lst backward-result)
    (cond
     [(empty? lst) (reverse backward-result)]
     [else (iter (rest lst)
                 (cons (f (first lst))
                       backward-result))]))
  (iter lst empty))

(define (my-map1 f lst)
  (cond
   [(empty? lst) empty]
   [else (cons (f (first lst))
               (my-map1 f (rest lst)))]))

(define (my-map3 f lst)
  (for/list ([i lst])
    (f i)))

(define (onlyAlpha l)
  (cond
   [(empty? l) empty]
   [else
    (if (char-alphabetic? (first l))
        (cons (first l) (onlyAlpha (rest l)))
        (onlyAlpha (rest l)))]))

(define (upper s)
  (let ([t (string->list s)])
    (let ([c (onlyAlpha t)])
      (list->string (map (lambda (e) (char-upcase e))
                         c)))))
;A - Z => 0 - 25
(define (toInt c)
  (let ([n (char->integer c)])
    (- n 65)))

(define (ROT12 str)
  (let ([chars (upper str)])
    (list->string
     (for/list ([i chars])
      (integer->char (+ (remainder (+ (toInt i) 12) 26) 65))))))

(define (powH a n lst)
  (if (= n 1)
      lst
      (powH a (- n 1) (cons (* a (first lst)) lst))))

(define (pow_back a n)
  (powH a n (list a)))

(define (powH_append a n prev lst)
  (if (= n 0)
      lst
      (powH_append a (- n 1) (* a prev) (append lst (list (* a prev))))))

(define (pow_num a n)
  (powH_append a n 1 (list 1)))

(define (pow2H n lst)
  (if (= n 1)
      lst
      (pow2H (- n 1) (cons (expt (first lst) 2) lst))))

(define (power2 a n)
  (reverse (pow2H n (list (expt a 2)))))

(define (power2k a n k)
  (map (lambda (a2) (remainder a2 k))
       (power2 a n)))