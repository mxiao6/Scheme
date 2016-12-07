#lang racket

;upper's helper function
;works like a filter
;@param l list of chars
;@return list of only Alphabetic and numeric chars, the others char(27)
(define (onlyAlpha l)
  (cond
   [(empty? l) empty]
   [else
    (if (or (char-alphabetic? (first l)) (char-numeric? (first l)))
        (cons (first l) (onlyAlpha (rest l))) ;put back as original
        (cons (integer->char 27) (onlyAlpha (rest l))))])) ;transfer the others to char(27)

;@param s string
;@return list of chars All letters are converted to uppercase.
(define (upper s)
  (let ([t (string->list s)])
    (let ([c (onlyAlpha t)]) ;filter to only alphabetic chars
      (map (lambda (e) (char-upcase e)) ;map to upper
                         c))))

;@param c char
;@return integer
;A - Z => 1 - 26
;0 - 9 => 48 - 57
;other => 27
(define (toInt c)
  (let ([n (char->integer c)])
    (if (char-alphabetic? c)
        (- n 64)
        n)))
;inverse of above
(define (toChar i)
  (cond
    [(< i 27) (integer->char (+ i 64))]
    [(= i 27) #\space]
    [else (integer->char i)]))

;Problem 1
;@param s string
;@return list of ints
(define (numerize s)
  (let ([chars (upper s)])
    (for/list ([i chars])
      (toInt i))))

;Problem 1
;@param l list of ints
;@return string
(define (inverse l)
  (list->string
   (for/list ([i l])
     (toChar i))))

;Problem 2
;param integers
;return b^n (mod k)
(define (b2n b n k)
  (cond [(= n 1) (remainder b k)]
        [(= n 2) (remainder (expt b 2) k)] ;base case
        [(= (remainder n 2) 0) (remainder (expt (b2n b (/ n 2) k) 2) k)] ;even
        [else (remainder (* (b2n b (- n 1) k) b) k)])) ;odd
