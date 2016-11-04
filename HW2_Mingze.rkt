#lang racket

;upper's helper function
;works like a filter
;@param l list of chars
;@return list of only Alphabetic chars
(define (onlyAlpha l)
  (cond
   [(empty? l) empty]
   [else
    (if (char-alphabetic? (first l))
        (cons (first l) (onlyAlpha (rest l))) ;put back as original
        (onlyAlpha (rest l)))])) ;remove non-alphabetic chars

;Question 1
;@param s string
;@return string without Non-alphabetic chars and
;  All letters are converted to uppercase.
(define (upper s)
  (let ([t (string->list s)])
    (let ([c (onlyAlpha t)]) ;filter to only alphabetic chars
      (list->string (map (lambda (e) (char-upcase e)) ;map to upper
                         c))))) ;transfer back to string

;ROT12's helper function
;@param c char
;@return integer
;A - Z => 0 - 25
(define (toInt c)
  (let ([n (char->integer c)])
    (- n 65)))

;Question 2
;@param str string
;@return string rotate 12 without Non-alphabetic chars and
;  All letters are converted to uppercase.
(define (ROT12 str)
  (let ([chars (upper str)]) ;normalizes the string to chars
    (list->string ;transfer back to string
     (for/list ([i chars]) ; for-loop
       (integer->char (+ (remainder (+ (toInt i) 12) 26) 65))))))
       ;1. to int; 2. rot 12; 3. devide 26; 4. back to char

;pow_back's helper function
;@param a integer under
;@param n integer power
;@param lst list of previous results
;@return list of first n power of a
(define (powH a n lst)
  (if (= n 0) ;base case
      lst
      (powH a (- n 1) (cons (* a (first lst)) lst)))) ;n is used as a counter

;Question 3-1
;@param a integer under
;@param n integer power
;@return backward list of first n power of a
(define (pow_back a n)
  (powH a n (list 1))) ;call helper function with initial value a^0 = 1

;pow_num's helper function
;@param a integer under
;@param n integer power
;@param prev the last term of lst
;@param lst list of previous results
(define (powH_append a n prev lst)
  (if (= n 0)
      lst
      (powH_append a (- n 1) (* a prev) (append lst (list (* a prev))))))

;Question 3-2
;@param a integer under
;@param n integer power
;@return forward list of first n power of a
(define (pow_num a n)
  (powH_append a n 1 (list 1))) ;initial value a^0 = 1

;power2's helper function
;@param n first n values of i
;@param lst list of previous results
;@return list of the first n values of i in a^(2^i)
(define (pow2H n lst)
  (if (= n 0)
      lst
      (pow2H (- n 1) (cons (expt (first lst) 2) lst)))) ;built-in exponentiation function

;Question 4-1
;@param a integer under
;@param n first n values of i
;@return list of the first n values of i in a^(2^i)
(define (power2 a n)
  (reverse (pow2H n (list a)))) ;initial value a^(2^0) = a

;Question 4-2
;@param a integer under
;@param n first n values of i
;@param k the divisor
;@return list of the remainder of the first n values of i in a^(2^i)
(define (remP2 a n k)
  (map (lambda (a2) (remainder a2 k))
       (power2 a n)))