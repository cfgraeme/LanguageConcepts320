; CSCI 320 Fall 2016
; Symbolic Differentiation Example
#lang racket

; this predicate just tells us whether a term is a constant term
(define constant? number?)

; this predicate tells us whether a term is a variable named x
(define (variable? T x) 
  (and (symbol? T) (eqv? T x)))

; this predicate tells us whether we have a term of the form (k x) representing the polynomial kx
(define (linear_term? T x)
  (cond ((and (number? (car T)) (variable? (cadr T) x) (null? (cddr T))))
        (else #f)))

; this predicate defines a "term in x" as either a constant, the variable x, a linear term in x
;  or a term of the form (a x n), where a and n are numbers.
(define (term? T x) 
  (cond ((constant? T) #t)
        ((variable? T x) #t)
        ((linear_term? T x) #t)
        ((and (number? (car T)) (variable? (cadr T) x) (number? (caddr T))))
        (else #f)))

; this predicate tells us whether a given expression is a list of "terms in x"
(define (term_list? T x)
   (cond ((null? T) #t)
         ((and (term? (car T) x) (term_list? (cdr T) x)) #t)
         (else #f)))


; this predicate defines a polynomial E in x as having the form of a term,
;  or the form (+ term_in_x term_in_x...term_in_x)
;  that is a list whose first element is the plus sign and whose cdr is a list of terms in x
(define (polynomial? T x)
  (cond ((term? T x) #t)
        ((and (eqv? (car T) '+) (term_list? (cdr T) x)) #t)
        (else #f)))

; here we define some more meaningfully named functions for handling terms in x of the form (a x n)
(define (get-coefficient T) (car T))
(define (get-variable T) (cadr T))
(define (get-exponent T) (caddr T))


; now we implement symbolic derivatives of terms.  Note, the derivative of a term in x with respsect to a variable
;  other than x is 0.  Likewise, the derivative with respect to x of a term in a variable other than x is 0.

(define (d-term T x)
  (cond ((constant? T) 0)
        ((variable? T x) 1)
        ((linear_term? T x) (get-coefficient T))
        ((term? T x) (list (* (get-coefficient T) (get-exponent T))
                         (get-variable T)
                         (- (get-exponent T) 1)))
        (else 0)))

; the derivative of a list with respect to x of a list of terms will be the list of derivatives
(define (d-term_list TL x)
  (cond ((null? TL) '())
        (else (cons(d-term (car TL) x) (d-term_list (cdr TL) x))))) 

; a polynomial is a sum of terms in x, so the derivative of a polynomial is the sum of the derivatives
;  of its constituent terms with respct to x
(define (d-polynomial P x)
  (cond ((term? P x) (d-term P x))
        ((polynomial? P x) (cons (car P)  (d-term_list (cdr P) x)))
        (else 0)))
                       
