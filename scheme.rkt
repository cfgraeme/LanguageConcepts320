
;320 HW #2


;makes a list with n copies of e
(define (make_list_of_size n e)
 (cond((= n 0) cons e '())
      (else(cons e (make_list_of_size (- n 1) e)))
  ))

; the derivative of a list with respect to x of a list of terms will be the list of derivatives
(define (d-term_list TL x)
  (cond ((null? TL) '())
        (else (map (lambda (T) (d-term T x)) TL))
        ))

;removes all occurrences of the constant term 0 from the polynomial with the following exceptions:
;The polynomial '(+ T 0) becomes simply T, as does the polynomial '(+ 0 T).
;It replaces all terms of the form (a x 1) by the term (a x).
(define (simplify_poly T x)
  (cond ((null? T) null) ; '() returns '()
        ((null? (cddr (remove_zero T))) (cadr (remove_zero T))) ;'(+ T) returns T
        (else (remove_zero (simplify T x))))) ;remove zeros, simplify linear terms
      
(define (simplify T x)
  (cond ((null? T) T)
        ((eqv? (car T) '+) (cons (car T) (simplify (cdr T) x))) ;keep leading operator
        ((number? (car T)) (cons (car T) (simplify (cdr T) x))) ;keep constants
        ((variable? (car T) x) (cons (car T) (simplify (cdr T) x)))  ;keep variables
        ((list? (car T)) (cons (simplify_term (car T) x) (simplify (cdr T) x))) ;simplify terms
        (else (cons (car T) (simplify (cdr T) x))))) ;keep else

(define (simplify_term T x)
  (cond ((null? T) T)
        ((eqv? (car T) 1) (simplify_term (cdr T) x))
        (else (cons (car T) (simplify_term (cdr T) x)))))
                
(define (remove_zero T)
  (cond ((null? T) null)
        ((eqv? (car T) 0) (remove_zero (cdr T))) ;remove zero
        (else (cons (car T) (remove_zero (cdr T))))))


(define (product? E x)
  (cond ((null? E) #f)
        ((null? (cdr E)) #f)
        ((null? (cddr E)) #f)
        ((and (eqv? (car E) '*) (polynomial? (caddr E) x) (polynomial? (cadr E) x)) #t)
        (else #f))) 

(define (d_product E x)
  (cond ((product? E x) (cons '+ (cons (cons '* (cons (caddr E) (cons (d-polynomial (cadr E) x) '())))
                        (cons (cons  '* (cons (cadr E) (cons (d-polynomial (caddr E) x) '()))) '()) )))
        (else '())))


(define (quotient? E x)
  (cond ((null? E) #f)
        ((null? (cdr E)) #f)
        ((null? (cddr E)) #f)
        ((and (eqv? (car E) '/) (polynomial? (caddr E) x) (polynomial? (cadr E) x)) #t)
        (else #f)))


(define (d-quotient E x)
  (cond ((quotient? E x) (cons '/  (cons (cons '- (cons (cons '* (cons (caddr E) (cons (d-polynomial (cadr E) x) '())))
                                       (cons (cons  '* (cons (cadr E) (cons (d-polynomial (caddr E) x) '()))) '()) ))
                                          (cons (cons '* (cons (caddr E) (cons (caddr E) '()))) '()))))
                         (else '())))