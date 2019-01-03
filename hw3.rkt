#lang racket

;Author: Thomas Nemeh. October 2nd, 2018.

(define firsts
  (lambda (llyst)
    (if (null? llyst) null (cons (car (car llyst)) (firsts (cdr llyst))))))

;(firsts '( (a b c) (d e f) (g h i) ) ) ;returns (a d g)

(define rests
  (lambda (llyst)
    (if (null? llyst) null (cons (cdr (car llyst)) (rests (cdr llyst))))))

;(rests '( (a b c) (d e f) (g h i) ) ) ;returns ( (b c) (e f) (h i) )

(define addvec
  (lambda (vec1 vec2)
    (if (null? vec1) null (cons (+ (car vec1) (car vec2)) (addvec (cdr vec1) (cdr vec2))))))

;(addvec '(1 2 3) '(4 5 6) ) ;returns (5 7 9)
;(addvec '( ) '( ) ) ;returrns ( )

(define dot-product
  (lambda (vec1 vec2)
    (if (null? vec1) 0 (+ (* (car vec1) (car vec2)) (dot-product (cdr vec1) (cdr vec2))))))

;(dot-product '(1 2 3) '(4 5 6) ) ;returns 32 (= 1*4 + 2*5 + 3*6)
;(dot-product '( ) '( )) ;returns 0

 (define dot-row
   (lambda (vec mat)
     (if (null? mat) null (cons (dot-product vec (car mat)) (dot-row vec (cdr mat))))))

;(dot-row '(1 2 3) '((1 4 7) (2 5 8) (3 6 9)) ) ;returns (30 36 42)

(define transpose
  (lambda (mat)
    (if (null? (car mat)) null (cons (firsts mat) (transpose (rests mat))))))

;(transpose '((1 2 3) (4 5 6)) ) ;returns ((1 4) (2 5) (3 6) )

(define matmult-a
  (lambda (mat1 trans-mat2)
    (if (null? mat1) null (cons (dot-row (car mat1) trans-mat2) (matmult-a (cdr mat1) trans-mat2)))))

(define matmult
  (lambda (mat1 mat2)
    (matmult-a mat1 (transpose mat2))))

;(matmult '((1 2 3) (4 5 6)) '((1 2) (3 4) (5 6)) ) ;returns ((22 28) (49 64))
;(not (list? null))
;(cons 'a '(b c d))

(define atom?
  (lambda (x)
    (cond
      ((list? x) #f)
      (else #t))))

(define flatten
  (lambda (L)
    (cond
      [(null? L) null]
      [(not (list? (car L))) (cons (car L) (flatten (cdr L)))]
      [else (append (flatten (car L)) (flatten (cdr L)))])))

;(flatten '(x y z z y) ) ;returns(x y z z y)
;(flatten '(a (x (y)) (((y)) y z))) ;returns (a x y y y z)

(define count
  (lambda (L)
    (cond
      [(null? L) 0]
      [(number? L) L] ; this means L is an atom
      [else (apply + (map count L))])))

;(count '(1 2 3 10))

(define apply-to
  (lambda (f L)
    (cond
      [(null? L) null]
      [(atom? L) (f L)]
      [else (map (lambda (x) (apply-to f x)) L)])))

;(apply-to add1 '(3 (4 5))) ;is (4 (5 6))

(define myOr (lambda args
  (cond
    [(null? args) #f]
    [(car args) #t]
    [else (apply myOr (cdr args))])))

(define element-of?
  (lambda (a L)
    (letrec ([h (lambda (x)
                  (cond
                    [(null? x) #f]
                    [(atom? x) (eq? x a)]
                    [else (apply myOr (map h x))]))])
      (h L))))

;(element-of? 'c '(a (b (1 2 (c) x b)))); returns #t
;(element-of? 'c '(a (b (1 2 (c) x b)))); returns #t
    

