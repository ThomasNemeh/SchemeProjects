#lang racket

;Thomas Nemeh Programming Abstraction Homework #1

;returns true if the argument is an atom, false otherwise
(define atom?
  (lambda (x)
    (cond
      ((list? x) #f)
      (else #t))))

;returns true if the argument is an empty list or a list whose every element is an atom
(define lat?
  (lambda (x)
    (cond
      ((null? x) #t)
      ((atom? (car x)) (lat? (cdr x)))
      (else #f))))

;returns true if the argument is not a list of atoms, false if it is an empty list or a list of atoms
(define not-lat?
  (lambda (x)
    (cond
      ((null? x) #f)
      ((not (atom? (car x))) #t)
      (else (not (atom? (car x))) (not-lat? (cdr x))))))

;returns true if the argument is an empty list or a list of integers, false otherwise
(define list-of-ints?
  (lambda (x)
    (cond
      ((null? x) #t)
      ((integer? (car x)) (list-of-ints? (cdr x)))
      (else #f))))

;returns true if predicate king-of-element returns true for each element of list s
(define list-of-same?
  (lambda (kind-of-element s)
    (cond
      ((null? s) #t)
      ((kind-of-element (car s)) (list-of-same? kind-of-element (cdr s)))
      (else #f))))

; function that takes only one argument, a predicate, and returns a function that takes an argument and says if the predicate returns #t for each element of the argument.
(define list-of-same2
  (lambda (kind-of-element)
    (lambda (s)
      (cond
        ((null? s) #t)
        ((kind-of-element (car s)) (list-of-same? kind-of-element (cdr s)))
        (else #f)))))
      
(define x 0) ;counter variable to aid recursion in the following functions: allmembers, rember2, and index
(define list '()) ;list to aid recursion in allmembers

;returns true if every member of lat1 is also a member of lat2, and #f if that isn't true.
(define allmembers
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) (set! x 0) #t)
      ((null? lat2) (set! x 0) #f)
      (else 
        (cond
          ((= x 0) (set! x 1) (set! list lat2)))
        (cond
          ((equal? (car lat1) (car lat2)) (allmembers (cdr lat1) list))
          (else (allmembers lat1 (cdr lat2))))))))

;removes the second occurence of a from lat, if there is one.
(define rember2
  (lambda (a lat)
    (cond
      ((null? lat) (set! x 0) '())
      (else
         (cond
           ((equal? (car lat) a) (set! x (+ x 1))))
         (cond
           ((= x 2) (set! x 0) (cdr lat))
           (else (cons (car lat) (rember2 a (cdr lat)))))))))

;removes every occurence of two consecutive instances of a in lat.       
(define rember-pair
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((null? (cdr lat)) lat)
      (else
         (cond
           ((and (equal? (car lat) a) (equal? (car (cdr lat)) a)) (rember-pair a (cdr (cdr lat))))
           (else (cons (car lat) (rember-pair a (cdr lat)))))))))
    
;builds a list containing n copies of object exp.
(define duplicate
  (lambda (n exp)
    (cond
      ((= n 0) '())
      (else (cons exp (duplicate (- n 1) exp))))))

;return largest value in lat
(define largest
 (lambda (lat)
   (cond
     ((null? lat) '())
     ((null? (cdr lat)) (car lat))
     (else (max (car lat) (largest (cdr lat)))))))
        
;returns the index of the first occurrence of atom a in lat. If a is not an element of lat this returns -1.
(define index
  (lambda (a lat)
    (cond
      ((null? lat) (set! x 0) -1)
      ((equal? a (car lat)) (define return x) (set! x 0) return)
      (else (set! x (+ x 1)) (index a (cdr lat))))))
    







