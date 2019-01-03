#lang racket
(define top (lambda (x) x))

;returns a list with the first instance of a in lat removed
(define rember-k 
  (lambda (a lat k)
    (cond
      [(null? lat) (k null)]
      [(eq? a (car lat)) (k (cdr lat))]
      [else (rember-k a (cdr lat) (lambda (x) (k (cons (car lat) x))))])))

(rember-k 'b '(a b a b a b b) (lambda (x) x))

;returns the index of a in lat, -1 if a is not in lat
(define index-k
  (lambda (a lat k)
    (cond
      [(null? lat) (k 0)]
      [(eq? a (car lat)) (k 0)]
      [else (index-k a (cdr lat) (lambda (x) (k (+ x 1))))])))

(index-k 'r '(a b a b z k)top)

;returns true if x is an atom, flase otherwise
(define atom?
  (lambda (x)
    (cond
      ((list? x) #f)
      (else #t))))

;returns the maximum element in a not necessarily flat list L
(define max-k
  (lambda (L k)
    (cond
      [(null? L) (k 0)]
      [(atom? (car L)) (max-k (cdr L) (lambda (x) (if (> x (car L)) (k x) (k (car L)))))]
      [else (max-k (car L) (lambda (x) (max-k (cdr L) (lambda (y) (if (> x y) (k x) (k y))))))])))

(max-k '(5 3 (4 7 2 (5) 1)) top)

;returns a list with all instances of old in a not necessarily flat list L replaced by new
(define replace-k
  (lambda (old new L k)
    (cond
      [(null? L) (k null)]
      [(atom? (car L)) (if (eq? (car L) old)
                           (replace-k old new (cdr L) (lambda (x) (k (cons new x))))
                           (replace-k old new (cdr L) (lambda (x) (k (cons (car L) x)))))]
      [else (replace-k old new (car L) (lambda (x) (replace-k old new (cdr L)
                                         (lambda (y) (k (cons x y))))))])))

(replace-k 'a 'x '(a b c (b c (a))) top) ;produces (x b c (b c (x)))

;returns a list of pairs for every element in l starting with a
(define pairone-k
  (lambda (a l k)
    (cond
      [(null? l) (k null)]
      [else (pairone-k a (cdr l) (lambda (x) (k (cons (list a (car l)) x))))])))

(pairone-k 'a '(3 5 7 5 2 6) top)

;returns a list of pairs with one element from l1 and another from l2
(define pairall-k
  (lambda (l1 l2 k)
    (cond
      [(null? l1) (k null)]
      [else (pairall-k (cdr l1) (cdr l2) (lambda (x) (k (cons (list (car l1) (car l2)) x))))])))

(pairall-k '(1 2 3) '(a b c) top)


  
                           
    


