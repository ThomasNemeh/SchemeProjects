#lang racket










; definition of tree datatype
(provide tree? empty-tree? non-empty-tree? empty-tree non-empty-tree value list-of-children number-of-children leaf? makeTree)
(provide Empty T1 T2 T3 T4 T5 T6 T7 T8)


; recognizers
(define tree? (lambda (t) (or (empty-tree? t) (non-empty-tree? t))))

(define empty-tree? (lambda (x) (null? x)))                     

(define non-empty-tree? (lambda (x)
                          (cond
                            [(list? x) (eq? (car x) 'tree)]
                            [else #f])))



; constructors
(define empty-tree (lambda () null))

(define non-empty-tree (lambda (value list-of-children)
                                      (list 'tree value list-of-children)))

; Convenience constructor 
(define makeTree 
  (lambda l 
    (non-empty-tree (car l) (cdr l)))) 

; utilty functions
(define value (lambda (tree)
                (cond
                  [(empty-tree? tree) 'error]
                  [else (cadr tree)])))

(define list-of-children (lambda (tree)
                (cond
                  [(empty-tree? tree) 'error]
                  [else (caddr tree)])))

(define number-of-children 
  (lambda (tr)
                (cond
                  [(empty-tree? tr) 'error]
                  [else (length (list-of-children tr))]))) 

(define leaf? 
  (lambda (tr)
                (cond
                  [(empty-tree? tr) 'error]
                  [else (zero? (number-of-children tr))])))



; example trees 

(define Empty (empty-tree))
(define T1 (makeTree 50)) 
(define T2 (makeTree 22)) 
(define T3 (makeTree 10)) 
(define T4 (makeTree 5)) 
(define T5 (makeTree 17)) 
(define T6 (makeTree 73 T1 T2 T3)) 
(define T7 (makeTree 100 T4 T5)) 
(define T8 (makeTree 16 T6 T7)) 



(define index-helper
  (lambda (a lat)
    (foldr (lambda (head tail) (if (equal? a head) 0 (+ 1 tail))) 0 lat)))

(define index
  (lambda (a lat)
    (let([pos (index-helper a lat)])
      (if (> pos (- 1 (length lat))) -1 pos))))

;(index 'z '(r e d a f))

(define replace
  (lambda (a b lat)
    (foldr (lambda (head tail) (if (equal? a head) (cons b tail) (cons head tail))) null lat)))

;(replace 3 5 '(1 2 3 4 5 4 3 2 1)) ;returns (1 2 5 4 5 4 5 2 1)

;(define bags '((duffle 8) (garment-bag 2) (briefcase 5) (valise 7) (steamer-trunk 65)))

(define weight
  (lambda (bags)
    (foldl (lambda (head tail) (+ (cadr head) tail)) 0 bags)))

;(weight bags)

(define heaviest-weight
  (lambda (bags)
    (foldr (lambda (head tail) (max (cadr head) tail)) 0 bags)))

(define heaviest
  (lambda (bags)
    (letrec([max (heaviest-weight bags)])
      (foldr (lambda (head tail) (if (= (cadr head) max) (car head) tail)) 0 bags))))

;(heaviest bags)

(define childSum
  (lambda (tr)
    (apply + (map cadr (list-of-children tr)))))

;(childSum T8) ;returns 173; 
;(childSum T6) ;returns 82.


(define allSum
  (lambda (tr)
    (letrec([allSum-a (lambda (tree)
               (cond
                 [(null? tree) 0]
                 [(number? tree) tree]
                 [(not (list? tree)) 0]
                 [else (apply + (map allSum-a tree))]))])
      (allSum-a tr))))

;(allSum T8) ; is 293
;(allSum T6) ;is 155

(define visitTree
  (lambda (f tr)
    (cond
      [(null? tr) null]
      [(number? tr) (f tr)]
      [(not (list? tr)) tr]
      [else (map (lambda (x) (visitTree f x)) tr)])))

;T8
;(visitTree add1 T8)

(define sizeof
    (letrec([sizeof-a (lambda (L)
               (cond
                 [(null? L) 0]
                 [(number? L) 1]
                 [(not (list? L)) 0]
                 [else (apply + (map sizeof-a L))]))])
      (lambda (tr)
        (sizeof-a tr))))

;(sizeof T8) ;returns 8
;(sizeof T6) ;retrurns 4 

(define height-helper
  (lambda (head tail)
    (cond
      [(null? head) 0]
      [(null? (list-of-children head)) 0]
      [else (max (+ 1 (foldr height-helper 0 (list-of-children head))) (+ 1 tail))])))

(define height
  (lambda (tr)
    (cond
      [(null? tr) 0]
      [else (foldr height-helper 0 (list-of-children tr))])))

;(height T8)
;(height T1)

(define count
  (lambda (n tr)
    (letrec([count-a (lambda (tree)
               (cond
                 [(null? tree) 0]
                 [(number? tree) (if (= tree n) 1 0)]
                 [(not (list? tree)) 0]
                 [else (apply + (map count-a tree))]))])
      (count-a tr))))

;(count 22 T8) ;returns 1
;(count 7 T8) ;returns 0

(define preorder
  (lambda (tr)
    (cond
      [(null? tr) null]
      [(not (list? tr)) (if (number? tr) (list tr) null)]
      [else (apply append (map preorder tr))])))


;(preorder T8) ;returns (16 73 50 22 10 100 5 17)

(define post-helper
  (lambda (head tail)
    (cond
      [(null? (list-of-children head)) (append (preorder head) tail)]
      [else (append (foldr post-helper null (list-of-children head)) (list (cadr head)) tail)])))

(define postorder
  (lambda (tr)
    (cond
      [(null? tr) null]
      [else (append (foldr post-helper null (list-of-children tr)) (list (value tr)))])))

;(postorder T8) ;returns (50 22 10 73 5 17 100 16)

