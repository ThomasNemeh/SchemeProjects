;Thomas Nemeh Lab 8. 12/4/2018.

#lang racket

(require "streams.rkt" "keyboard.rkt")


(define Tester$ (cons$ 1 (cons$ 2 (cons$ 3 Tester$))))

;delete all instacnes of x from stream s
(define rember-all$
  (lambda (x s)
    (if (equal? (car$ s) x) (rember-all$ x (cdr$ s)) (cons$ (car$ s) (rember-all$ x (cdr$ s))))))

;(define rember-test$ (rember-all$ 3 Tester$))
;(print$ rember-test$)

;replace instances of x with y in s
(define subst-all$
  (lambda (x y s)
    (if (equal? (car$ s) x) (cons$ y (subst-all$ x y (cdr$ s))) (cons$ (car$ s) (subst-all$ x y (cdr$ s))))))

;(define subst-test$ (subst-all$ 3 9 Tester$))
;(print$ subst-test$)

(define pairsFrom$
     (lambda (p)
          (cons$ p (pairsFrom$ (nextPair p)))))

(define pairs$ (pairsFrom$ (cons 1 1)))

;defines next pair in our grid
(define nextPair
  (lambda (p)
    (cons (car p) (+ 1 (cdr p)))))

;(print$ (pairsFrom$ (cons 1 1)))

;scales stream by n
(define scale
  (lambda (n s)
    (map$ (lambda (t) (* n t)) s)))

;merges two ordered lists into one ordered list from least to greatest
(define merge$
  (lambda (s1 s2)
    (cond
      [(< (car$ s1) (car$ s2)) (cons$ (car$ s1) (merge$ (cdr$ s1) s2))]
      [(> (car$ s1) (car$ s2)) (cons$ (car$ s2) (merge$ s1 (cdr$ s2)))]
      [(= (car$ s1) (car$ s2)) (cons$ (car$ s1) (merge$ (cdr$ s1) (cdr$ s2)))])))

(define Ones$ (cons$ 1 Ones$))

(define IntsFrom$ (lambda (n)
  (cons$ n (IntsFrom$ (+ n 1)))))
(define Ints$ (IntsFrom$ 0))
(define Evens$ (map$ (lambda (t) (* 2 t)) Ints$))
(define Odds$ (+$ Evens$ Ones$))

;(print$ (merge$ Evens$ Odds$))

;list of all integers with no prime factors other than 2, 3, or 5
(define S (cons$ 1 (merge$ (scale 2 S) (merge$ (scale 3 S) (scale 5 S)))))

;(print$ S)

;the sequence of sums for the first n terms of a sequence
(define PartialSums$
  (lambda (s)
    (cons$ (car$ s) (+$ (PartialSums$ s) (cdr$ s)))))

(define *$ (lambda (s1 s2) (cons$ (* (car$ s1) (car$ s2)) (*$ (cdr$ s1) (cdr$ s2)))))

;stream of factorials from 1 onwards 
(define fact-stream$    
     (cons$ 1 (*$ fact-stream$ (IntsFrom$ 1))))

;(print$ fact-stream$)

;(define e-series$ (cons$ 1 (map$ (lambda (x) (/ 1.0 x)) fact-stream$)))

;(print$ e-series$)

;(define Powers$ (lambda (x) (cons$ 1 (map$ (lambda (t) (* x t)) (Powers$ x)))))

;(define E (lambda (x) (PartialSums$ (*$ (Powers$ x) e-series$))))

;functions to get stream of even or odd numbers
(define alternate-all$
  (lambda (x s)
    (if (even? x) (alternate-all$ (+ x 1) (cdr$ s)) (cons$ (car$ s) (alternate-all$ (+ x 1) (cdr$ s))))))

;stream of factorials for odds and evens
(define odds-fact$ (cdr$ (alternate-all$ 2 fact-stream$)))
(define evens-fact$ (cdr$ (alternate-all$ 1 fact-stream$)))

;(print$ evens-fact$)
;(print$ odds-fact$)

;get 1 / previous streams as first step to construct cos and sin
(define almost-sin$ (cons$ 1 (map$ (lambda (x) (/ 1.0 x)) odds-fact$)))
(define almost-cos$ (map$ (lambda (x) (/ 1.0 x)) evens-fact$))

;(define sin (lambda (x) (PartialSums$ (*$  (Powers$ x) sin-series$))))
;(define cos (lambda (x) (PartialSums$ (*$  (Powers$ x) cos-series$))))

;(print$ almost-sin$)
;(print$ almost-cos$)

;function to generate sin and cos
(define generate-trig$
  (lambda (count s)
    (if (even? count)
        (cons$ 0 (cons$ (car$ s) (generate-trig$ (+ 1 count) (cdr$ s))))
        (cons$ 0 (cons$ (* -1 (car$ s)) (generate-trig$ (+ 1 count) (cdr$ s)))))))

;generate sin and cos series
(define sin-series$ (generate-trig$ 2 almost-sin$))
(define cos-series$ (cons$ 1 (generate-trig$ 1 almost-cos$)))
;(print$ sin$)
;(print$ cos-series$)

;(output$ (keyboard-stream))

;filter to print b if two a's are entered
(define grune-a-b
  (lambda (s)
    (cond
      [(eq? s 'the-empty-stream) s]
      [else (if (eq? (car$ s) 'a)
        (if (eq? (car$ (cdr$ s)) 'a) (cons$ 'b (grune-a-b (cdr$ (cdr$ s)))) (cons$ (car$ s) (grune-a-b (cdr$ s))))
        (cons$ (car$ s) (grune-a-b (cdr$ s))))])))

;(output$ (grune-a-b (keyboard-stream)))

;returns filter to print param b if two param a's are enterd 
(define grune
  (lambda (a b)
    (lambda (s) 
      (cond
        [(eq? s 'the-empty-stream) s]
        [else
          (if (eq? (car$ s) a)
            (if (eq? (car$ (cdr$ s)) a) (cons$ b ((grune a b) (cdr$ (cdr$ s)))) (cons$ (car$ s) ((grune a b) (cdr$ s))))
            (cons$ (car$ s) ((grune a b) (cdr$ s))))]))))

;(output$ ((grune 'x 'y) (keyboard-stream)))

;(output$ ((grune 'c 'd) ((grune 'b 'c) ((grune 'a 'b) (keyboard-stream)))))

