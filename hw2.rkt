#lang racket

;cs275 hw2
;Author: Thomas Nemeh. September 24, 2018.

;merges 2 lists together in sorted order
(define merge
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) lat2)
      ((null? lat2) lat1)
      ((< (car lat1) (car lat2)) (cons (car lat1) (merge (cdr lat1) lat2)))
      (else (cons (car lat2) (merge lat1 (cdr lat2)))))))

;sorts list using insertion sort
(define sort
  (lambda (lat)
  (letrec([f (lambda (sorted-lat unsorted-lat)
               (if (null? unsorted-lat) sorted-lat (f (insert '() sorted-lat (car unsorted-lat)) (cdr unsorted-lat))))]
          [insert (lambda (lat1 lat2 atom)
                    (cond
                      [(null? lat2) (append lat1 (cons atom '()))]
                      [(< atom (car lat2)) (append lat1 (cons atom lat2))]
                      [else (insert (append lat1 (cons (car lat2) '())) (cdr lat2) atom)]))])
    (f '() lat))))

;tests if sublist is contained in list
(define contains-sublist
  (lambda (sublist list)
    (letrec([f (lambda (list1 list2)
                 (cond
                   [(null? list1) #t]
                   [(null? list2) #f]
                   [(equal? (car list1) (car list2)) (f (cdr list1) (cdr list2))]
                   [else #f]))])
      (cond
        [(null? sublist) #t]
        [(null? list) #f]
        [(f sublist list) #t]
        [else (contains-sublist sublist (cdr list))]))))

;cuts sublist out of list
(define rember-sublist
  (lambda (sublist list)
    (letrec([f (lambda (list1 list2)
                 (cond
                   [(null? list1) #t]
                   [(null? list2) #f]
                   [(equal? (car list1) (car list2)) (f (cdr list1) (cdr list2))]
                   [else #f]))]
            [remove-sublist (lambda (list1 list2)
                 (if (null? list1) list2 (remove-sublist (cdr list1) (cdr list2))))])
      (cond
        [(null? list) '()]
        [(f sublist list) (remove-sublist sublist list)]
        [(cons (car list) (rember-sublist sublist (cdr list)))]))))

;retrieve the phone number of a person in a phone-book based on their name
(define phone-number
  (lambda (person phone-book)
    (cond
      [(null? phone-book) 'disconnected ]
      [(equal? (car (car phone-book)) person) (car (cdr (car phone-book)))]
      [else (phone-number person (cdr phone-book))])))

;retrieve the name of a person in a phone-book based on their phone number
(define person
  (lambda (phone-number phone-book)
    (cond
      [(null? phone-book) 'disconnected ]
      [(equal? (cdr (car phone-book)) (cons phone-number '())) (car (car phone-book))]
      [else (person phone-number (cdr phone-book))])))

;count the length of a list
(define length
  (lambda (lat)
    (letrec([length-a (lambda (lat acc)
               (cond
                 [(null? lat) acc]
                 [else (length-a (cdr lat) (+ 1 acc))]))])
      (length-a lat 0)))) 

;count instances of a in lat
(define count
  (lambda (a lat)
    (letrec([count-a (lambda (a lat acc)
               (cond
                 [(null? lat) acc]
                 [(equal? a (car lat)) (count-a a (cdr lat) (+ 1 acc))]
                 [else (count-a a (cdr lat) acc)]))])
      (count-a a lat 0))))

;returns the maximum number in a list
(define max
  (lambda (lat)
    (letrec([max-a (lambda (lat acc)
               (cond
                 [(null? lat) acc]
                 [(> (car lat) acc) (max-a (cdr lat) (car lat))]
                 [(max-a (cdr lat) acc)]))])
      (max-a lat 0))))

;returns index of first occurance of a
(define index
  (lambda (a lat)
    (letrec([index-a (lambda (a lat acc)
               (cond
                 [(null? lat) -1]
                 [(equal? (car lat) a) acc]
                 [(index-a a (cdr lat) (+ 1 acc))]))])
      (index-a a lat 0))))


                
                  
    


