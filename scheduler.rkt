#lang racket

(define requests
  '((0 8) (8 11)
    (1 3) (4 13) (5 6)
    (3 10) (7 9) (10 12)))

(define lst1 '((0 7) (4 8) (3 14)))
(define lst2 '((0 1) (2 6)))

(define (divide lst n)
  (if (< n 1)
      (list '() lst)
      (list (cons (car lst) (car (divide (cdr lst) (- n 1)))) 
            (cadr (divide (cdr lst) (- n 1))))))

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (st lst1 lst2)
  (if (null? lst1)
      lst2
      (if (null? lst2)
          lst1
          (if (< (car (car lst1)) (car (car lst2)))
              (cons (car lst1) (st (cdr lst1) lst2))
              (cons (car lst2) (st lst1 (cdr lst2)))))))

(define (ft lst1 lst2)
  (if (null? lst1)
      lst2
      (if (null? lst2)
          lst1
          (if (< (cadr (car lst1)) (cadr (car lst2)))
              (cons (car lst1) (ft (cdr lst1) lst2))
              (cons (car lst2) (ft lst1 (cdr lst2)))))))

(define (duration lst1 lst2)
  (list (- (cadr (car lst1)) (car (car lst1))) (- (cadr (car lst2)) (car (car lst2)))))

(define (sh lst1 lst2)
  (if (null? lst1)
      lst2
      (if (null? lst2)
          lst1
          (if (< (car (duration lst1 lst2)) (cadr (duration lst1 lst2)))
              (cons (car lst1) (sh (cdr lst1) lst2))
              (cons (car lst2) (sh lst1 (cdr lst2)))))))

(define (add2end e lst)
  (if (null? lst)
      (cons e '())
      (cons (car lst) (add2end e (cdr lst)))))

(define (lastl lst)
  (if (null? (cdr lst))
      (car lst)
      (lastl (cdr lst))))

(define (schedule lst final_list)
  (cond
    [(null? lst) final_list]
    [(null? final_list) (schedule (cdr lst) (cons (car lst) final_list))]
    [(equal? (car (car lst)) (cadr (lastl final_list))) (schedule (cdr lst) (add2end (car lst) final_list))]
    [(> (car (car lst)) (cadr (lastl final_list))) (schedule (cdr lst) (add2end (car lst) final_list))]
    [ else (schedule (cdr lst) final_list)]))

(define (st_msort lst)
  (if (< (length lst) 2)
      lst
      (st (st_msort (car (divide lst (/ (length lst) 2))))
             (st_msort (cadr (divide lst (/ (length lst) 2)))))))

(define (ft_msort lst)
  (if (< (length lst) 2)
      lst
      (ft (ft_msort (car (divide lst (/ (length lst) 2))))
             (ft_msort (cadr (divide lst (/ (length lst) 2)))))))

(define (sh_msort lst)
  (if (< (length lst) 2)
      lst
      (sh (sh_msort (car (divide lst (/ (length lst) 2))))
             (sh_msort (cadr (divide lst (/ (length lst) 2)))))))

(define (getreqlst lst sc)
  (cond
    [(equal? sc 'st) (schedule (st_msort lst) '())]
    [(equal? sc 'ft) (schedule (ft_msort lst) '())]
    [(equal? sc 'sh) (schedule (sh_msort lst) '())]))
