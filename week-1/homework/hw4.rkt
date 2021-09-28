
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define ones (lambda () (cons 1 ones)))

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0)    (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons
                 (if (= (modulo x 5) 0) (- x) x)
                 (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (s)
                (cons s
                      (lambda () (f (if (string=? s "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
       (letrec ([f (lambda(x)
                (lambda () (cons (cons 0 (car (x))) (f (cdr (x))))))])
      (f s)))