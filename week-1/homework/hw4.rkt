
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

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

(define (cycle-lists xs ys)
     (letrec([f (lambda(n)
                (lambda() (cons  (cons (list-nth-mod xs n) (list-nth-mod ys n)) (f (+ n 1)) )))])
     (f 0 )))

(define (vector-assoc v vec)
         (letrec  ([len (vector-length vec)]
                   [f (lambda(current)
                     (cond[(= current len) #f]
                          [(pair? (vector-ref vec current)) (if (equal? (car (vector-ref vec current)) v)
                                                                (vector-ref vec current)
                                                                (f (+ current 1)))]
                          [#t (f (+ current 1))]))])
         (f 0)))

(define (cached-assoc xs n)
         (letrec
             ([cache (make-vector n)]
              [cache-slot 0]
              [find (lambda(x)
                      (let ([v-from-cache (vector-assoc x cache)])
                        (if v-from-cache
                            v-from-cache
                            (let ([v-from-xs (vector-assoc x xs)])
                              (if v-from-xs
                                  (begin
                                    (vector-set! cache cache-slot v-from-xs)
                                    (set! cache-slot (remainder (+ cache-slot 1) n))
                                    v-from-xs)
                                  v-from-xs)))))])
           find))
