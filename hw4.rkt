#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;     Author        : Yihao Wang     ;;;;;;;;;;
;;;;;;;;;;     Last modified : 29/01/2020     ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; 1.   sequence
;      argument(s) : low: number, high: number, stride: number
;      return      : number list
;      description : returns a list of numbers from low to high inclusive,
;                    seperated by stride and in sorted order
;      note        : stride must be positive

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


; 2.   string-append-map
;      argument(s) : xs: string list, suffix: string
;      return      : string list
;      description : returns a list of string, each of them is an element in xs
;                    suffixed with suffix, in order
;      note        : uses Racket's library functions map and string-append

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))


; 3.   list-nth-mod
;      argument(s) : xs: list, n: number  
;      return      : any
;      description : takes a list xs and a number n, returns xs[n % (length xs)]
;                    raises errors if n is negative or xs is empty
;      note        : uses list-tail, car, remainder and length

(define (list-nth-mod xs n)
  (cond [(< n 0)    (error "list-nth-mod: negetive number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t         (car (list-tail xs (remainder n (length xs))))]))


; 4.   stream-for-n-steps
;      argument(s) : s: stream, n: number
;      return      : list
;      description : makes a list of the first n elements from a stream
;      note        : n must be non-negative

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([s (s)]) ; let only uses bindings before the let expression, so it is ok to use the same name s
        (cons (car s) (stream-for-n-steps (cdr s) (- n 1))))))


; 5.   funny-number-stream
;      argument(s) : none
;      return      : number stream
;      description : a stream of natrual numbers with any elements divisible by 5 negated
;      note        : this piece of code uses a different way of making streams
;                    from the method shown in the lectures,
;                    instead of thunking the body part of the letrec and the second part of cons,
;                    I directly thunked the return value of the helper function f.
;                    doing this will reduce the number of thunks from 2 to only 1.
;                    I think this will produce cleaner and more understandable code

(define funny-number-stream
  (letrec ([f (lambda (x)
                (lambda () (cons (if (= 0 (remainder x 5)) (- x) x)
                                 (f (+ x 1)))))])
    (f 1)))


; 6.   dan-then-dog
;      argument(s) : none 
;      return      : string stream
;      description : a stream alternation from "dan.jpg" and "dog.jpg"
;      note        : same as the previous problem

(define dan-then-dog
  (letrec ([f (lambda (b)
                (lambda () (cons (if b "dan.jpg" "dog.jpg")
                                 (f (not b)))))])
    (f #t)))

; 7.   stream-add-zero
;      argument(s) : s: stream
;      return      : stream
;      description : takes a stream, replace each of its elements v with (0 . v)

(define (stream-add-zero s)
  (let ([s (s)])
    (lambda () (cons (cons 0 (car s))
                     (stream-add-zero (cdr s))))))


; 8.   cycle-lists
;      argument(s) : xs: list, ys: list
;      return      : pair stream
;      description : takes two lists, zip them to a stream of pairs
;      note        : uses 1 thunk instead of 2, see the note of problem 5

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n xs ys)
                (lambda () (cons (cons (list-nth-mod xs n)
                                       (list-nth-mod ys n))
                                 (f (+ n 1) xs ys))))])
    (f 0 xs ys)))


; 9.   vector-assoc
;      argument(s) : v: any, vec: vector 
;      return      : pair | #f
;      description : returns the first pair in vec whose car is equal to v
;                    ignoring any non-pair values in vec
;                    returns #f if no found records
;      note        : uses vector-length, vector-ref and equal?

(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (= n (vector-length vec))
                    #f
                    (let ([p (vector-ref vec n)])
                          (if (and (pair? p) (equal? v (car p)))
                               p
                               (f (+ n 1))))))])
    (f 0)))


; 10.  cached-assoc
;      argument(s) : xs: list, n: number
;      return      : function(v: any) -> pair | #f
;      description : given a list and the cache size, return a cached version of assoc
;                    where vec is already partially applied
;      note        : n must be positive

(define (cached-assoc xs n)
  (let ([memo (make-vector n #f)]
        [pos  0])
        (lambda (v)
          (let ([c (vector-assoc v memo)])
            (if c
                c
                (let ([c (assoc v xs)])
                  (if c
                      (begin (vector-set! memo pos c)
                             (set! pos (if (= pos (- n 1)) 0 (+ pos 1)))
                             c)
                      #f)))))))


; 11.  while-less e1 do e2
;      description : repeat evaluating e2 until e2 is larger than e1
;                    e1 is evaluated exactly once, and e2 at least once
;      note        : e1 and e2 must evaluate to numbers

(define-syntax while-less
  (syntax-rules (do)
                [(while-less e1 do e2)
                 (letrec ([e e1]
                          [f (lambda () (if (< e2 e) ; did not check if e2 is a number
                                                     ; as the problem specified that
                                                     ; we could assume it is
                                            (f)
                                            #t))])
                   (f))]))



