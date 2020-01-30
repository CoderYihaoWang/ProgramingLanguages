;; Programming Languages, Homework 5
#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;     Author        : Yihao Wang     ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;     Last modified : 30/01/2020     ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;             Problem 1              ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (racketlist->mupllist lst)
  (if (null? lst)
      (aunit)
      (apair (car lst) (racketlist->mupllist (cdr lst)))))

(define (mupllist->racketlist lst)
  (if (aunit? lst)
      null
      (cons (apair-e1 lst) (mupllist->racketlist (apair-e2 lst)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;             Problem 2              ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;     added cases      ;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        [(int? e)     e]
        [(aunit? e)   e]
        [(closure? e) e]
        [(fun? e)     (closure env e)]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([p (eval-under-env (fst-e e) env)])
           (if (apair? p)
               (apair-e1 p)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([p (eval-under-env (snd-e e) env)])
           (if (apair? p)
               (apair-e2 p)
               (error "MUPL snd applied to non-apair")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(call? e)
         (let ([c (eval-under-env (call-funexp e) env)]
               [a (eval-under-env (call-actual e) env)])
           (if (closure? c)                     
                 (let* ([f (closure-fun c)]  ; f is guaranteed to be a fun since closures are internal - no need to check
                        [nameopt (fun-nameopt f)]
                        [env (cons (cons (fun-formal f) a)
                                   (if nameopt ; ensure only strings are added to env, not #f
                                       (cons (cons nameopt c) (closure-env c))
                                       (closure-env c)))]) ; shadows the original env
                   (eval-under-env (fun-body f) env))
               (error "MUPL call applied to non-fun")))]
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;             Problem 3              ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;             Problem 4              ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define mupl-map
  (fun #f "f"
       (fun "self" "lst"
            (ifaunit (var "lst")
                     (aunit)
                     (apair (call (var "f")    (fst (var "lst")))
                            (call (var "self") (snd (var "lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "n" (add (var "n")
                                                (var "i")))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;             Problem 5              ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  ; different from the sample solution, I did not use an internal struct to keep the returned value
  ; so I had to compute the result with two functions:
  ;
  ; compute-free-vars (the outer function) recursively changes every expression's fun part to fun-challenge
  ;
  ; free-list (the local function) recursively produces a list of free variables from the fun part of an expression
  ;
  ; becuase both these two functions must be implemented recursively and hence both involve lots of branching,
  ; this makes the function body looks quite long.
  ; the sample answer merges these two steps together and only uses one-pass branching,
  ; which makes the function somewhat shorter, in terms of number of lines
  ;
  ; But in my defence,
  ; my version of solution contains much clearer branches, many of those are one liners,
  ; and they are not so deeply nested as in the sample answer,
  ; this is good for readability and debuggability
  (define (free-list e)
    (cond [(int? e)           (set)]
          [(aunit? e)         (set)]
          [(var? e)           (set (var-string e))]
          [(fun-challenge? e) (fun-challenge-freevars e)]
          [(closure? e)       (free-list (closure-fun e))]
          [(isaunit? e)       (free-list (isaunit-e e))]
          [(fst? e)           (free-list (fst-e e))]
          [(snd? e)           (free-list (snd-e e))]
          [(apair? e)         (set-union (free-list (apair-e1 e))
                                         (free-list (apair-e2 e)))]
          [(add? e)           (set-union (free-list (add-e1 e))
                                         (free-list (add-e2 e)))]
          [(call? e)          (set-union (free-list (call-funexp e))
                                         (free-list (call-actual e)))]
          [(ifgreater? e)     (set-union (free-list (ifgreater-e1 e))
                                         (free-list (ifgreater-e2 e))
                                         (free-list (ifgreater-e3 e))
                                         (free-list (ifgreater-e4 e)))]
          [(mlet? e)          (set-union (free-list (mlet-e e))
                                         (set-remove (free-list (mlet-body e))
                                                     (mlet-var e)))]
          [(fun? e)           (let ([free (set-remove (free-list (fun-body e))
                                                      (fun-formal e))])
                                (if (fun-nameopt e)
                                    (set-remove free (fun-nameopt e))
                                    free))]))  ; end of define free-list e
    (cond [(var? e)           e]
          [(int? e)           e]
          [(aunit? e)         e]
          [(fun-challenge? e) e]
          [(isaunit? e)       (isaunit (compute-free-vars (isaunit-e e)))]
          [(apair? e)         (apair (compute-free-vars (apair-e1 e))
                                     (compute-free-vars (apair-e2 e)))]
          [(fst? e)           (fst (compute-free-vars (fst-e e)))]
          [(snd? e)           (snd (compute-free-vars (snd-e e)))]
          [(add? e)           (add (compute-free-vars (add-e1 e))
                                   (compute-free-vars (add-e2 e)))]
          [(mlet? e)          (mlet (mlet-var e)
                                    (compute-free-vars (mlet-e e))
                                    (compute-free-vars (mlet-body e)))]
          [(ifgreater? e)     (ifgreater (compute-free-vars (ifgreater-e1 e))
                                         (compute-free-vars (ifgreater-e2 e))
                                         (compute-free-vars (ifgreater-e3 e))
                                         (compute-free-vars (ifgreater-e4 e)))]
          [(call? e)          (call (compute-free-vars (call-funexp e))
                                    (compute-free-vars (call-actual e)))]
          [(fun? e)           (let ([nameopt (fun-nameopt e)]
                                    [formal  (fun-formal  e)]
                                    [body    (compute-free-vars (fun-body e))])
                                (fun-challenge nameopt formal body
                                               (let ([free (set-remove (free-list body) formal)])
                                                 (if nameopt
                                                     (set-remove free nameopt)
                                                     free))))]
          [(closure? e)       (closure (closure-env e) (compute-free-vars (closure-fun e)))]))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e)        e]
        [(aunit? e)      e]
        [(closure? e)    e]
        ; new branch
        [(fun-challenge? e) (let ([free (fun-challenge-freevars e)])
                              (closure (filter (lambda (v)
                                                 (set-member? free (car v)))
                                               env)
                                       e))]
        [(isaunit? e)
         (let ([v (eval-under-env-c (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]
        [(apair? e)
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([p (eval-under-env-c (fst-e e) env)])
           (if (apair? p)
               (apair-e1 p)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([p (eval-under-env-c (snd-e e) env)])
           (if (apair? p)
               (apair-e2 p)
               (error "MUPL snd applied to non-apair")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([v (eval-under-env-c (mlet-e e) env)])
           (eval-under-env-c (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(call? e) ; changed branch
         (let ([c (eval-under-env-c (call-funexp e) env)]
               [a (eval-under-env-c (call-actual e) env)])
           (if (closure? c)                     
               (let* ([f (closure-fun c)]  
                      [nameopt (fun-challenge-nameopt f)]
                      [env (cons (cons (fun-challenge-formal f) a)
                                 (if nameopt
                                     (cons (cons nameopt c) (closure-env c))
                                     (closure-env c)))])
                 (eval-under-env-c (fun-challenge-body f) env))
               (error "MUPL call applied to non-fun")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
