#lang lazy

(require lazy/force)

;; This function will play the role of specifying the recursion
;; that should define the factorial function
;; It can simply be generalized to other recursive functions
(define fact-recurse
    (lambda (f) (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))))

(define fact-part
    (lambda (self) (lambda (n) (if (= n 0) 1 (* n ((self self) (- n 1)))))))
(define (fact-part self)
        (lambda (n) (if (= n 0) 1 (* n ((self self) (- n 1))))))

;; Clearly, and somewhat remarkably, the below statement yields the 
;; appropriate definition of factorial; however, it's not as concise
;; as we'd like it to be
;(define factorial (fact-part fact-part))

;; One can easily verify that fact-part is equivalent to
;; (lambda (self) (fact-recurse (self self)))

;; One can thus make a simpler definition of factorial:
;(define factorial ((lambda (self) (fact-recurse (self self)))
;                   (lambda (self) (fact-recurse (self self)))))

;; Simplifying the above further to avoid the obvious repeated form:
;(define factorial ((lambda (x) (x x))
;                   (lambda (self) (fact-recurse (self self)))))

;; Replacing "self" with "x", we obtain the traditional form of the
;; combinator definition of factorial
;(define factorial ((lambda (x) (x x))
;                   (lambda (x) (fact-recurse (x x)))))

;; Generalizing this to arbitrary "f-recurse" functions, we obtain
;; the normal-order (read: lazy) Y combinator
(define Y (lambda (f)
                  ((lambda (x) (x x))
                   (lambda (x) (f (x x))))))

;; We can now define factorial in terms of the normal-order Y combinator:
(define factorial (Y fact-recurse))
(displayln (!! (map factorial (build-list 10 values))))

;; To remove any doubts about the generality of the Y combinator,
;; we here define fibonacci in terms of Y
(define fib-recurse (lambda (f) (lambda (n) (if (or (= n 0) (= n 1)) n (+ (f (- n 1)) (f (- n 2)))))))
(define fibonacci (Y fib-recurse))
(displayln (!! (map fibonacci (build-list 20 values))))
