#lang racket

;; This works much the same as the normal order Y combinator, but
;; with a lambda wrapped around (self self) to avoid infinite
;; recursion.

;; This function will play the role of specifying the recursion
;; that should define the factorial function
;; It can simply be generalized to other recursive functions
(define fact-recurse
    (lambda (f) (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))))

(define fact-part
    (lambda (self) (lambda (n) (if (= n 0) 1 (* n ((self self) (- n 1)))))))

;; Clearly, and somewhat remarkably, the below statement yields the 
;; appropriate definition of factorial; however, it's not as concise
;; as we'd like it to be
;(define factorial (fact-part fact-part))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Differences start here ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One can easily verify that fact-part is equivalent to
;; (lambda (self) (fact-recurse (lambda (y) ((self self) y))))
;; Note that (lambda (y) ((self self) y)) is just (self self)
;; wrapped in a lambda to avoid infinite evaluation of (self self).

;; One can thus make a simpler definition of factorial:
;(define factorial ((lambda (self) (fact-recurse (lambda (y) ((self self) y))))
;                   (lambda (self) (fact-recurse (lambda (y) ((self self) y))))))

;; Simplifying the above further to avoid the obvious repeated form:
;(define factorial ((lambda (x) (x x))
;                   (lambda (self) (fact-recurse (lambda (y) ((self self) y))))))

;; Replacing "self" with "x", we obtain the traditional form of the
;; combinator definition of factorial
;(define factorial ((lambda (x) (x x))
;                   (lambda (x) (fact-recurse (lambda (y) ((x x) y))))))

;; Generalizing this to arbitrary "f-recurse" functions, we obtain
;; the applicative order (read: strict) Y combinator:
(define Y (lambda (f)
                  ((lambda (x) (x x))
                   (lambda (x) (f (lambda (y) ((x x) y)))))))

;; We can now define factorial in terms of the normal-order Y combinator:
(define factorial (Y fact-recurse))
(displayln (map factorial (build-list 10 values)))

;; To remove any doubts about the generality of the Y combinator,
;; we here define fibonacci in terms of Y
(define fib-recurse (lambda (f) (lambda (n) (if (or (= n 0) (= n 1)) n (+ (f (- n 1)) (f (- n 2)))))))
(define fibonacci (Y fib-recurse))
(displayln (map fibonacci (build-list 20 values)))
