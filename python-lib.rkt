#lang plai-typed

(require "python-core-syntax.rkt")

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

(define-type-alias Lib (CExp -> CExp))

(define print-lambda
  (CFunc (list 'to-print)
    (CPrim1 'print (CId 'to-print))))

;; ___assertTure
(define assert-true-lambda
  (CFunc (list 'check-true)
    (CIf (CId 'check-true) (CTrue) (CError (CStr "Assert failed")))))

;; ___assertFalse
(define assert-false-lambda
  (CFunc (list 'check-false)
    (CIf (CId 'check-false) (CError (CStr "Assert failed")) (CTrue))))

;; ___assertIn
(define assert-in-lambda)

;; ___assertNotIn
(define assert-notin-lambda)

;; ___assertEqual
(define assert-equal-lambda
  (CFunc (args)
    (Cif (CPrim2 '== args) (CTrue) (CError (CStr "Assert failed")))))

;; ___assertNotEqual
(define assert-notequal-lambda
  (CFunc (args)
    (Cif (CPrim2 '== args) (CError (CStr "Assert failed")) (CTrue))))

;; ___assertRaises
(define assert-raises-lambda)

;; ___assertIs
(define assert-is-lambda
  (CFunc (args)
    (Cif (CPrim2 'is args) (CTrue) (CError (CStr "Assert failed")))))

;; ___fail
(define fail-lambda)

(define true-val
  (CTrue))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define lib-functions
  (list (bind 'print print-lambda)
        (bind 'True true-val)
        (bind '___assertTrue assert-true-lambda)

))

(define (python-lib expr)
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (CLet name value
                                 (python-lib/recur (rest libs)))))]))]
    (python-lib/recur lib-functions)))


