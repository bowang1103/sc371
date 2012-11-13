#lang plai-typed

(require "python-core-syntax.rkt"
         "python-objects.rkt")

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

(define-type-alias Lib (CExp -> CExp))

(define print-lambda
  ($to-object 
   (CFunc (list 'to-print)
     (CPrim1 'print (CId 'to-print)))))

;; callable
(define callable-lambda
  ($to-object
   (CFunc (list 'check-callable)
     (CPrim1 'callable (CId 'check-callable)))))

;; bool
(define bool-lambda
  ($to-object
   (CFunc (list 'check-bool)
     (CPrim1 'bool (CId 'check-bool)))))

;; ___assertTure
(define assert-true-lambda
  ($to-object 
   (CFunc (list 'check-true)
     (CIf (CId 'check-true) (CId 'True) (CError (CStr "Assert True Failed"))))))

;; ___assertFalse
(define assert-false-lambda
  ($to-object 
   (CFunc (list 'check-false)
     (CIf (CId 'check-false) (CError (CStr "Assert False Failed")) (CId 'True)))))

;; ___assertIn
(define assert-in-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2)
     (CIf (CPrim2 'in (CId 'arg1) (CId 'arg2)) (CId 'True) (CError (CStr "Assert In Failed"))))))

;; ___assertNotIn
(define assert-notin-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2)
     (CIf (CPrim2 'in (CId 'arg1) (CId 'arg2)) (CError (CStr "Assert Not In Failed")) (CId 'True)))))

;; ___assertEqual
(define assert-equal-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2)
     (CIf (CPrim2 '== (CId 'arg1) (CId 'arg2)) (CId 'True) (CError (CStr "Assert Equal Failed"))))))

;; ___assertNotEqual
(define assert-notequal-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2)
     (CIf (CPrim2 '== (CId 'arg1) (CId 'arg2)) (CError (CStr "Assert Not Equal Failed")) (CId 'True)))))

;; ___assertRaises
;(define assert-raises-lambda)

;; ___assertIs
(define assert-is-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2)
     (CIf (CPrim2 'is (CId 'arg1) (CId 'arg2)) (CId 'True) (CError (CStr "Assert Is Failed"))))))

;; ___fail
(define fail-lambda
  ($to-object 
   (CFunc (list)
    (CError (CStr "Fail")))))

;(define true-val
 ; (CTrue))

(define true-val
  ($to-object (CTrue)))

(define false-val
  ($to-object (CFalse)))

#|
Exception Built in function
|#

(define typeerror 
  ($to-object 
   (CFunc (list 'arg1) 
	;(CPrim1 'print (CId 'arg1)))))
      (CObject "TypeError" (CId 'arg1) (CEmpty)))))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define lib-functions
  (list (bind 'print print-lambda)
        (bind 'callable callable-lambda)
        (bind 'bool bool-lambda)
        (bind 'True true-val)
        (bind 'False false-val)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        (bind '___assertIn assert-in-lambda)
        (bind '___assertNotIn assert-notin-lambda)
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertNotEqual assert-notequal-lambda)
        ;(bind '___assertRaises assert-raises-lambda)
        (bind '___assertIs assert-is-lambda)
        (bind '___fail fail-lambda)

        ;;Exception built in 
        (bind 'TypeError typeerror)

))

(define (python-lib (expr : CExp)) : CExp
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (CLet name value
                                 (python-lib/recur (rest libs)))))]))]
    (python-lib/recur lib-functions)))


