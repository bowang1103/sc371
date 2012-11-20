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
          (list ($to-object (CStr ""))) ;; default ''
     (CPrim1 'print (CId 'to-print)))))

;; callable
(define callable-lambda
  ($to-object
   (CFunc (list 'check-callable) (list)
     (CPrim1 'callable (CId 'check-callable)))))

;; bool
(define bool-lambda
  ($to-object
   (CFunc (list 'check-bool) 
          (list (CId 'False)) ;; default false
     (CPrim1 'bool (CId 'check-bool)))))

;; int
(define int-lambda
  ($to-object
   (CFunc (list 'to-int) 
          (list ($to-object (CNum 0))) ;; default 0
     (CPrim1 'int (CId 'to-int)))))

;; float
(define float-lambda
  ($to-object
   (CFunc (list 'to-float) 
          (list ($to-object (CNum 0.0))) ;; default 0.0
     (CPrim1 'float (CId 'to-float)))))

;; str
(define str-lambda
  ($to-object
   (CFunc (list 'to-str)
          (list ($to-object (CStr ""))) ;; default ''
     (CPrim1 'str (CId 'to-str)))))

;; len
(define len-lambda
  ($to-object
   (CFunc (list 'to-len) (list)
     (CPrim1 'len (CId 'to-len)))))

;; list
(define list-lambda
  ($to-object
   (CFunc (list 'to-list) 
          (list ($to-object (CList (list)))) ;; default []
     (CPrim1 'list (CId 'to-list)))))

;; tuple
(define tuple-lambda
  ($to-object
   (CFunc (list 'to-tuple)
          (list ($to-object (CTuple (list)))) ;; default ()
     (CPrim1 'tuple (CId 'to-tuple)))))

;; set
(define set-lambda
  ($to-object
   (CFunc (list 'to-set)
          (list ($to-object (CSetV (list)))) ;; default ()
     (CPrim1 'set (CId 'to-set)))))

;; dict
(define dict-lambda
  ($to-object
   (CFunc (list 'to-dict)
          (list ($to-object (CDict (list) (list)))) ;; default {}
     (CPrim1 'dict (CId 'to-dict)))))

;; abs
(define abs-lambda
  ($to-object
   (CFunc (list 'to-abs) (list)
     (CPrim1 'abs (CId 'to-abs)))))

;; isinstance
(define isinstance-lambda
  ($to-object
   (CFunc (list 'instance 'class) (list)
     (CPrim2 'instanceof (CPrim1 'tagof (CId 'instance)) (CPrim1 'tagof (CApp (CId 'class) (list)))))))

;; ___assertTure
(define assert-true-lambda
  ($to-object 
   (CFunc (list 'check-true) (list)
     (CIf (CId 'check-true) (CId 'True) (CError (CStr "Assert True Failed"))))))

;; ___assertFalse
(define assert-false-lambda
  ($to-object 
   (CFunc (list 'check-false) (list)
     (CIf (CId 'check-false) (CError (CStr "Assert False Failed")) (CId 'True)))))

;; ___assertIn
(define assert-in-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2) (list)
     (CIf (CPrim2 'in (CId 'arg1) (CId 'arg2)) (CId 'True) (CError (CStr "Assert In Failed"))))))

;; ___assertNotIn
(define assert-notin-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2) (list)
     (CIf (CPrim2 '!in (CId 'arg1) (CId 'arg2)) (CId 'True) (CError (CStr "Assert Not In Failed"))))))

;; ___assertEqual
(define assert-equal-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2) (list)
     (CIf (CPrim2 '== (CId 'arg1) (CId 'arg2)) (CId 'True) (CError (CStr "Assert Equal Failed"))))))

;; ___assertNotEqual
(define assert-notequal-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2) (list)
     (CIf (CPrim2 '!= (CId 'arg1) (CId 'arg2)) (CId 'True) (CError (CStr "Assert Not Equal Failed"))))))

;; ___assertRaises
;(define assert-raises-lambda)

;; ___assertIs
(define assert-is-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2) (list)
     (CIf (CPrim2 'is (CId 'arg1) (CId 'arg2)) (CId 'True) (CError (CStr "Assert Is Failed"))))))

;; ___assertIsNot
(define assert-isnot-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2) (list)
     (CIf (CPrim2 '!is (CId 'arg1) (CId 'arg2)) (CId 'True) (CError (CStr "Assert Is Not Failed"))))))

;; ___fail
(define fail-lambda
  ($to-object 
   (CFunc (list) (list)
    (CError (CStr "Fail")))))

;(define true-val
 ; (CTrue))

(define true-val
  ($to-object (CTrue)))

(define false-val
  ($to-object (CFalse)))

(define none-val
  ($to-object (CEmpty)))	
#| Exception Built in function |#

(define (exception-lambda (type : string)) : CExp
  ($to-object 
   (CFunc (list 'arg1) (list)
          ($to-object (CException type (CId 'arg1))))))

(define (ContructExc (excpt : CExp) (message : string)) : CExp
  (CApp excpt (list ($to-object (CStr message)))))


(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define lib-functions
  (list (bind 'print print-lambda)
        (bind 'callable callable-lambda)
        (bind 'bool bool-lambda)
        (bind 'True true-val)
        (bind 'False false-val)
        (bind 'None none-val)
        (bind 'int int-lambda)
        (bind 'float float-lambda)
        (bind 'str str-lambda)
        (bind 'len len-lambda)
        (bind 'list list-lambda)
        (bind 'tuple tuple-lambda)
        (bind 'set set-lambda)
        (bind 'dict dict-lambda)
        (bind 'abs abs-lambda)
        (bind 'isinstance isinstance-lambda)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        (bind '___assertIn assert-in-lambda)
        (bind '___assertNotIn assert-notin-lambda)
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertNotEqual assert-notequal-lambda)
        ;(bind '___assertRaises assert-raises-lambda)
        (bind '___assertIs assert-is-lambda)
        (bind '___assertIsNot assert-isnot-lambda)
        (bind '___fail fail-lambda)

        ;;Exception built in 
        (bind 'Exception (exception-lambda "Exception"))
        (bind 'TypeError (exception-lambda "TypeError"))
        (bind 'ZeroDivisionError (exception-lambda "ZeroDivisionError"))
        (bind 'KeyError (exception-lambda "KeyError"))
        (bind 'IndexError (exception-lambda "IndexError"))
        (bind 'RuntimeError (exception-lambda "RuntimeError"))

))


;; Purpose: lookup lib-functions
(define (lookup_lib-funcs [name : symbol] [lib-funcs : (listof LibBinding)]) : CExp
  (cond
    [(empty? lib-funcs) (core-error (string-append (symbol->string name) " : location not found in store"))]
    [else  (if (equal? name (bind-left (first lib-funcs)))
               (bind-right (first lib-funcs))
               (lookup_lib-funcs name (rest lib-funcs)))]))



(define (python-lib (expr : CExp)) : CExp
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (CLet name value
                                 (python-lib/recur (rest libs)))))]))]
    (python-lib/recur lib-functions)))



