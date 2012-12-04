#lang plai-typed

(require "python-core-syntax.rkt"
         "python-syntax.rkt"
         "python-objects.rkt"
         "python-primitives.rkt"
         "python-desugar.rkt")

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
   (CFunc (list 'to-print) (list)
          (list ($to-object (CStr ""))) ;; default ''
     (CPrim1 'print (CId 'to-print)))))

;; callable
(define callable-lambda
  ($to-object
   (CFunc (list 'check-callable) (list) (list)
     (CPrim1 'callable (CId 'check-callable)))))

;; bool
(define bool-lambda
  ($to-object
   (CFunc (list 'check-bool) (list)
          (list (CId 'False)) ;; default false
     (CPrim1 'bool (CId 'check-bool)))))

;; int
(define int-lambda
  ($to-object
   (CFunc (list 'to-int) (list)
          (list ($to-object (CNum 0))) ;; default 0
     (CPrim1 'int (CId 'to-int)))))

;; float
(define float-lambda
  ($to-object
   (CFunc (list 'to-float) (list)
          (list ($to-object (CNum 0.0))) ;; default 0.0
     (CPrim1 'float (CId 'to-float)))))

;; str
(define str-lambda
  ($to-object
   (CFunc (list 'to-str) (list)
          (list ($to-object (CStr ""))) ;; default ''
     (CPrim1 'str (CId 'to-str)))))

;; len
(define len-lambda
  ($to-object
   (CFunc (list 'to-len) (list) (list)
     (CPrim1 'len (CId 'to-len)))))

#|;; list
(define list-lambda
  ($to-object
   (CFunc (list 'to-list) (list)
          (list ($to-object (CList (list)))) ;; default []
     (CPrim1 'list (CId 'to-list)))))|#
;; list
(define list-lambda
  (let ([elt (getId)])
    (desugar (PyFunc (PyArgs (list 'iterable) (list) (list (PyList (list))))
                     (PyListComp (PyId elt) (list (PyComp (PyId elt) (PyId 'iterable) (list (PyId 'True)))))))))

;; tuple
(define tuple-lambda
  ($to-object
   (CFunc (list 'to-tuple) (list)
          (list ($to-object (CTuple (list)))) ;; default ()
     (CPrim1 'tuple (CId 'to-tuple)))))

;; set
(define set-lambda
  ($to-object
   (CFunc (list 'to-set) (list)
          (list ($to-object (CSetV (list)))) ;; default ()
     (CPrim1 'set (CId 'to-set)))))

;; dict
(define dict-lambda
  ($to-object
   (CFunc (list 'to-dict) (list)
          (list ($to-object (CDict (list) (list)))) ;; default {}
     (CPrim1 'dict (CId 'to-dict)))))

;; range
(define range-lambda
  ($to-object
   (CFunc (list 'arg1 'arg2 'arg3)
	      (list)
          (list (CId 'None) (CId 'None)) ;; defualt start and step
     (CIf (CPrim2 'is (CId 'arg2) (CId 'None))
          ($to-object (CRange (list ($to-object (CNum 0)) (CId 'arg1) ($to-object (CNum 1)))))
          (CIf (CPrim2 'is (CId 'arg3) (CId 'None))
               ($to-object (CRange (list (CId 'arg1) (CId 'arg2) ($to-object (CNum 1)))))
               ($to-object (CRange (list (CId 'arg1) (CId 'arg2) (CId 'arg3)))))))))

;; abs
(define abs-lambda
  ($to-object
   (CFunc (list 'to-abs) (list) (list)
     (CPrim1 'abs (CId 'to-abs)))))

;; isinstance
(define isinstance-lambda
  ($to-object
   (CFunc (list 'instance 'class) (list) (list)
     (CPrim2 'instanceof (CPrim1 'tagof (CId 'instance)) (CPrim1 'tagof (CApp (CId 'class) (list) (list)))))))

;; locals
(define locals-lambda
  ($to-object
   (CFunc (list) (list) (list)
          (CPrim0 'locals))))

;; min
(define min-lambda
  ($to-object
   (CFunc (list 'sequence) (list) (list)
     (CPrim1 'min (CId 'sequence)))))

;; max
(define max-lambda
  ($to-object
   (CFunc (list 'sequence) (list) (list)
     (CPrim1 'max (CId 'sequence)))))

;; iter
(define iter-lambda
  ($to-object
   (CFunc (list 'iterableo 'sentinel) 
          (list) 
          (list (CId 'None))
     (CIf (CPrim2 'is (CId 'sentinel) (CId 'None))
          (CApp (CGetfield (CId 'iterableo) "__iter__") (list) (list))
          ($to-object (CCalIter (CId 'iterableo) (CId 'sentinel)))))))

;; next
(define next-lambda
  ($to-object
   (CFunc (list 'iterobj) (list) (list)
     (CApp (CGetfield (CId 'iterobj) "__next__") (list) (list)))))

;; all
#|(define all-lambda
  ($to-object
   (CFunc (list 'iterable) (list) (list)
     (CTryExn (CLet 'iterobj (CApp (CGetfield (CId 'iterable) "__iter__") (list) (list))
                    (COperation (CId 'iterobj) "Iter" "all" (list)))
              (CExceptHandler (CId 'None) (raise-error "TypeError" "Object Not Iterable") (CId 'AttributeError))
              (CId 'None)))))
|#
(define all-lambda
  (desugar (PyFunc (PyArgs (list 'iterable) (list) (list))
                   (PyTryExcept
                    (PyFor (PyId 'temp-elm)
                           (PyId 'iterable)
                           (PyIf (PyUnaryOp 'not (PyId 'temp-elm))
                                 (PyReturn (PyId 'False))
                                 (PyEmp))
                           (PyId 'True))
                    (PyExceptHandler (PyId 'None) (PyRaise (PyStr "Object Not Iterable") 
                                                           (PyApp (PyId 'TypeError) (list (PyStr "Object Not Iterable")) (list))) 
                                     (PyId 'AttributeError))
                    (PyId 'None)))))

;; any
#|(define any-lambda
  ($to-object
   (CFunc (list 'iterable) (list) (list)
     (CTryExn (CLet 'iterobj (CApp (CGetfield (CId 'iterable) "__iter__") (list) (list))
                    (COperation (CId 'iterobj) "Iter" "any" (list)))
              (CExceptHandler (CId 'None) (raise-error "TypeError" "Object Not Iterable") (CId 'AttributeError))
              (CId 'None)))))|#
(define any-lambda
  (desugar (PyFunc (PyArgs (list 'iterable) (list) (list))
                   (PyTryExcept
                    (PyFor (PyId 'temp-elm)
                           (PyId 'iterable)
                           (PyIf (PyId 'temp-elm)
                                 (PyReturn (PyId 'True))
                                 (PyEmp))
                           (PyId 'False))
                    (PyExceptHandler (PyId 'None) (PyRaise (PyStr "Object Not Iterable") 
                                                           (PyApp (PyId 'TypeError) (list (PyStr "Object Not Iterable")) (list))) 
                                     (PyId 'AttributeError))
                    (PyId 'None)))))

#|
;; filter
(define filter-lambda
  ($to-object
   (CFunc (list 'filt-func 'iterable) (list) (list)
     (CTryExn (CLet 'iterobj (CApp (CGetfield (CId 'iterable) "__iter__") (list) (list))
                    (COperation (CId 'iterobj) "Iter" "filter" (list(CId 'filt-func))))
              (CExceptHandler (CId 'None) (raise-error "TypeError" "Object Not Iterable") (CId 'AttributeError))
              (CId 'None)))))
|#
;; filter
(define filter-lambda
  (desugar (PyFunc (PyArgs (list 'filt-func 'iterable) (list) (list))
                   (PyIf (PyCompare (PyId 'filt-func) (list 'is) (list (PyId 'None)))
                         (PyGenComp (PyId 'filt-elm)
                                    (list (PyComp (PyId 'filt-elm) (PyId 'iterable) (list (PyId 'filt-elm)))))
                         (PyGenComp (PyId 'filt-elm) 
                                    (list (PyComp (PyId 'filt-elm) (PyId 'iterable) (list (PyApp (PyId 'filt-func) (list (PyId 'filt-elm)) (list))))))
                   ))))
  
;; ___assertTure
(define assert-true-lambda
  ($to-object 
   (CFunc (list 'check-true) (list) (list)
     (CIf (CId 'check-true) (CId 'True) (CError (CStr "Assert True Failed"))))))

;; ___assertFalse
(define assert-false-lambda
  ($to-object 
   (CFunc (list 'check-false) (list) (list)
     (CIf (CId 'check-false) (CError (CStr "Assert False Failed")) (CId 'True)))))

;; ___assertIn
(define assert-in-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2) (list) (list)
     (CIf (CPrim2 'in (CId 'arg1) (CId 'arg2)) (CId 'True) (CError (CStr "Assert In Failed"))))))

;; ___assertNotIn
(define assert-notin-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2) (list) (list)
     (CIf (CPrim2 '!in (CId 'arg1) (CId 'arg2)) (CId 'True) (CError (CStr "Assert Not In Failed"))))))

;; ___assertEqual
(define assert-equal-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2) (list) (list)
     (CIf (CPrim2 '== (CId 'arg1) (CId 'arg2)) (CId 'True) (CError (CStr "Assert Equal Failed"))))))

;; ___assertNotEqual
(define assert-notequal-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2) (list) (list)
     (CIf (CPrim2 '!= (CId 'arg1) (CId 'arg2)) (CId 'True) (CError (CStr "Assert Not Equal Failed"))))))

;; ___assertRaises
;(define assert-raises-lambda)

;; ___assertIs
(define assert-is-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2) (list) (list)
     (CIf (CPrim2 'is (CId 'arg1) (CId 'arg2)) (CId 'True) (CError (CStr "Assert Is Failed"))))))

;; ___assertIsNot
(define assert-isnot-lambda
  ($to-object 
   (CFunc (list 'arg1 'arg2) (list) (list)
     (CIf (CPrim2 '!is (CId 'arg1) (CId 'arg2)) (CId 'True) (CError (CStr "Assert Is Not Failed"))))))

;; ___fail
(define fail-lambda
  ($to-object 
   (CFunc (list) (list) (list)
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
#|
def ___assertRaises(e, f, *args):
  try:
    f(*args)
  except e as the_exn:
    return
  else:
    assert(False)
  assert(False)

___assertRaises(TypeError, range)

|#
  
;; ___assertRaises
(define assert-raises-lambda
  ($to-object 
   (CFunc (list 'e_exception 'f_function) (list 'args) (list) 
          (CSeq (CTryExn (CApp (CId 'f_function) (list) (list (CId 'args)))
                         ;(CExceptHandler ($to-object (CStr "the_exn")) (CRet (CEmpty)) (CId 'e_exception))
                         (CExceptHandler ($to-object (CStr "the_exn")) (CRet (CId 'None)) (CId 'e_exception))
                         ;(CExceptHandler ($to-object (CStr "the_exn")) (CPrim1 'print (CStr "PASS")) (CId 'e_exception))
                         ;(CPrim1 'print (CStr "FAILED")))
                         (CError (CStr "Assert False Failed2")))
                ;(CPrim1 'print (CStr "FAILED!!\n"))))))
                (CError (CStr "Assert False Failed3"))))))

(define (exception-lambda (type : string)) : CExp
  ($to-object 
   (CFunc (list 'err-msg) (list)
          (list ($to-object (CStr "")))    ; default argument ""
          ($to-object (CException type (CId 'err-msg))))))

(define (ContructExc (excpt : CExp) (message : string)) : CExp
  (CApp excpt (list ($to-object (CStr message))) (list)))


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
        (bind 'range range-lambda)
        (bind 'abs abs-lambda)
        (bind 'min min-lambda)
        (bind 'max max-lambda)
        (bind 'iter iter-lambda)
        (bind 'next next-lambda)
        (bind 'all all-lambda)
        (bind 'any any-lambda)
        (bind 'filter filter-lambda)
        (bind 'isinstance isinstance-lambda)
        (bind 'locals locals-lambda)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        (bind '___assertIn assert-in-lambda)
        (bind '___assertNotIn assert-notin-lambda)
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertNotEqual assert-notequal-lambda)
        (bind '___assertRaises assert-raises-lambda)
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
        (bind 'ValueError (exception-lambda "ValueError"))
        (bind 'AttributeError (exception-lambda "AttributeError"))
        (bind 'StopIteration (exception-lambda "StopIteration"))
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



