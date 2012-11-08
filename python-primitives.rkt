#lang plai-typed

(require "python-core-syntax.rkt")

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#

(require (typed-in racket/base [display : (string -> void)]))

(define (pretty [arg : CVal]) : string
  (type-case CVal arg
    [VNum (n) (to-string n)]
    [VStr (s) s]
    [VTrue () "true"]
    [VFalse () "false"]
    [VEmpty () ""]
    [VObject (type flds) (cond
                           [(equal? type "Int") (to-string (some-v (hash-ref flds "value")))])]
    [VClosure (env args body) (error 'prim "Can't print closures yet")]))
			
(define (print [arg : CVal]) : void
  (display (string-append (pretty arg) "\n")))

; None False (zero of any number type) 
; (empty sequence () [] "") (empty mapping {}) 
; (obj ___bool___ or ___len___ return false or 0)
(define (bool (arg : CVal)) : CVal
  (type-case CVal arg
    [VNum (n) (if (= 0 n) (VTrue) (VFalse))]
    [VTrue () (VTrue)]
    [VFalse () (VFalse)]
    ;; TODO: all other implicit false
    [else (VTrue)]
    ))

; unaryop = {~, not, pos, neg} ~must be int, pos and neg should be numeric, not ?
(define (python-prim1 [op : symbol] [arg : CAns]) : CAns
  (case op
    [(print) (begin (print (AVal-val arg)) arg)]))

;;boolop may have to handle in other function
; boolop = {and, or}
; op = {+, -, *, /, %, **, <<, >>, bor, ^, band, //}
; compare op = {==, !=, <, <=, >, >=, is, !is, in, !in} a < b < c => a < b and b < c
(define (python-prim2 [op : symbol] [arg1 : CAns] [arg2 : CAns]) : CAns
  (case op
    [(==) (AVal (if (equal? arg1 arg2) (VTrue) (VFalse)) (AVal-env arg2) (AVal-sto arg2) (AVal-lenv arg2))]
    [(!=) (AVal (if (equal? arg1 arg2) (VFalse) (VTrue)) (AVal-env arg2) (AVal-sto arg2) (AVal-lenv arg2))]
    ;; TODO: add all other cases
    ))

