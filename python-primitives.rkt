#lang plai-typed

(require "python-core-syntax.rkt"
         "python-objects.rkt")

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
    [VObject (type value flds) (cond
                                 [(equal? type "Int") (pretty value)]
                                 [(equal? type "Str") (pretty value)])]
    [VClosure (args body env) (error 'prim "Can't print closures yet")]))
			
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
(define (python-prim2 [op : symbol] [arg1 : CAns] [arg2 : CAns]) : CExp
  (let ([val-l (getPrimVal (AVal-val arg1))]
        [val-r (getPrimVal (AVal-val arg2))])
    (case op
      [(==) (if (equal? val-l val-r) 
                      ($to-object (VTrue))
                      ($to-object (VFalse)))]
      [(!=) (if (equal? val-l val-r)
                      ($to-object (VFalse)) 
                      ($to-object (VTrue)))]
      ;; TODO: add all other cases
      [else (cond 
              ;; both arg2 are number
              [(and (VNum? val-l) (VNum? val-r))
               (case op
                 [(+) ($to-object (VNum (+ (VNum-n val-l) (VNum-n val-r))))])]
              [else (error 'prim2 "no case yet")])]
    )))

;; get object value from an Ans
(define (getPrimVal (obj : CVal)) : CVal
  (type-case CVal obj
    [VObject (type value flds) value]
    [else (error 'getPrimVal "input not an object")]))