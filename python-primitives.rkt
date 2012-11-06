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
    [VObject (type flds) (error 'prim "Cant print Object yet")]
    [VClosure (env args body) (error 'prim "Can't print closures yet")]))
			
(define (print [arg : CVal]) : void
  (display (pretty arg)))

(define (python-prim1 [op : symbol] [arg : CAns]) : CAns
  (case op
    [(print) (begin (print (AVal-val arg)) arg)]))

