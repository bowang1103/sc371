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
    [VList (elms) (foldr string-append  ""
                    (list "[" 
                          (let ([ptvals (map pretty elms)])
                            (foldl (lambda (el rst) (string-append rst (string-append ", " el))) (first ptvals) (rest ptvals)))
                          "]"))]
    #|[VTuple (elms) (foldr string-append  ""
                    (list "(" 
                          (let ([ptvals (map pretty elms)])
                            (foldl (lambda (el rst) (string-append rst (string-append ", " el))) (first ptvals) (rest ptvals)))
                          ")"))]|#
    [VTrue () "true"]
    [VFalse () "false"]
    [VEmpty () ""]
    [VObject (type value loc flds) (cond
                                     [(equal? type "Int") (pretty value)]
                                     [(equal? type "Str") (pretty value)]
                                     [(equal? type "List") (pretty value)]
                                     [(equal? type "Bool") (if (equal? "1" (pretty value)) "True" "False")])]
    [VClosure (args body env) (error 'prim "Can't print closures yet")]
    [VPoint (name field) (error 'prim "VPoint")]))
			
(define (print [arg : CVal]) : void
  (display (string-append (pretty arg) "\n")))

; None False (zero of any number type) 
; (empty sequence () [] "") (empty mapping {}) 
; (obj ___bool___ or ___len___ return false or 0)
(define (trueOrFalse (arg : CVal)) : CVal
  (type-case CVal arg
    [VNum (n) (if (= 0 n) (VFalse) (VTrue))]
    [VStr (s) (if (equal? "" s) (VFalse) (VTrue))]
    [VList (ls) (if (empty? ls) (VFalse) (VTrue))]
    [VTrue () (VTrue)]
    [VFalse () (VFalse)]
    ;; TODO: all other implicit false
    [else (VTrue)]
    ))

(define (mod (l : number) (r : number)) : number
    (if (< l 0) (mod (+ l r) r)
        (if (>= l r) (mod (- l r) r) l))) 

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
        [val-r (getPrimVal (AVal-val arg2))]
        [loc-l (getObjLoc (AVal-val arg1))]
        [loc-r (getObjLoc (AVal-val arg2))])
    (case op
      [(==) (if (equal? val-l val-r)
                (CId 'True)
                (CId 'False))]
      [(!=) (if (equal? val-l val-r)
                (CId 'False)
                (CId 'True))]
      [(is) (if (equal? loc-l loc-r)
                (CId 'True)
                (CId 'False))]
      [(!is) (if (equal? loc-l loc-r)
                 (CId 'False)
                 (CId 'True))]
      ;; TODO: add all other cases
      [else (cond 
              ;; NUMBER CASE (and BOOL)
              [(and (VNum? val-l) (VNum? val-r))
               (case op
                 [(+) ($to-object (CNum (+ (VNum-n val-l) (VNum-n val-r))))]
                 [(-) ($to-object (CNum (- (VNum-n val-l) (VNum-n val-r))))]
                 [(*) ($to-object (CNum (* (VNum-n val-l) (VNum-n val-r))))]
                 [(/) ($to-object (CNum (/ (VNum-n val-l) (VNum-n val-r))))] ;; should take care /0 case
                 [(//) ($to-object (CNum (floor (/ (VNum-n val-l) (VNum-n val-r)))))]
                 [(%) ($to-object (CNum (mod (VNum-n val-l) (VNum-n val-r))))]
                 [(<) (if (< (VNum-n val-l) (VNum-n val-r)) (CId 'True) (CId 'False))]
                 [(<=) (if (<= (VNum-n val-l) (VNum-n val-r)) (CId 'True) (CId 'False))]
                 [(>) (if (> (VNum-n val-l) (VNum-n val-r)) (CId 'True) (CId 'False))]
                 [(>=) (if (>= (VNum-n val-l) (VNum-n val-r)) (CId 'True) (CId 'False))]
                 )]
              ;; STRING CASE
              [(and (VStr? val-l) (VStr? val-r))
               (case op
                 [(+) ($to-object (CStr (string-append (VStr-s val-l) (VStr-s val-r))))])]
              [else (error 'prim2 "no case yet")])]
    )))

;; get object value from an Ans
(define (getPrimVal (obj : CVal)) : CVal
  (type-case CVal obj
    [VObject (type value loc flds) value]
    [else (error 'getPrimVal "input not an object")]))

;; get object loc from an Ans
(define (getObjLoc (obj : CVal)) : Location
  (type-case CVal obj
    [VObject (type value loc flds) loc]
    [else (error 'getPrimVal "input not an object")]))
