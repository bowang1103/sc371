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
    [VStr (s) (foldr string-append "" (list "'" s "'"))]
    [VList (elms) (foldr string-append  ""
                    (list "[" 
                          (let ([ptvals (map pretty elms)])
                            (foldl (lambda (el rst) (string-append rst (string-append ", " el))) (first ptvals) (rest ptvals)))
                          "]"))]
    [VTuple (elms) (foldr string-append  ""
                    (list "(" 
                          (let ([ptvals (map pretty elms)])
                            (foldl (lambda (el rst) (string-append rst (string-append ", " el))) (first ptvals) (rest ptvals)))
                          ")"))]
    [VDict (dict) (letrec ([keys (hash-keys dict)]
                           [pair (map (lambda(x) 
                                       (foldr string-append ""
                                              (list (pretty x) 
                                                    ": " 
                                                    (pretty (some-v (hash-ref dict x)))))) keys)])
                    (foldr string-append ""
                           (list "{" 
                                 (foldl (lambda (el rst) (string-append rst (string-append ", " el))) (first pair) (rest pair))
                           "}")))]
    [VTrue () "true"]
    [VFalse () "false"]
    [VEmpty () ""]
    [VObject (type value loc flds) (cond
                                     [(equal? type "Int") (pretty value)]
                                     [(equal? type "Str") (pretty value)]
                                     [(equal? type "List") (pretty value)]
                                     [(equal? type "Tuple") (pretty value)]
                                     [(equal? type "Dict") (pretty value)]
                                     [(equal? type "Bool") (if (equal? "1" (pretty value)) "True" "False")])]
    [VClosure (args body env) (error 'prim "Can't print closures yet")]
    [VPoint (name field) (error 'prim "VPoint")]))
			
(define (print [arg : CVal]) : void
  (display (string-append (pretty arg) "\n")))

; None False (zero of any number type) 
; (empty sequence () [] "") (empty mapping {}) 
; (obj ___bool___ or ___len___ return false or 0)
(define (isObjTrue (obj : CVal)) : boolean
  (let ([val (getObjVal obj)])
    (type-case CVal val
               [VNum (n) (not (= 0 n))]
               [VStr (s) (not (equal? "" s))]
               [VList (ls) (not (empty? ls))]
               [VTrue () true]
               [VFalse () false]
               ;; TODO: all other implicit false
               [else true]
               )))

(define (negNumeric (obj : CVal)) : CExp
  (let ([pv (getObjVal obj)])
    (if (VNum? pv) ($to-object (CNum (* -1 (VNum-n pv)))) (core-error "Neg input should be a numeric type"))))

(define (posNumeric (obj : CVal)) : CExp
  (let ([pv (getObjVal obj)])
    (if (VNum? pv) ($to-object (CNum (VNum-n pv))) (core-error "Pos input should be a numeric type"))))

(define (intNumeric (obj : CVal)) : CExp
  (let ([pv (getObjVal obj)])
    (if (VNum? pv) ($to-object (CNum (num-to-int (VNum-n pv) 0))) (core-error "int input should be a numeric type"))))

(define (floatNumeric (obj : CVal)) : CExp
  (let ([pv (getObjVal obj)])
    (if (VNum? pv) ($to-object (CNum (* 1.0 (VNum-n pv)))) (core-error "float input should be a numeric type"))))

(define (absNumeric (obj : CVal)) : CExp
  (let ([pv (getObjVal obj)])
    (if (VNum? pv) ($to-object (CNum (if (< 0 (VNum-n pv)) (VNum-n pv) (* -1 (VNum-n pv))))) (core-error "abs input should be a numeric type"))))

(define (invertNumeric (obj : CVal)) : CExp
  (let ([pv (getObjVal obj)]
        [type (getObjType obj)])
    (if (or (equal? "Bool" type) (equal? "Int" type)) 
        ($to-object (CNum (sub1 (* -1 (VNum-n pv))))) (core-error "invert input should be a int type"))))

(define (num-to-int (n : number) (rst : number)) : number
  (if (>= n 0)
      (if (> (add1 rst) n) rst (num-to-int n (add1 rst)))
      (if (< (sub1 rst) n) rst (num-to-int n (sub1 rst)))))

(define (mod (l : number) (r : number)) : number
    (if (< l 0) (mod (+ l r) r)
        (if (>= l r) (mod (- l r) r) l)))

(define (toString (obj : CVal)) : CExp
  ($to-object (CStr (pretty obj))))

; unaryop = {~, not, pos, neg} ~must be int, pos and neg should be numeric, not ?
(define (python-prim1 [op : symbol] [arg : CAns]) : CExp
  (let ([obj (AVal-val arg)])
    (case op
      [(print) (begin (print obj) (CStr "Print Return Value"))]
      [(callable) (if (equal? "Func" (VObject-type obj)) (CId 'True) (CId 'False))]
      [(bool) (if (isObjTrue obj) (CId 'True) (CId 'False))]
      [(not) (if (isObjTrue obj) (CId 'False) (CId 'True))]
      [(~) (invertNumeric obj)]
      [(neg) (negNumeric obj)]
      [(pos) (posNumeric obj)]
      [(int) (intNumeric obj)]
      [(float) (floatNumeric obj)]
      [(str) (toString obj)]
      [(abs) (absNumeric obj)])))

;;boolop may have to handle in other function
; boolop = {and, or}
; op = {+, -, *, /, %, **, <<, >>, bor, ^, band, //}
; compare op = {==, !=, <, <=, >, >=, is, !is, in, !in} a < b < c => a < b and b < c
(define (python-prim2 [op : symbol] [arg1 : CAns] [arg2 : CAns]) : CExp
  (let ([val-l (getObjVal (AVal-val arg1))]
        [val-r (getObjVal (AVal-val arg2))]
        [loc-l (getObjLoc (AVal-val arg1))]
        [loc-r (getObjLoc (AVal-val arg2))])
    (case op
      [(==) (if (equal? val-l val-r)
                ($to-object (CTrue))
                ($to-object (CFalse)))]
      [(!=) (if (equal? val-l val-r)
                ($to-object (CFalse))
                ($to-object (CTrue)))]
      [(is) (if (equal? loc-l loc-r)
                ($to-object (CTrue))
                ($to-object (CFalse)))]
      [(!is) (if (equal? loc-l loc-r)
                 ($to-object (CFalse))
                 ($to-object (CTrue)))]
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
                 [(>) (if (> (VNum-n val-l) (VNum-n val-r)) (CId 'True) (CId 'False))]
                 [(<=) (if (<= (VNum-n val-l) (VNum-n val-r)) (CId 'True) (CId 'False))]
                 [(>=) (if (>= (VNum-n val-l) (VNum-n val-r)) (CId 'True) (CId 'False))]
                 )]
              ;; STRING CASE
              [(and (VStr? val-l) (VStr? val-r))
               (case op
                 [(+) ($to-object (CStr (string-append (VStr-s val-l) (VStr-s val-r))))])]
              [else (error 'prim2 "no case yet")])]
    )))

;; get object value from an Ans
(define (getObjVal (obj : CVal)) : CVal
  (type-case CVal obj
    [VObject (type value loc flds) value]
    [else (error 'getObjVal "input not an object")]))

;; get object type from an Ans
(define (getObjType (obj : CVal)) : string
  (type-case CVal obj
    [VObject (type value loc flds) type]
    [else (error 'getObjType "input not an object")]))

;; get object loc from an Ans
(define (getObjLoc (obj : CVal)) : Location
  (type-case CVal obj
    [VObject (type value loc flds) loc]
    [else (error 'getObjLoc "input not an object")]))
