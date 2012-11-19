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

(define curstore (hash empty))

(define (pretty [arg : CVal]) : string
  (type-case CVal arg
    [VNum (n) (to-string n)]
    [VStr (s) (foldr string-append "" (list "'" s "'"))]
    [VList (elms) (foldr string-append  ""
                    (list "[" 
                          (if (empty? elms) ""
                              (let ([ptvals (map pretty elms)])
                                (foldl (lambda (el rst) (string-append rst (string-append ", " el))) (first ptvals) (rest ptvals))))
                          "]"))]
    [VTuple (elms) (foldr string-append  ""
                    (list "("
                          (if (empty? elms) ""
                              (let ([ptvals (map pretty elms)])
                                (foldl (lambda (el rst) (string-append rst (string-append ", " el))) (first ptvals) (rest ptvals))))
                          ")"))]
    [VDict (dict) (letrec ([keys (hash-keys dict)]
                           [pair (map (lambda(x) 
                                       (foldr string-append ""
                                              (list (pretty x) 
                                                    ": " 
                                                    (pretty (some-v (hash-ref dict x)))))) keys)])
                    (foldr string-append "" 
                           (list "{" 
                                 (if (empty? pair) "" (foldl (lambda (el rst) (string-append rst (string-append ", " el))) (first pair) (rest pair)))
                                 "}")))]
    [VTrue () "True"]
    [VFalse () "False"]
    [VEmpty () ""]
    [VObject (type value loc flds) (cond
                                     [(equal? type "Int") (pretty value)]
                                     [(equal? type "Str") (pretty value)]
                                     [(equal? type "List") (pretty value)]
                                     [(equal? type "Tuple") (pretty value)]
                                     [(equal? type "Dict") (pretty value)]
                                     [(equal? type "MPoint") (pretty value)]
                                     [(equal? type "Empty") (pretty value)]
                                     [(equal? type "Bool") (if (equal? "1" (pretty value)) "True" "False")]
                                     [(equal? type "Exception") (pretty value)])]
    [VClosure (args defaults body) (error 'prim "Can't print closures yet")]
    [VPoint (name field) (error 'prim "VPoint")]
    [VMPoint (loc) (pretty (some-v (hash-ref curstore loc)))]
    [VException (type message) (string-append (string-append type ": ") (pretty message))]
    
    
    #|(cond [(VObject? excpt)
                 (let ([excv (getObjVal excpt)])
                   (begin (display (string-append (VException-type excv) ": "))
                          (display (string-append (VStr-s (VException-message excv)) "\n"))
                          excpt))]|#
    ))
			
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
      [VTuple (ls) (not (empty? ls))]
      [VDict (dict) (not (empty? (hash-keys dict)))]
      [VTrue () true]
      [VFalse () false]
      ;; TODO: all other implicit false
      [VEmpty () false]
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
    (if (VNum? pv) ($to-object (CNum (* 1.0 (if (= 0 (VNum-n pv)) 0.0 (VNum-n pv))))) (core-error "float input should be a numeric type"))))

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

(define (getLength (obj : CVal)) : CExp
  (type-case CVal (getObjVal obj)
    [VStr (s) ($to-object (CNum (length (string->list s))))]
    [VList (elms) ($to-object (CNum (length elms)))]
    [VTuple (elms) ($to-object (CNum (length elms)))]
    [VDict (dict) ($to-object (CNum (length (hash-keys dict))))]
    [else (core-error (foldr string-append "" (list "obj of " (getObjType obj) " has no len()")))]))

; unaryop = {~, not, pos, neg} ~must be int, pos and neg should be numeric, not ?
(define (python-prim1 [op : symbol] [arg : CAns]) : CExp
  (let ([obj (AVal-val arg)])
    (case op
      [(print) (begin (set! curstore (AVal-sto arg)) (print obj) (CStr "Print Return Value"))]
      [(callable) (if (equal? "Func" (VObject-type obj)) (CId 'True) (CId 'False))]
      [(bool) (if (isObjTrue obj) (CId 'True) (CId 'False))]
      [(not) (if (isObjTrue obj) (CId 'False) (CId 'True))]
      [(~) (invertNumeric obj)]
      [(neg) (negNumeric obj)]
      [(pos) (posNumeric obj)]
      [(int) (intNumeric obj)]
      [(float) (floatNumeric obj)]
      [(str) (toString obj)]
      [(abs) (absNumeric obj)]
      [(list) (CWrap "List" obj)]
      [(tuple) (CWrap "Tuple" obj)]
      ;[(dict)]
      
      [(tagof) ($to-object (CStr (getObjType obj)))]
      [(len) (getLength obj)])))

;;boolop may have to handle in other function
; boolop = {and, or}
; op = {+, -, *, /, %, **, <<, >>, bor, ^, band, //}
; compare op = {==, !=, <, <=, >, >=, is, !is, in, !in} a < b < c => a < b and b < c
(define (python-prim2 [op : symbol] [arg1 : CAns] [arg2 : CAns]) : CExp
  (let ([val-l (getNoneObjectVal (AVal-val arg1) (AVal-sto arg1))]
        [val-r (getNoneObjectVal (AVal-val arg2) (AVal-sto arg1))]
        [loc-l (getObjLoc (AVal-val arg1))]
        [loc-r (getObjLoc (AVal-val arg2))]
        [type-l (getObjType (AVal-val arg1))]
        [type-r (getObjType (AVal-val arg2))])
    (case op
      [(==) (if (equal? val-l val-r)
                (CId 'True)
                (CId 'False))]
      [(!=) (if (equal? val-l val-r)
                (CId 'False)
                (CId 'True))]
      [(is) (if (is2ObjSame type-l val-l loc-l type-r val-r loc-r)
                (CId 'True)
                (CId 'False))]
      [(!is) (if (is2ObjSame type-l val-l loc-l type-r val-r loc-r)
                 (CId 'False)
                 (CId 'True))]
      [(in) (if (isIn val-l val-r)
                (CId 'True)
                (CId 'False))]
      [(!in) (if (isIn val-l val-r)
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
                 [(/) (if (or (equal? (VNum-n val-r) 0)
                              (equal? (VNum-n val-r) 0.0))
                          ;;Two way of raising exception
                          (CRaise ($to-object (CEmpty)) ($to-object (CException "ZeroDivisionError" ($to-object (CStr "divison by zero")))))
                          ;(CRaise ($to-object (CEmpty)) (CApp (CId 'ZeroDivisionError) (list ($to-object (CStr "divison by zero")))))
                          ($to-object (CNum (/ (VNum-n val-l) (VNum-n val-r)))))]
                 [(//) (if (or (equal? (VNum-n val-r) 0)
                               (equal? (VNum-n val-r) 0.0))
                           (CRaise ($to-object (CEmpty)) ($to-object (CException "ZeroDivisionError" ($to-object (CStr "divison by zero")))))
                           ($to-object (CNum (floor (/ (VNum-n val-l) (VNum-n val-r))))))]
                 [(%) (if (or (equal? (VNum-n val-r) 0)
                              (equal? (VNum-n val-r) 0.0))
                          (CRaise ($to-object (CEmpty)) ($to-object (CException "ZeroDivisionError" ($to-object (CStr "divison by zero")))))
                          ($to-object (CNum (mod (VNum-n val-l) (VNum-n val-r)))))]
                 [(<) (if (< (VNum-n val-l) (VNum-n val-r)) (CId 'True) (CId 'False))]
                 [(>) (if (> (VNum-n val-l) (VNum-n val-r)) (CId 'True) (CId 'False))]
                 [(<=) (if (<= (VNum-n val-l) (VNum-n val-r)) (CId 'True) (CId 'False))]
                 [(>=) (if (>= (VNum-n val-l) (VNum-n val-r)) (CId 'True) (CId 'False))]
                 )]
              ;; STRING CASE
              [(and (VStr? val-l) (VStr? val-r))
               (case op
                 [(+) ($to-object (CStr (string-append (VStr-s val-l) (VStr-s val-r))))]
                 [(instanceof) (if (equal? val-l val-r)
                                   (CId 'True)
                                   (if (and (equal? (VStr-s val-l) "Bool") (equal? (VStr-s val-r) "Int")) (CId 'True) (CId 'False)))])]
              [else (error 'prim2 "no case yet")])]
    )))

;; see if two obj is same by comparing its loc or val according to mutability 
(define (is2ObjSame (type1 : string) (val1 : CVal) (loc1 : Location) (type2 : string) (val2 : CVal) (loc2 : Location)) : boolean
  (if (equal? type1 type2)
      (if (isImmutable type1)
          (equal? val1 val2)
          (equal? loc1 loc2))
      false))

(define (isIn (val1 : CVal) (val2 : CVal)) : boolean
  (cond
    [(and (VStr? val1) (VStr? val2)) (subString? (VStr-s val1) (VStr-s val2))] ; 2 strings
    [(VList? val2) (isInRecur val1 (VList-es val2))]
    [(VTuple? val2) (isInRecur val1 (VTuple-es val2))]
    [(VDict? val2) (isInRecur val1 (hash-keys (VDict-dict val2)))]
    [else (error 'isIn "val2 is not iterable")]))

(define (isInRecur (target : CVal) (all : (listof CVal))) : boolean
  (if (empty? all)
      false
      (if (equal? target (first all))
          true
          (isInRecur target (rest all)))))

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

;; get rid of object, return back pure prim val only
(define (getNoneObjectVal (obj : CVal) (store : Store)) : CVal
  (case (string->symbol (VObject-type obj))
    [(Int) (VObject-value obj)]
    [(Float) (VObject-value obj)]
    [(Str) (VObject-value obj)]
    [(Bool) (VObject-value obj)]
    [(List) (VList 
             (map2 getNoneObjectVal 
                   (VList-es (VObject-value obj)) 
                   (build-list (length (VList-es (VObject-value obj))) (lambda(x) store))))]
    [(Tuple) (VTuple (map2 getNoneObjectVal 
                           (VTuple-es (VObject-value obj)) 
                           (build-list (length (VTuple-es (VObject-value obj))) (lambda(x) store))))]
    [(Dict) (VObject-value obj)]
    [(True) (VTrue)]
    [(False) (VFalse)]
    [(MPoint) (getNoneObjectVal (some-v (hash-ref store (VMPoint-loc (VObject-value obj)))) store)]))

;; check whether the target string is in the original string
(define (subString? (target : string) (all : string)) : boolean
  (let ([st (string->list target)]
        [sa (string->list all)])
    (cond
      [(empty? st) true]
      [(empty? sa) false]
      [else (loopCheck st sa)])))

;; loop the string to find whether the target string is in
(define (loopCheck (target : (listof char)) (str : (listof char))) : boolean
  (let ([rst (findFirstAppear (first target) str)])
    (if (empty? rst)
      false
      (if (checkSame target rst)
          true
          (loopCheck target (rest rst))))))

;; find the first place of the str whose first element equals to the begin char
(define (findFirstAppear (begin : char) (str : (listof char))) : (listof char)
  (if (empty? str)
      empty
      (if (equal? begin (first str))
          str
          (findFirstAppear begin (rest str)))))

;; check the whether target is the first part of another string
(define (checkSame (target : (listof char)) (all : (listof char))) : boolean
  (if (> (length target) (length all))
      false
      (equal? target (build-list (length target) (lambda (x) (list-ref all x))))))
