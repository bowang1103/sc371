#lang plai-typed

(require "python-core-syntax.rkt"
         "python-objects.rkt"
         "python-ascii.rkt")

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
    [VSet (es) (let ([elms (hash-keys es)])
                 (if (empty? elms) "set()"
                     (foldr string-append  ""
                            (list "{"
                                  (let ([ptvals (map pretty elms)])
                                    (foldl (lambda (el rst) (string-append rst (string-append ", " el))) (first ptvals) (rest ptvals)))
                                  "}"))))]
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
    [VRange (to from step es) (let ([prettyRange (map pretty (list to from step))])
                                (foldr string-append "" 
                                       (list "range(" (first prettyRange) ", " (second prettyRange) 
                                             (if (equal? "1" (third prettyRange)) "" (string-append ", " (third prettyRange))) ")")))]
    [VIter (at es) "Iter Object"]
    [VTrue () "True"]
    [VFalse () "False"]
    [VEmpty () ""]
    [VObject (type value loc flds) (cond
                                     [(equal? type "Int") (pretty value)]
                                     [(equal? type "Float") (pretty value)]
                                     [(equal? type "Str") (pretty value)]
                                     [(equal? type "List") (pretty value)]
                                     [(equal? type "Tuple") (pretty value)]
                                     [(equal? type "Set") (pretty value)]
                                     [(equal? type "Dict") (pretty value)]
                                     [(equal? type "Range") (pretty value)]
                                     [(equal? type "Iter") (pretty value)]
                                     [(equal? type "MPoint") (pretty value)]
                                     [(equal? type "None") (pretty value)]
                                     [(equal? type "Bool") (if (equal? "1" (pretty value)) "True" "False")]
                                     [(equal? type "Exception") (pretty value)])]
    [VClosure (args varargs defaults body env) (error 'prim "Can't print closures yet")]
    [VPoint (name field) (error 'prim "VPoint")]
    [VMPoint (loc) (pretty (some-v (hash-ref curstore loc)))]
    [VEnv (e) "I haven't address"]
    [VException (type message) (string-append (string-append type ": ") (pretty message))]
    [VRet (ret) (pretty ret)]
    
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
      [VSet (es) (not (empty? (hash-keys es)))]
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
    [VSet (elms) ($to-object (CNum (length (hash-keys elms))))]
    [VDict (dict) ($to-object (CNum (length (hash-keys dict))))]
    [else (core-error (foldr string-append "" (list "obj of " (getObjType obj) " has no len()")))]))

(define (findMinOrMax (val : CVal) (findMin? : boolean)) : CExp
  (cond
    [(VStr? val) 
     (let ([chars (string->list (VStr-s val))])
       (if (empty? chars)
           (core-error "arg is an empty sequence")
           ($to-object (CStr (list->string (list (foldl 
                                                  (lambda (el rst) (if (< (some-v (hash-ref ascii el)) (some-v (hash-ref ascii rst)))
                                                                       (if findMin? el rst)
                                                                       (if findMin? rst el)))
                                                  (first chars) (rest chars))))))))]
    [(or (VList? val) (VTuple? val))
     (let ([elms (if (VList? val) (VList-es val) (VTuple-es val))])
       (if (empty? elms)
           (core-error "arg is an empty sequence")
           (let ([elm (foldl 
                       (lambda (el rst) 
                         (if (VEmpty? rst) rst
                             (cond
                               [(and (VNum? el) (VNum? rst))
                                (if (< (VNum-n el) (VNum-n rst))
                                    (if findMin? el rst)
                                    (if findMin? rst el))]
                               [(and (VStr? el) (VStr? rst))
                                (if (= -1 (strCompare (VStr-s el) (VStr-s rst)))
                                    (if findMin? el rst)
                                    (if findMin? rst el))]
                               [else (VEmpty)])))
                       (first elms) (rest elms))])
             (cond
               [(VNum? elm)
                ($to-object (CNum (VNum-n elm)))]
               [(VStr? elm)
                ($to-object (CStr (VStr-s elm)))]
               [else (core-error "elements in sequence not the same type")]))))]
    ;[VTuple (elms)]
    ;[VSet (elms)]
    [else (core-error "Input not a sequence object")]))

; unaryop = {~, not, pos, neg} ~must be int, pos and neg should be numeric, not ?
(define (python-prim1 [op : symbol] [arg : CAns]) : CExp
  (let ([obj (AVal-val arg)])
    (case op
      [(print) (begin  (set! curstore (AVal-sto arg)) (print obj) ($to-object (CStr "Print Return Value")))]
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
      [(min) (findMinOrMax (getNoneObjectVal obj (AVal-sto arg)) true)]
      [(max) (findMinOrMax (getNoneObjectVal obj (AVal-sto arg)) false)]
      [(list) (CWrap "List" obj)]
      [(tuple) (CWrap "Tuple" obj)]
      [(set) (CWrap "Set" obj)]
      ;[(dict)]
      
      [(tagof) ($to-object (CStr (getObjType obj)))]
      [(len) (getLength obj)])))

;;boolop may have to handle in other function
; boolop = {and, or}
; op = {+, -, *, /, %, **, <<, >>, bor, bxor, band, //}
; compare op = {==, !=, <, <=, >, >=, is, !is, in, !in} a < b < c => a < b and b < c
(define (python-prim2 [op : symbol] [arg1 : CAns] [arg2 : CAns]) : CExp
  (let ([clean-val-l (getNoneObjectVal (AVal-val arg1) (AVal-sto arg2))]
        [clean-val-r (getNoneObjectVal (AVal-val arg2) (AVal-sto arg2))]
        [val-l (getObjVal (AVal-val arg1))]
        [val-r (getObjVal (AVal-val arg2))]
        [loc-l (getObjLoc (AVal-val arg1))]
        [loc-r (getObjLoc (AVal-val arg2))]
        [type-l (getObjType (AVal-val arg1))]
        [type-r (getObjType (AVal-val arg2))])
    
    (case op
      [(==) (if (equal? clean-val-l clean-val-r)
                (CId 'True)
                (CId 'False))]
      [(!=) (if (equal? clean-val-l clean-val-r)
                (CId 'False)
                (CId 'True))]
      [(is) (if (is2ObjSame type-l clean-val-l loc-l type-r clean-val-r loc-r)
                (CId 'True)
                (CId 'False))]
      [(!is) (if (is2ObjSame type-l clean-val-l loc-l type-r clean-val-r loc-r)
                 (CId 'False)
                 (CId 'True))]
      [(in) (if (isIn clean-val-l clean-val-r)
                (CId 'True)
                (CId 'False))]
      [(!in) (if (isIn clean-val-l clean-val-r)
                 (CId 'False)
                 (CId 'True))]
      
      ;; TODO: add all other cases
      [else (cond 
              ;; NUMBER CASE (and BOOL)
              [(and (VNum? clean-val-l) (VNum? clean-val-r))
               (case op
                 [(+) ($to-object (CNum (+ (VNum-n clean-val-l) (VNum-n clean-val-r))))]
                 [(-) ($to-object (CNum (- (VNum-n clean-val-l) (VNum-n clean-val-r))))]
                 [(*) ($to-object (CNum (* (VNum-n clean-val-l) (VNum-n clean-val-r))))]
                 [(/) (if (or (equal? (VNum-n clean-val-r) 0)
                              (equal? (VNum-n clean-val-r) 0.0))
                          ;;Two way of raising exception
                          (raise-error "ZeroDivisionError" "division by zero")
                          ;(CRaise ($to-object (CEmpty)) ($to-object (CException "ZeroDivisionError" ($to-object (CStr "divison by zero")))))
                          ;(CRaise ($to-object (CEmpty)) (CApp (CId 'ZeroDivisionError) (list ($to-object (CStr "divison by zero")))))
                          ($to-object (CNum (/ (VNum-n clean-val-l) (VNum-n clean-val-r)))))]
                 [(//) (if (or (equal? (VNum-n clean-val-r) 0)
                               (equal? (VNum-n clean-val-r) 0.0))
                           (raise-error "ZeroDivisionError" "division by zero")
                           ;(CRaise ($to-object (CEmpty)) ($to-object (CException "ZeroDivisionError" ($to-object (CStr "divison by zero")))))
                           ($to-object (CNum (floor (/ (VNum-n clean-val-l) (VNum-n clean-val-r))))))]
                 [(%) (if (or (equal? (VNum-n clean-val-r) 0)
                              (equal? (VNum-n clean-val-r) 0.0))
                          (raise-error "ZeroDivisionError" "division by zero")
                          ;(CRaise ($to-object (CEmpty)) ($to-object (CException "ZeroDivisionError" ($to-object (CStr "divison by zero")))))
                          ($to-object (CNum (mod (VNum-n clean-val-l) (VNum-n clean-val-r)))))]
                 [(<) (if (< (VNum-n clean-val-l) (VNum-n clean-val-r)) (CId 'True) (CId 'False))]
                 [(>) (if (> (VNum-n clean-val-l) (VNum-n clean-val-r)) (CId 'True) (CId 'False))]
                 [(<=) (if (<= (VNum-n clean-val-l) (VNum-n clean-val-r)) (CId 'True) (CId 'False))]
                 [(>=) (if (>= (VNum-n clean-val-l) (VNum-n clean-val-r)) (CId 'True) (CId 'False))]
                 )]
              [(VNum? val-l)
               (if (equal? "Int" type-l)
                   (case op
                     [(*) (cond
                            [(VStr? val-r) (sequenceConcat "Str" (build-list (VNum-n val-l) (lambda (n) val-r)))]
                            [(VTuple? val-r) (sequenceConcat "Tuple" (build-list (VNum-n val-l) (lambda (n) val-r)))]
                            [(VList? val-r) (sequenceConcat "List" (build-list (VNum-n val-l) (lambda (n) val-r)))])])
                   (core-error "cannot multiply sequence by a non-int"))]
              ;; STRING CASE
              [(and (VStr? clean-val-l) (VStr? clean-val-r))
               (case op
                 [(+) (sequenceConcat "Str" (list clean-val-l clean-val-r))]
                 [(<) (if (= -1 (sequenceCompare clean-val-l clean-val-r)) (CId 'True) (CId 'False))]
                 [(>) (if (= 1 (sequenceCompare clean-val-l clean-val-r)) (CId 'True) (CId 'False))]
                 [(<=) (if (or (= 0 (sequenceCompare clean-val-l clean-val-r))
                               (= -1 (sequenceCompare clean-val-l clean-val-r))) (CId 'True) (CId 'False))]
                 [(>=) (if (or (= 0 (sequenceCompare clean-val-l clean-val-r))
                               (= 1 (sequenceCompare clean-val-l clean-val-r))) (CId 'True) (CId 'False))]
                 [(instanceof) (if (equal? clean-val-l clean-val-r)
                                   (CId 'True)
                                   (if (and (equal? (VStr-s clean-val-l) "Bool") (equal? (VStr-s clean-val-r) "Int")) (CId 'True) (CId 'False)))])]
              [(VStr? clean-val-l)
               (case op
                 [(*) (if (equal? "Int" type-r)
                          (sequenceConcat "Str" (build-list (VNum-n clean-val-r) (lambda (n) clean-val-l)))
                          (core-error "cannot multiply sequence by a non-int"))]
                 )]
              ;; TUPLE CASE
              [(VTuple? val-l)
               (case op
                 [(+) (if (VTuple? val-r)
                          (sequenceConcat "Tuple" (list val-l val-r))
                          (core-error "cannot + a non tuple object to a tuple"))]
                 [(*) (if (equal? "Int" type-r)
                          (sequenceConcat "Tuple" (build-list (VNum-n val-r) (lambda (n) val-l)))
                          (core-error "cannot multiply sequence by a non-int"))])]
              
              ;; LIST CASE
              [(VList? val-l)
               (case op
                 [(+) (if (VList? val-r)
                          (sequenceConcat "List" (list val-l val-r))
                          (core-error "cannot + a non list object to a list"))]
                 [(*) (if (equal? "Int" type-r)
                          (sequenceConcat "List" (build-list (VNum-n val-r) (lambda (n) val-l)))
                          (core-error "cannot multiply sequence by a non-int"))])]
              
              ;; SET CASE
              [(VSet? clean-val-l)
               (if (VSet? clean-val-r)
                   (let ([lh (VSet-es clean-val-l)]
                         [rh (VSet-es clean-val-r)])
                     (case op
                       [(-) (CWrap "Set" (VObject "Set" 
                              (VSet (foldl (lambda (key ht) (hash-remove ht key)) lh (hash-keys rh))) 
                              -1 (hash empty)))]
                       [(band) (CWrap "Set" (VObject "Set" 
                                 (VSet (foldl 
                                        (lambda (key rst) (if (some? (hash-ref lh key)) (hash-set rst key true) rst))
                                        (hash empty) (hash-keys rh)))
                                 -1 (hash empty)))]
                       [(bor) (CWrap "Set" (VObject "Set" 
                                 (VSet (foldl (lambda (key ht) (hash-set ht key true)) lh (hash-keys rh)))
                                 -1 (hash empty)))]
                       [(bxor) (CWrap "Set" (VObject "Set" 
                                 (VSet (foldl 
                                        (lambda (key rst) (if (some? (hash-ref lh key)) (hash-remove rst key) (hash-set rst key true)))
                                        lh (hash-keys rh)))
                                 -1 (hash empty)))]))
                   (core-error (foldr string-append "" (list "cannot " (symbol->string op) " by a non-set"))))]
               
              
              
              [else (error 'prim2 "no case yet")])]
    )))

;; see if two obj is same by comparing its loc or val according to mutability 
(define (is2ObjSame (type1 : string) (val1 : CVal) (loc1 : Location) (type2 : string) (val2 : CVal) (loc2 : Location)) : boolean
  (if (equal? type1 type2)
      (if (isImmutable type1)
          (equal? val1 val2)
          (equal? loc1 loc2))
      false))

;; check if a value is in a collection
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

;; concat a list of sequence together
(define (sequenceConcat (type : string) (seqs : (listof CVal))) : CExp
  (case (string->symbol type)
    [(Str) ($to-object (CStr 
                        (if (empty? seqs)
                            ""
                            (foldl (lambda (el rst) (string-append rst (VStr-s el))) (VStr-s (first seqs)) (rest seqs)))))]
    [(List) (CWrap "List" (VObject "List" (VList 
                                           (if (empty? seqs)
                                               (list)
                                               (foldl (lambda (el rst) (append rst (VList-es el))) (VList-es (first seqs)) (rest seqs))))
                                           -1 (hash empty)))]
    [(Tuple) (CWrap "Tuple" (VObject "Tuple" (VTuple 
                                              (if (empty? seqs)
                                                  (list)
                                                  (foldl (lambda (el rst) (append rst (VTuple-es el))) (VTuple-es (first seqs)) (rest seqs))))
                                              -1 (hash empty)))]
    [else (core-error "input is not a sequence")]))

;; compare two sequence objects, return 0 if =, 1 if >, -1 if <
(define (sequenceCompare (val1 : CVal) (val2 : CVal)) : number
  (cond 
    [(and (VNum? val1) (VNum? val2)) (if (= (VNum-n val1) (VNum-n val2)) 0 (if (> (VNum-n val1) (VNum-n val2)) 1 -1))]
    [(and (VStr? val1) (VStr? val2)) (strCompare (VStr-s val1) (VStr-s val2))]
    ;[(and (VList? val1) (VList? val2))]
    ;[(and (VTuple? val1) (VTuple? val2))]
    ;[(and (VSet? val1) (VSet? val2))]
    [else -2]))

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
    [(List) (VList (map2 getNoneObjectVal 
                   (VList-es (VObject-value obj)) 
                   (build-list (length (VList-es (VObject-value obj))) (lambda(x) store))))]
    [(Tuple) (VTuple (map2 getNoneObjectVal 
                           (VTuple-es (VObject-value obj)) 
                           (build-list (length (VTuple-es (VObject-value obj))) (lambda(x) store))))]
    [(Dict) (let ([dict (VDict-dict (VObject-value obj))])
              (VDict (cleanDictFoldl2 
                      (hash-keys dict) 
                      (map (lambda (key) (getNoneObjectVal (some-v (hash-ref dict key)) store)) (hash-keys dict))
                      (hash empty))))]
    [(Range) (VObject-value obj)]
    [(Iter) (VObject-value obj)]
    [(Set) (getObjVal obj)]
    [(True) (VTrue)]
    [(False) (VFalse)]
    [(MPoint) (getNoneObjectVal (some-v (hash-ref store (VMPoint-loc (VObject-value obj)))) store)]
    [(Exception) (VObject-value obj)]
    [(None) (VEmpty)]))

;; create a hash table for dictionary given clean keys and celan vals
(define (cleanDictFoldl2 (cleanKeys : (listof CVal)) (cleanVals : (listof CVal)) (ht : (hashof CVal CVal))) : (hashof CVal CVal)
  (if (empty? cleanKeys)
      ht
      (cleanDictFoldl2 (rest cleanKeys) (rest cleanVals) (hash-set ht (first cleanKeys) (first cleanVals)))))

;; specific function to app values into hash table
(define (valfoldl2 (keys : (listof CVal)) (values : (listof CVal)) (h : (hashof CVal CVal)) (store : Store)) : (hashof CVal CVal)
  (if (empty? keys)
      h            
      (valfoldl2 (rest keys) (rest values) (hash-set h (getNoneObjectVal (first keys) store) 
                                                     (if (isImmutable (VObject-type (first values)))
                                                         (first values)
                                                         (VObject "MPoint" (VMPoint (VObject-loc (first values))) -1 (hash empty)))) store)))

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

;; transfer the value to a CExp
(define (valueToObjectCExp (val : CVal)) : CExp
  (type-case CVal val
    [VNum (n) (CNum -1)]
    [VStr (s) (CStr "")]
    [VList (es) (CList (list))]
    [VTuple (es) (CTuple (list))]
    [VSet (es) (CSetV (list))]
    [else (CEmpty)]))

;; the API to call isRecurImmutable
(define (isObjRecurImmutable (obj : CVal) (store : Store)) : boolean
  (if (and (isImmutable (VObject-type obj))
           (not (equal? (VObject-type obj) "Tuple")))
      true
      (type-case CVal (VObject-value obj)
        [VTuple (es) (foldl (lambda (x result) (and (isRecurImmutable x store) result)) true es)]
        [VList (es) (foldl (lambda (x result) (and (isRecurImmutable x store) result)) true es)]
        [VSet (es) true]
        [VMPoint (loc) (isRecurImmutable (some-v (hash-ref store loc)) store)]
        [else false])))

;; check whether all the elements can reach from the object is immutable
(define (isRecurImmutable (obj : CVal) (store : Store)) : boolean
  (if (not (isImmutable (VObject-type obj)))
      false
      (type-case CVal (VObject-value obj)
        [VTuple (es) (foldl (lambda (x result) (and (isRecurImmutable x store) result)) true es)]
        ;; if it's a pointer get the value from the store and call the function again
        [VMPoint (loc) (isRecurImmutable (some-v (hash-ref store loc)) store)]
        [else true])))

(define (raise-error error-type msg)
  (CRaise ($to-object (CEmpty)) ($to-object (CException error-type ($to-object (CStr msg))))))
