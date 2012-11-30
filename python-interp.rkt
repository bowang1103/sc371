#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "python-objects.rkt"
         "python-lib.rkt")

;; define the number to represent the level of different scope
;; 3 for global, 2 for nonlocal, 1 for local
(define local-level 3)
(define nonlocal-level 2)
(define global-level 1)

(define (getEmptyEnv) : Env
  (hash-set (hash-set (hash-set (hash empty) global-level (hash empty)) nonlocal-level (hash empty)) local-level (hash empty)))

(define (interp [expr : CExp]) : CVal
  (type-case CAns (interp-env expr (getEmptyEnv) (hash empty) (hash (list (values 1 (list)))))
    [AExc (excpt env sto lenv) (begin (print excpt) excpt)]  
    [AVal (value env sto lenv) value]))

(define (interp-env [expr : CExp] [env : Env] [store : Store] [lenv : LocalEnv]) : CAns
  (type-case CExp expr 
    [CNum (s) (AVal (VNum s) env store lenv)]
    [CStr (s) (AVal (VStr s) env store lenv)]
    [CException (type ms) (let ([msv (interp-env ms env store lenv)])
                            (let ([msv-str (getObjVal (AVal-val msv))])
                              (AVal (VException type msv-str) env store lenv)))]
    
    [CList (es) (letrec ([rst (interpArgs es env store lenv)]
                         [last (if (empty? rst)
                                   (AVal (VList empty) env store lenv)
                                   (first (reverse rst)))])
                  (if (AExc? last)
                      last
                      (AVal (VList (map (lambda (x) (if (isImmutable (VObject-type (AVal-val x)))
                                                        (AVal-val x)
                                                        (VObject "MPoint" (VMPoint (VObject-loc (AVal-val x))) -1 (hash empty))))
                                        rst)) 
                            (AVal-env last) (AVal-sto last) (AVal-lenv last))))]
    
    [CTuple (es) (letrec ([rst (interpArgs es env store lenv)]
                          [last (if (empty? rst)
                                    (AVal (VTuple empty) env store lenv)
                                    (first (reverse rst)))])
                   (if (AExc? last)
                       last
                       (AVal (VTuple (map (lambda (x) (if (isImmutable (VObject-type (AVal-val x)))
                                                          (AVal-val x)
                                                          (VObject "MPoint" (VMPoint (VObject-loc (AVal-val x))) -1 (hash empty))))
                                          rst)) 
                             (AVal-env last) (AVal-sto last) (AVal-lenv last))))]
    
    [CSetV (es) (letrec ([rst (interpArgs es env store lenv)]
                         [last (if (empty? rst)
                                   (AVal (VSet (hash empty)) env store lenv)
                                   (first (reverse rst)))])
                  (if (AExc? last)
                      last
                      ;; check whether all the elements in the set are immutable
                      (if (not (foldl (lambda (x result) (and (isRecurImmutable (AVal-val x) (AVal-sto last)) result)) true rst))
                          (interp-error "They are not hashable" env store lenv)
                          ;; build the set
                          (AVal (VSet (let ([keys (map (lambda (x) (getNoneObjectVal (AVal-val x) (AVal-sto last))) rst)])
                                        (foldl (lambda (x ht) (hash-set ht x true)) (hash empty) keys)))
                                (AVal-env last) (AVal-sto last) (AVal-lenv last)))))]
    [CDict (keys values) (if (equal? (length keys) (length values))
                             (letrec ([keyrst (interpArgs keys env store lenv)]
                                      [keylast (if (empty? keyrst)
                                                   (AVal (VEmpty) env store lenv)
                                                   (first (reverse keyrst)))])
                               (if (AExc? keylast)
                                   keylast
                                   ;; use the length to check whether all keys are recursive immutable
                                   ;; by filter if they are all immutable they length should be same
                                   (if (not (equal? (length (filter (lambda(x) (isRecurImmutable (AVal-val x) (AVal-sto keylast))) keyrst))
                                                    (length keyrst)))
                                       (interp-error "key should be immutable" env store lenv)
                                       (letrec ([valuesrst (interpArgs values (AVal-env keylast) (AVal-sto keylast) (AVal-lenv keylast))]
                                                [valueslast (if (empty? valuesrst)
                                                                (AVal (VEmpty) env store lenv)
                                                                (first (reverse valuesrst)))])
                                         (if (AExc? valueslast)
                                             valueslast
                                             (AVal (VDict (valfoldl2 (map AVal-val keyrst) (map AVal-val valuesrst) (hash empty) store))
                                                   (AVal-env valueslast) (AVal-sto valueslast) (AVal-lenv valueslast)))))))
                             (interp-error "length is not same" env store lenv))]
    
    [CRange (range) (letrec ([rangeList (interpArgs range env store lenv)]
                             [last (first (reverse rangeList))])
                      (if (AExc? last)
                          last
                          (letrec ([objList (map AVal-val rangeList)]
                                   [numList (map getObjVal objList)]
                                   [typeList (map getObjType objList)])
                            (if (foldl (lambda (el rst) (and el rst)) true (map (lambda (el) (or (equal? el "Int") (equal? el "Bool"))) typeList))
                                (if (= 0 (VNum-n (third numList)))
                                    (interp-env (raise-error "ValueError" "arg 3 must not be zero") (AVal-env last) (AVal-sto last) (AVal-lenv last))
                                    ;(interp-error "arg 3 must not be zero" (AVal-env last) (AVal-sto last) (AVal-lenv last))
                                    (let ([rangeValAns (interpArgs (getRangeList numList) (AVal-env last) (AVal-sto last) (AVal-lenv last))])
                                      (AVal (VRange (first objList) (second objList) (third objList)
                                                    (map AVal-val rangeValAns)) (AVal-env last) (AVal-sto last) (AVal-lenv last))))
                                (interp-env (raise-error "TypeError" "args for range should be integer type") (AVal-env last) (AVal-sto last) (AVal-lenv last))))))]
    
    [CIter (lst) (let ([listAns (interp-env lst env store lenv)])
                   (begin ;(display (to-string (AVal-val listAns)))
                   (type-case CAns listAns
                     [AVal (obj-lst obj-e obj-s obj-le)
                           (AVal (VIter 0 (VList-es (VObject-value obj-lst))) obj-e obj-s obj-le)]
                     [else listAns])))]
    
    [CCopy (obj) (AVal obj env store lenv)]
    [CTrue () (AVal (VTrue) env store lenv)]
    [CFalse () (AVal (VFalse) env store lenv)]
    [CEmpty () (AVal (VEmpty) env store lenv)]
    
    [CWrap (type obj) (case (string->symbol type)
                        [(List) (let ([rst (interp-env ($to-object (CList (list))) env store lenv)])
                                  (AVal (VObject type (type-case CVal (VObject-value obj)
                                                        [VStr (s) (VList (map (lambda(x) (AVal-val (interp-env ($to-object (CStr (list->string (list x)))) env store lenv))) 
                                                                              (string->list s)))]
                                                        [VList (es) (VList es)]
                                                        [VTuple (es) (VList es)]
                                                        [VDict (dict) (VList (map (lambda(x) (let ([t (AVal-val (interp-env ($to-object (valueToObjectCExp x)) env store lenv))])
                                                                                               (VObject (VObject-type t) x (VObject-loc t) (VObject-field t)))) (hash-keys dict)))]
                                                        [VRange (from to step es) (VList es)]
                                                        ;; TODO [VIter]
                                                        [else (VList (list (VEmpty)))]) (VObject-loc (AVal-val rst)) (VObject-field (AVal-val rst))) 
                                        (AVal-env rst) (AVal-sto rst) (AVal-lenv rst)))]
                        [(Tuple) (let ([rst (AVal-val (interp-env ($to-object (CTuple (list))) env store lenv))])
                                   (AVal (VObject type (type-case CVal (VObject-value obj)
                                                         [VStr (s) (VTuple (map (lambda(x) (AVal-val (interp-env ($to-object (CStr (list->string (list x)))) env store lenv))) 
                                                                                (string->list s)))]
                                                         [VList (es) (VTuple es)]
                                                         [VTuple (es) (VTuple es)]
                                                         [VDict (dict) (VTuple (map (lambda(x) (let ([t (AVal-val (interp-env ($to-object (valueToObjectCExp x)) env store lenv))])
                                                                                                 (VObject (VObject-type t) x (VObject-loc t) (VObject-field t)))) (hash-keys dict)))]
                                                         [else (VTuple (list (VEmpty)))]) (VObject-loc rst) (VObject-field rst)) env store lenv))]
                        [(Set) (let ([rst (interp-env ($to-object (CSetV (list))) env store lenv)])
                                 (if (not (isObjRecurImmutable obj store))
                                     (interp-error "It's not hashable" env store lenv)
                                     (AVal (VObject type (type-case CVal (VObject-value obj)
                                                       [VStr (s) (VSet (let ([keys (map (lambda(x) (VObject-value (AVal-val (interp-env ($to-object (CStr (list->string (list x)))) env store lenv)))) 
                                                                                        (string->list s))])
                                                                         (foldl (lambda (x ht) (hash-set ht x true)) (hash empty) keys)))]
                                                       [VList (es) (VSet (foldl (lambda (x ht) (hash-set ht x true)) (hash empty) (VList-es (getNoneObjectVal obj (AVal-sto rst)))))]
                                                       [VTuple (es) (VSet (foldl (lambda (x ht) (hash-set ht x true)) (hash empty) (VTuple-es (getNoneObjectVal obj (AVal-sto rst)))))]
                                                       [VSet (es) (VSet es)]
                                                       [VDict (dict) (VSet (foldl (lambda (x ht) (hash-set ht x true)) (hash empty) (hash-keys dict)))]
                                                       [else (VSet (hash empty))]) (VObject-loc (AVal-val rst)) (VObject-field (AVal-val rst))) env store lenv)))])]
    
    [CError (e) (let ([ans (interp-env e env store lenv)])
                  (AExc (AVal-val ans) (AVal-env ans) (AVal-sto ans) (AVal-lenv ans)))]
    
    [CIf (i t e) (let ([ians (interp-env i env store lenv)])
                   (type-case CAns ians
                     [AVal (v-i e-i s-i le-i)
                           (if (isObjTrue v-i)
                               (interp-env t e-i s-i le-i)
                               (interp-env e e-i s-i le-i))]
                     [else ians]))]
    
    [CId (id) (let ([rst (grabValue id env store lenv)])
                (begin #| (display "In CId : \n") (display (to-string id)) (display (to-string (hash-keys store))) (display "\n") |#
                  (type-case CAns rst
                    [AVal (v-v e-v s-v le-v) 
                          (let ([val (VObject-value v-v)])
                            (if (VPoint? val)
                                (interp-env (CGetfield (VPoint-obj val) (VPoint-field val)) e-v s-v le-v)
                                (if (VMPoint? val)
                                    (AVal (some-v (hash-ref store (VMPoint-loc val))) e-v s-v le-v)
                                    rst)))]
                    [else rst])))]
    
    [CLet (id bind body) (let ([bindAns (interp-env bind env store lenv)])
                                  (type-case CAns bindAns
                             [AVal (v-bind e-bind s-bind le-bind)
                                   (begin #| (display "In CLet: \n") (display (to-string id)) (display (to-string (hash-keys s-bind))) (display "\n") |#
                                     (let ([where (VObject-loc v-bind)])
                                       (interp-env body
                                                   (envSet id where e-bind le-bind)
                                                   (hash-set s-bind where v-bind)
                                                   lenv)))]
                                    [else bindAns]))]
    
    [CSet (id value) (let ([vans (interp-env value env store lenv)])
                       (type-case CAns vans
                         [AVal (v-v e-v s-v le-v) 
                               (let ([where (newLoc)])
                                 (if (isImmutable (VObject-type v-v))
                                     (AVal (VObject (VObject-type v-v) (VObject-value v-v) where (VObject-field v-v)) 
                                           (envSet id where e-v le-v)
                                           (begin #| (display "in CSet : ") (display (to-string id)) (display (to-string (hash-keys s-v))) |# (hash-set s-v where v-v))
                                           (add-lenv id le-v))
                                     (AVal v-v 
                                           (envSet id (VObject-loc v-v) e-v le-v)
                                           (hash-set s-v (VObject-loc v-v) v-v) (add-lenv id le-v))))]
                         ;                                     (type-case CExp value
                         ;                                       [CId (c-id) (AVal v-v (hash-set e-v id (some-v (hash-ref e-v c-id))) s-v le-v)]
                         ;                                       ;[CGetelement (obj indexs) (AVal v-v (hash-set e-v id (some-v (hash-ref e-v ))))]
                         ;                                       [CGetfield (c-obj c-fd) (AVal v-v (hash-set e-v id where)
                         ;                                                                     (hash-set s-v where (VPoint c-obj c-fd))
                         ;                                                                     (hash-set le-v id true))]
                         ;                                       [else (AVal v-v (hash-set e-v id (VObject-loc v-v))
                         ;                                           (hash-set s-v (VObject-loc v-v) v-v)
                         ;                                           (hash-set le-v id true))])))]
                         [else vans]))]
    
    [CDel (tg) (let ([tar (interp-env tg env store lenv)])
                 (type-case CAns tar
                   [AVal (v-v e-v s-v le-v) 
                         (type-case CExp tg
                           [CId (id) (AVal (AVal-val tar) (envRemove id env le-v) store (del-lenv id lenv))]
                           [CGetelement (obj indexs) (let ([v-o (AVal-val (interp-env obj env store lenv))])
                                                       (case (string->symbol (VObject-type v-o))
                                                         [(Dict) (interp-env (COperation obj "Dict" "pop" indexs) env store lenv)]))]
                           [else tar])]
                   [else tar]))]
    
    [CSetelement (obj index value) 
                 (let ([oval (interp-env obj env store lenv)] 
                       [ival (interp-env index env store lenv)]
                       [vval (interp-env value env store lenv)])
                   (if (and (AVal? oval) (and (AVal? ival) (AVal? vval)))
                       (if (or (and (equal? (VObject-type (AVal-val ival)) "Int") 
                                    (equal? (VObject-type (AVal-val oval)) "List"))
                               (equal? (VObject-type (AVal-val oval)) "Dict"))
                           (AVal (AVal-val vval)
                                 env
                                 (hash-set store 
                                           (VObject-loc (AVal-val oval))
                                           (cond
                                             [(equal? (VObject-type (AVal-val oval)) "List") 
                                              (VObject "List" 
                                                       (VList 
                                                        (map2 (lambda (x y) 
                                                                (if (equal? y (VNum-n (VObject-value (AVal-val ival))))
                                                                    (AVal-val vval)
                                                                    x)) 
                                                              (VList-es (VObject-value (AVal-val oval)))
                                                              (build-list (length (VList-es (VObject-value (AVal-val oval)))) (lambda(x) x))))
                                                       (VObject-loc (AVal-val oval))
                                                       (VObject-field (AVal-val oval)))]
                                             [(equal? (VObject-type (AVal-val oval)) "Dict")
                                              (VObject "Dict"
                                                       (VDict
                                                        (hash-set (VDict-dict (VObject-value (AVal-val oval))) 
                                                                  (VObject-value (AVal-val ival))
                                                                  (AVal-val vval)))
                                                       (VObject-loc (AVal-val oval))
                                                       (VObject-field (AVal-val oval)))])) lenv)
                           (if (equal? (VObject-type (AVal-val oval)) "Tuple") 
                               (interp-error "Tuple cannot be changed" env store lenv)
                               (interp-error "Index is not a number" env store lenv)))
                       (cond
                         [(AExc? oval) oval]
                         [(AExc? ival) ival]
                         [(AExc? vval) vval]
                         [else (interp-error "You should not been here" env store lenv)])))]
    
    [CGetelement (obj indexs) (let ([oval (interp-env obj env store lenv)])
                                (if (AVal? oval)
                                    (letrec ([ival (interpArgs indexs (AVal-env oval) (AVal-sto oval) (AVal-lenv oval))]
                                             [last (first (reverse ival))])
                                      (if (AVal? last)
                                          (if (equal? (length ival) 1)
                                              (if (and (equal? (VObject-type (AVal-val last)) "Int")
                                                       (not (equal? (VObject-type (AVal-val oval)) "Dict")) )
                                                  (let ([i (VNum-n (VObject-value (AVal-val last)))])
                                                    (case (string->symbol (VObject-type (AVal-val oval)))
                                                      [(Str) (let ([lst (string->list (VStr-s (VObject-value (AVal-val oval))))])
                                                               (if (or (>= i (length lst))
                                                                       (> (- 0 i) (length lst)))
                                                                   (interp-error "Index is out" env store lenv)
                                                                   (let ([rst (interp-env ($to-object (CStr "")) 
                                                                                          (AVal-env last) (AVal-sto last) (AVal-lenv last))])
                                                                     (AVal (VObject "Str" (VStr (list->string (list (if (>= i 0)
                                                                                                                        (list-ref lst i)
                                                                                                                        (list-ref lst (+ (length lst) i))))))
                                                                                    (VObject-loc (AVal-val rst)) (VObject-field (AVal-val rst)))
                                                                           (AVal-env rst) (AVal-sto rst) (AVal-lenv rst)))))]
                                                      [(List) (if (or (>= i (length (VList-es (VObject-value (AVal-val oval)))))
                                                                      (> (- 0 i) (length (VList-es (VObject-value (AVal-val oval))))))
                                                                  (interp-error "Index is out" env store lenv)
                                                                  (AVal (let ([getrst (getElement (VList-es (VObject-value (AVal-val oval)))
                                                                                                  (reverse (build-list (if (< i 0)
                                                                                                                           (+ (+ (length (VList-es (VObject-value (AVal-val oval)))) i) 1)
                                                                                                                           (+ i 1)) (lambda(x) x))))])
                                                                          (if (equal? (VObject-type getrst) "MPoint")
                                                                              (some-v (hash-ref (AVal-sto last) (VMPoint-loc (VObject-value getrst))))
                                                                              getrst)) 
                                                                        (AVal-env last) (AVal-sto last) (AVal-lenv last)))]
                                                      [(Tuple) (if (>= i (length (VTuple-es (VObject-value (AVal-val oval)))))
                                                                   (interp-error "Index is out" env store lenv)
                                                                   (AVal (let ([getrst (getElement (VTuple-es (VObject-value (AVal-val oval)))
                                                                                                   (reverse (build-list (if (< i 0)
                                                                                                                            (+ (+ (length (VList-es (VObject-value (AVal-val oval)))) i) 1)
                                                                                                                            (+ i 1)) (lambda(x) x))))])
                                                                           (if (equal? (VObject-type getrst) "MPoint")
                                                                               (some-v (hash-ref (AVal-sto last) (VMPoint-loc (VObject-value getrst))))
                                                                               getrst)) 
                                                                         (AVal-env last) (AVal-sto last) (AVal-lenv last)))]))
                                                  (if (and (equal? (VObject-type (AVal-val oval)) "Dict") 
                                                           (isImmutable (VObject-type (AVal-val last))))
                                                      (let ([valuerst (hash-ref (VDict-dict (VObject-value (AVal-val oval))) (getNoneObjectVal (AVal-val last) (AVal-sto last)))])
                                                        (if (none? valuerst)
                                                            (interp-error "The key is not existed" env store lenv)
                                                            (AVal (some-v valuerst) (AVal-env last) (AVal-sto last) (AVal-lenv last))))
                                                      (interp-error "Index type is wrong" env store lenv)))
                                              (if (and (or (equal? (VObject-type (AVal-val (first ival))) "Int")
                                                           (equal? (VObject-type (AVal-val (first ival))) "None"))
                                                       (or (equal? (VObject-type (AVal-val (second ival))) "Int")
                                                           (equal? (VObject-type (AVal-val (second ival))) "None"))
                                                       (or (equal? (VObject-type (AVal-val (third ival))) "Int")
                                                           (equal? (VObject-type (AVal-val (third ival))) "None"))
                                                       (or (equal? (VObject-type (AVal-val oval)) "List")
                                                           (equal? (VObject-type (AVal-val oval)) "Tuple")
                                                           (equal? (VObject-type (AVal-val oval)) "Str")))
                                                  (let ([f-i (if (equal? (VObject-type (AVal-val (first ival))) "Int")
                                                                 (appindex (VObject-value (AVal-val (first ival))) (AVal-val oval))
                                                                 (if (equal? (VObject-type (AVal-val (third ival))) "Int")
                                                                     (if (> (VNum-n (VObject-value (AVal-val (third ival)))) 0)
                                                                         (VNum 0)
                                                                         (VNum (type-case CVal (VObject-value (AVal-val oval))
                                                                                 [VList (es) (- (length es) 1)]
                                                                                 [VTuple (es) (- (length es) 1)]
                                                                                 [VStr (s) (- (length (string->list s)) 1)]
                                                                                 [else -1])))
                                                                     (VNum 0)))]
                                                        [m-i (if (equal? (VObject-type (AVal-val (second ival))) "Int")
                                                                 (appindexm (VObject-value (AVal-val (second ival))) (AVal-val oval))
                                                                 (if (equal? (VObject-type (AVal-val (third ival))) "Int")
                                                                     (if (> (VNum-n (VObject-value (AVal-val (third ival)))) 0)
                                                                         (VNum (type-case CVal (VObject-value (AVal-val oval))
                                                                                 [VList (es) (length es)]
                                                                                 [VTuple (es) (length es)]
                                                                                 [VStr (s) (length (string->list s))]
                                                                                 [else -1]))
                                                                         (VNum -1))
                                                                     (VNum (type-case CVal (VObject-value (AVal-val oval))
                                                                             [VList (es) (length es)]
                                                                             [VTuple (es) (length es)]
                                                                             [VStr (s) (length (string->list s))]
                                                                             [else -1]))))]
                                                        [s-i (if (equal? (VObject-type (AVal-val (third ival))) "Int")
                                                                 (VObject-value (AVal-val (third ival)))
                                                                 (VNum 1))])
                                                    (getallelements oval f-i m-i s-i))
                                                  (interp-error "Index with wrong type" env store lenv)))
                                          last))
                                    oval))]
    
    [CSeq (e1 e2) (let ([e1Ans (interp-env e1 env store lenv)])
                    (type-case CAns e1Ans
                      [AVal (v-e1 e-e1 s-e1 le-e1) (interp-env e2 e-e1 s-e1 le-e1)]
                      [else e1Ans]))]
    
    [CRet (ret) (let ([result (interp-env ret env store lenv)])
                  (type-case CAns result
                    [AVal (v-a e-a s-a l-a)
                          (let ([retVal (AExc (VRet v-a) e-a s-a l-a)]) ;; repackage to a AExc with a VRet inside
                            (type-case CExp ret 
                              [CId (x) (type-case CVal v-a
                                         [VObject (t-o v-o l-o f-o)
                                                  (type-case CVal v-o
                                                    [VClosure (args varargs defaults body e-v)
                                                              (AExc (VRet (VObject t-o v-o l-o f-o)) e-a (hash-set s-a e-v (VEnv (mergeNAndL e-a))) l-a)]
                                                    [else retVal])]
                                         [else retVal])]
                              [else retVal]))]
                    [else result]))]
    
    [CApp (fun args starargs) 
          (let ([funAns (interp-env fun env store lenv)])
                       ;; function answer
                       (type-case CAns funAns
                         [AVal (v-fobj e-fobj s-fobj le-fobj)
                               ;; function value
                               (begin
                                 #| (map (lambda (x) (begin (display (to-string x)) (display " : ") (display (to-string (some-v (hash-ref le-fobj x)))) (display "\n"))) (hash-keys le-fobj)) |#
                                 (type-case CVal (VObject-value v-fobj)
                                   [VClosure (clargs clvarargs cldfts clbody clenv)
                                             (letrec ([self (if (and (not (equal? clargs empty))
                                                                     (symbol=? 'self (first clargs)))
                                                                (list (interp-env (CId 'self) (hash-set (hash-set e-fobj nonlocal-level (VEnv-e (some-v (hash-ref s-fobj clenv)))) local-level (hash empty)) s-fobj le-fobj))
                                                                (list))]
                                                      [newLocList (if (empty? self)
                                                                      (allocLocList (length clargs))
                                                                      (cons (VObject-loc (AVal-val (first self))) (allocLocList (- (length clargs) 1))))]
                                                      [bind-es (bind-args clargs
                                                                          clvarargs
                                                                          (begin #|(display (to-string newLocList))|# newLocList)
                                                                          ;; interp arguments with closure environment
                                                                          (begin
                                                                            (append self (let ([test-val (interpArgs_Func 
                                                                                                          args 
                                                                                                          starargs 
                                                                                                          ;(hash-set (hash-set e-fobj nonlocal-level (VEnv-e (some-v (hash-ref s-fobj clenv)))) local-level (hash empty))  
                                                                                                          (begin #|
                                                                                                            (display "In CApp : \n")
                                                                                                            (display (to-string fun))
                                                                                                            (display "\n")
                                                                                                            (display "  global-level: ")
                                                                                                            (display (to-string (hash-keys (some-v (hash-ref env global-level)))))
                                                                                                            (display "\n  nonlocal-level: ")
                                                                                                            (display (to-string (hash-keys (some-v (hash-ref env nonlocal-level)))))
                                                                                                            (display "\n  local-level: ")
                                                                                                            (display (to-string (hash-keys (some-v (hash-ref env local-level))))) 
                                                                                                            (display "\n in clenv: ")
                                                                                                            (display (to-string (hash-keys (VEnv-e (some-v (hash-ref s-fobj clenv)))))) |#
                                                                                                            e-fobj)
                                                                                                          s-fobj
                                                                                                          le-fobj)])
                                                                                           (begin
                                                                                             test-val))))
                                                                          cldfts
                                                                          (hash-set (hash-set e-fobj nonlocal-level (VEnv-e (some-v (hash-ref s-fobj clenv)))) local-level (hash empty)) 
                                                                          s-fobj (hash-set le-fobj (+ 1 (getmaxnumber (hash-keys lenv))) (list)))]) ;; extend closure-env (if no arguments)
                                               (type-case CAns bind-es
                                                 [AVal (v-es e-es s-es le-es)
                                                       (begin #|
                                                         (display "The EnvAfterBind : \n")
                                                         (display "  global-level: ")
                                                         (display (to-string (hash-keys (some-v (hash-ref e-es global-level)))))
                                                         (display "\n  nonlocal-level: ")
                                                         (display (to-string (hash-keys (some-v (hash-ref e-es nonlocal-level)))))
                                                         (display "\n  local-level: ")
                                                         (display (to-string (hash-keys (some-v (hash-ref e-es local-level)))))
                                                         (display "\n Env in CApp")
                                                         (display (to-string (hash-keys s-es)))
                                                         (display "\n") |#
                                                         (type-case CAns (interp-env clbody e-es s-es le-es)
                                                           [AVal (v-clbody e-clbody s-clbody le-clbody) (AVal v-clbody env s-clbody lenv)]
                                                           [AExc (v-clbody e-clbody s-clbody le-clbody) 
                                                               (if (VRet? v-clbody)
                                                                   (AVal (VRet-ret v-clbody) env s-clbody lenv)
                                                                   (AExc v-clbody env s-clbody lenv))]))]
                                                 [AExc (v-es e-es s-es le-se) (AExc v-es env store lenv)]))]
                                   [else (interp-error "Not a function" e-fobj s-fobj le-fobj)]))]
                         [else funAns]))]
    
    [CFunc (args varargs defaults body) (let ([dftAns (interpArgs defaults env store lenv)])
                                          (let ([where (newLoc)])
                                            (cond [(empty? dftAns) (AVal (VClosure args varargs (list) body where) env (begin #| (display "The Location: ") (display (to-string where)) |#
                                                                                                                         (let ([tstven (hash-set store where (VEnv (mergeNAndL env)))])
                                                                                                                           (begin #| (display (to-string (hash-keys tstven))) |# tstven))) lenv)]
                                                  [else (let ([lastAns (first (reverse dftAns))])
                                                          (type-case CAns (first (reverse dftAns))
                                                            [AVal (v e s le) (AVal (VClosure args varargs (map AVal-val dftAns) body where) e (hash-set s where (VEnv (mergeNAndL env))) le)]
                                                            [else lastAns]))])))]
    
    [CPrim1 (prim arg) (let ([argAns (interp-env arg env store lenv)])
                         (type-case CAns argAns
                           [AVal (v-obj e-obj s-obj le-obj)                                                 
                                   (interp-env (python-prim1 prim argAns) e-obj s-obj le-obj)]
                           [else argAns]))]
    
    [CPrim2 (prim arg1 arg2) (let ([arg1Ans (interp-env arg1 env store lenv)])
                               (if (AVal? arg1Ans)
                                   (let ([arg2Ans (interp-env arg2 (AVal-env arg1Ans) (AVal-sto arg1Ans) (AVal-lenv arg1Ans))])
                                     (if (AVal? arg2Ans)
                                         (interp-env (python-prim2 prim arg1Ans arg2Ans) (AVal-env arg2Ans) (AVal-sto arg2Ans) (AVal-lenv arg2Ans))
                                         arg2Ans))
                                   arg1Ans))]
     
    ;;setfield for object 
    [CSetfield (obj fld value) (let ([objv (interp-env obj env store lenv)])
                                 (type-case CAns objv
                                   [AVal (v-obj e-obj s-obj le-obj)
                                         (let ([val (interp-env value e-obj s-obj le-obj)]) 
                                           (type-case CAns val
                                             [AVal (v-val e-val s-val le-val)
                                                   (cond 
                                                     [(not (VObject? v-obj)) (interp-error (string-append "Non-object in field update: " (pretty v-obj)) e-obj s-obj le-obj)]
                                                     [else (begin (hash-set! (VObject-field v-obj) fld v-val)
                                                                  (AVal v-obj e-val s-val le-val))])]
                                             [else val]))]
                                   [else objv]))]
    ;;getfield for object
    [CGetfield (obj fld) (let ([objv (interp-env obj env store lenv)])
                           (type-case CAns objv
                             [AVal (v-obj e-obj s-obj le-obj)
                                   (let ([result (hash-ref (VObject-field v-obj) fld)])
                                     (cond 
                                       [(none? result) (interp-error (string-append "Unbound identifier: " fld) e-obj s-obj le-obj)]
                                       [else (AVal (some-v result) e-obj s-obj le-obj)]))]
                             [else objv]))]
    ;; initialize the object
    [CObject (type prim exp)
             (let ([primVal (interp-env prim env store lenv)])
               (type-case CAns primVal
                 [AVal (v-pv e-pv s-pv le-pv)
                       (let ([rs (interp-env exp e-pv s-pv (hash-set le-pv (+ 1 (getmaxnumber (hash-keys le-pv))) (list)))]
                             [where (newLoc)])
                         (type-case CAns rs
                           [AVal (v-rs e-rs s-rs le-rs) 
                                 (begin #| (display "IN CObject: \n") (display (to-string (hash-keys s-rs))) |#
                                   (AVal (VObject type v-pv where
                                                  (let ([rst (make-hash empty)])
                                                    (begin (map (lambda (x) (hash-set! rst (symbol->string x) (AVal-val (grabValue x e-rs s-rs le-rs))))
                                                                (some-v (hash-ref le-rs (getmaxnumber (hash-keys le-rs))))) rst))) e-rs s-rs le-pv))]
                           [else rs]))]
                 [else primVal]))]
    
    ;; operations of different class
    [COperation (obj type op args)
                (let ([o-val (interp-env obj env store lenv)])
                  (type-case CAns o-val
                      [AVal (v-o e-o s-o le-o)
                            (case (string->symbol type)
                              [(Str) (case (string->symbol op)
                                       [(iter) (interp-env ($to-object (CIter (CWrap "List" v-o))) e-o s-o le-o)])]
                              [(List) (case (string->symbol op)
                                        [(iter) (interp-env ($to-object (CIter (CWrap "List" v-o))) e-o s-o le-o)])]
                              [(Range) (case (string->symbol op)
                                         [(iter) (interp-env ($to-object (CIter (CWrap "List" v-o))) e-o s-o le-o)])]
                              [(Tuple) (case (string->symbol op)
                                         [(iter) (interp-env ($to-object (CIter (CWrap "List" v-o))) e-o s-o le-o)])]
                              [(Iter) (case (string->symbol op)
                                        [(iter) o-val]
                                        [(next) (letrec ([iter (VObject-value v-o)]
                                                         [itat (VIter-at iter)]
                                                         [ites (VIter-es iter)])
                                                  (if (>= itat (length ites))
                                                      (interp-env (raise-error "StopIteration" "iterator end") e-o s-o le-o)
                                                      (let ([updateObj (VObject (VObject-type v-o) (VIter (add1 itat) ites) (VObject-loc v-o) (VObject-field v-o))])
                                                        (AVal (list-ref ites itat) e-o (hash-set s-o (VObject-loc v-o) updateObj) le-o))))])]
                              [(Dict) (case (string->symbol op)
                                        [(iter) (interp-env ($to-object (CIter (CWrap "List" v-o))) e-o s-o le-o)]
                                        [(clear) (let ([rst (VObject (VObject-type v-o) (VDict (hash empty)) (VObject-loc v-o) (VObject-field v-o))])
                                                   (AVal rst e-o (hash-set s-o (VObject-loc v-o) rst) le-o))]
                                        [(keys) (let ([rst (AVal-val (interp-env ($to-object (CSetV (list))) e-o s-o le-o))])
                                                  (AVal (VObject (VObject-type rst) 
                                                                 (VSet (foldl (lambda (x ht) (hash-set ht x true)) (hash empty) (hash-keys (VDict-dict (VObject-value v-o)))))
                                                                 (VObject-loc rst) (VObject-field rst)) e-o s-o le-o))]
                                        [(pop) (let ([index (AVal-val (interp-env (first args) e-o s-o le-o))])
                                                 (AVal (VEmpty) env (hash-set store (VObject-loc v-o) 
                                                                              (VObject (VObject-type v-o) 
                                                                                       (VDict (hash-remove (VDict-dict (VObject-value v-o)) (getNoneObjectVal index store))) 
                                                                                       (VObject-loc v-o)
                                                                                       (VObject-field v-o))) lenv))]
                                        [(values) (let ([rst (AVal-val (interp-env ($to-object (CList (list))) e-o s-o le-o))])
                                                    (AVal (VObject (VObject-type rst) (VList (map (lambda(x) (some-v (hash-ref (VDict-dict (VObject-value v-o)) x))) (hash-keys (VDict-dict (VObject-value v-o)))))
                                                                   (VObject-loc rst) (VObject-field rst)) e-o s-o le-o))]
                                        [(get) (let ([arg (interp-env (second args) e-o s-o le-o)])
                                                 (let ([rst (interp-env (CGetelement obj (list (first args))) e-o s-o le-o)])
                                                   (if (AVal? rst)
                                                       rst
                                                       (if (equal? (VObject-type (AVal-val arg)) "Empty")
                                                           (interp-env ($to-object (CEmpty)) (AExc-env rst) (AExc-sto rst) (AExc-lenv rst))
                                                           arg))))]
                                        [(items) (let ([rst (AVal-val (interp-env ($to-object (CList (list))) e-o s-o le-o))])
                                                   (interp-env (CWrap "Set" (VObject (VObject-type rst) 
                                                                                     (VList (map (lambda(x) (let ([tmptup (AVal-val (interp-env ($to-object (CTuple (list))) e-o s-o le-o))])
                                                                                                              (VObject (VObject-type tmptup) 
                                                                                                                       (VTuple (list (let ([newkey (AVal-val (interp-env 
                                                                                                                                                              ($to-object (valueToObjectCExp x))
                                                                                                                                                              e-o s-o le-o))])
                                                                                                                                       (VObject (VObject-type newkey) x (VObject-loc newkey) (VObject-field newkey))) 
                                                                                                                                     (some-v (hash-ref (VDict-dict (VObject-value v-o)) x))))
                                                                                                                       (VObject-loc tmptup)
                                                                                                                       (VObject-field tmptup)))) 
                                                                                                 (hash-keys (VDict-dict (VObject-value v-o))))) (VObject-loc rst) (VObject-field rst))) 
                                                               e-o s-o le-o))]
                                        [(update) (let ([arg (interp-env (first args) e-o s-o le-o)])
                                                    (type-case CAns arg
                                                      [AVal (v-a e-a s-a le-a)
                                                            (case (string->symbol (VObject-type v-a))
                                                              [(Dict) (let ([keys (hash-keys (VDict-dict (VObject-value v-a)))])
                                                                        (foldl (lambda (x rst)  
                                                                                 (interp-env (CSetelement (CId 'self)
                                                                                                          (let ([newkey (AVal-val (interp-env 
                                                                                                                                   ($to-object (valueToObjectCExp x))
                                                                                                                                   (AVal-env rst) (AVal-sto rst) (AVal-lenv rst)))])
                                                                                                            (CCopy (VObject (VObject-type newkey)
                                                                                                                            x
                                                                                                                            (VObject-loc newkey)
                                                                                                                            (VObject-field newkey)))) 
                                                                                                          (CCopy (some-v (hash-ref (VDict-dict (VObject-value v-a)) x))))
                                                                                             (AVal-env rst) (AVal-sto rst) (AVal-lenv rst))) o-val keys))]
                                                              [(None) o-val]
                                                              [else (interp-error "argument must be dict or empty" e-a s-a le-a)])]
                                                      [else arg]))])])]
                    [else o-val]))]

    [CTryExn (body hdlers els) (let ([bodyv (interp-env body env store lenv)])
                                 (type-case CAns bodyv
                                   ;; if no exception, interp "else" part
                                   [AVal (v-bv e-bv s-bv le-bv) (interp-env els e-bv s-bv le-bv)]
                                   ;; Exception, interp "Exception Handler" part
                                   ;; bind exception object to Env & Store
                                   [AExc (v-bv e-bv s-bv le-bv) 
                                         (let ([where (newLoc)])
                                           (let ([value (grabValue 'exception_symbol e-bv s-bv le-bv)])
                                             (let ([hdlersv (interp-env hdlers (envSet 'exception_symbol where e-bv le-bv) (hash-set s-bv where v-bv) le-bv)])
                                               (type-case CAns value
                                                 [AVal (v-ev e-ev s-ev le-ev) 
                                                       (type-case CAns hdlersv
                                                         ;; leave the Except body ; replace 'exception_symbol with previous exception in environment 
                                                         [AVal (v-hd e-hd s-hd le-hs) (AVal v-hd (envSet 'exception_symbol where e-hd le-hs) (hash-set s-hd where v-ev) le-hs)] 
                                                         [AExc (v-hd e-hd s-hd le-hs) (AExc v-hd e-hd s-hd le-hs)])]
                                                 [AExc (v-ev e-ev s-ev le-ev)
                                                       (type-case CAns hdlersv
                                                         ;; case : no exception sitting in the env
                                                         ;; leave the Except body ; remove 'exception_symbol in environment
                                                         [AVal (v-hd e-hd s-hd le-hs) (AVal v-hd (envRemove 'exception_symbol e-hd le-hs) (hash-remove s-hd where) le-hs)] 
                                                         [AExc (v-hd e-hd s-hd le-hs) (AExc v-hd e-hd s-hd le-hs)])]))))]))]
                                               
    
    
    [CTryFinally (b fb) (let ([bv (interp-env b env store lenv)])
                          ;;A finally clause is always executed before leaving the try statement, 
                          ;;whether an exception has occurred or not
                          (type-case CAns bv
                            [AVal (v-bv e-bv s-bv le-bv) (interp-env fb e-bv s-bv le-bv)]
                            [AExc (v-bv e-bv s-bv le-bv) 
                                  (let ([where (newLoc)])
                                    (let ([value (grabValue 'exception_symbol e-bv s-bv le-bv)])
                                      (let ([fbv (interp-env fb (envSet 'exception_symbol where e-bv le-bv) (hash-set s-bv where v-bv) le-bv)])
                                        (type-case CAns value
                                                     [AVal (v-ev e-ev s-ev le-ev) 
                                                           (type-case CAns fbv
                                                             ;; leave the Except body ; replace 'exception_symbol with previous exception in environment 
                                                             [AVal (v-hd e-hd s-hd le-hs) (AVal v-hd (envSet 'exception_symbol where e-hd le-hs) (hash-set s-hd where v-ev) le-hs)]
                                                             ;; continue pass down the exception
                                                             [AExc (v-hd e-hd s-hd le-hs) (AExc v-hd e-hd s-hd le-hs)])]
                                                     [AExc (v-ev e-ev s-ev le-ev)
                                                           (type-case CAns fbv
                                                             ;; case : no exception sitting in the env
                                                             ;; leave the Except body ; remove 'exception_symbol in environment
                                                             [AVal (v-hd e-hd s-hd le-hs) (AVal v-hd (envRemove 'exception_symbol e-hd le-hs) (hash-remove s-hd where) le-hs)]
                                                             ;; continue pass down the exception
                                                             [AExc (v-hd e-hd s-hd le-hs) (AExc v-hd e-hd s-hd le-hs)])]))))]))]
    
    [CExceptHandler (name body type) (let ([value (grabValue 'exception_symbol env store lenv)])
                                       (let ([ErrorType (VException-type (getObjVal (AVal-val value)))]
                                             [typev (if (not (CId? type))
                                                        (interp-env type env store lenv)
                                                        (interp-env (ContructExc type "") env store lenv))]
                                             [namev (interp-env name env store lenv)])
                                         ;; Two condition for entering the Except body 
                                         ;; (a) except "nothing" : (b) except "certain Exception"
                                         (if (or (begin ;(display "Type : ") 
                                                        ;(display (getObjVal (AVal-val typev)))
                                                        ;(display (equal? ErrorType (VException-type (getObjVal (AVal-val typev)))))
                                                        ;(display "\n")
                                                        (VEmpty? (getObjVal (AVal-val typev))))       
                                                 (begin ;(display "Type2 :")
                                                        ;(display (VException-type (getObjVal (AVal-val typev))))
                                                        ;(display (equal? ErrorType (VException-type (getObjVal (AVal-val typev)))))
                                                        ;(display "\n")
                                                        (equal? ErrorType (VException-type (getObjVal (AVal-val typev))))))
                                             (if (VEmpty? (getObjVal (AVal-val namev)))
                                                 (interp-env body env store lenv)
                                                 (let ([where (newLoc)])
                                                   (interp-env body
                                                               (envSet (string->symbol (VStr-s (getObjVal (AVal-val namev)))) where env lenv)
                                                               (hash-set store where (AVal-val value))
                                                               lenv)))
                                             ;;Didn't go inside the Except body
                                             (AVal (VEmpty) env store lenv))))]  
    
    
    ;;Haven't handle "cause" yet ;
    [CRaise (cause exc) 
            (let ([excv (interp-env exc env store lenv)])
              (if (VEmpty? (getObjVal (AVal-val excv)))
                  ;; handle case : raise with no argument 
                  (let ([value (grabValue 'exception_symbol env store lenv)])
                    (type-case CAns value
                      ;; take 'exception_symbol that sit on the enviroment
                      [AVal (v-v e-v s-v le-v) (AExc v-v e-v s-v le-v)]
                      ;; There's no 'exception_symbol sit on the env, raise runtimeError
                      [AExc (v-v e-v s-v le-v) 
                            (let ([runTimeError (interp-env (ContructExc (CId 'RuntimeError) "No active exception to reraise") e-v s-v le-v)])
                              (AExc (AVal-val runTimeError) e-v s-v le-v))]))
                  ;; hande case : raie with argument (i.e. raise TypeError("foo")
                  (AExc (AVal-val excv) (AVal-env excv) (AVal-sto excv) (AVal-lenv excv))))]
    
    
    
    [else (begin (display expr)
                 (error 'interp "no case"))]))


;; produece a range list for range object by given start num, end num and step num
(define (getRangeList (range : (listof CVal))) : (listof CExp)
  (map (lambda (el) ($to-object (CNum el))) (getNumRangeList (VNum-n (first range)) (VNum-n (second range)) (VNum-n (third range)))))

(define (getNumRangeList (from : number) (to : number) (step : number)) : (listof number)
  (if (or (and (> step 0) (> to from)) 
          (and (< step 0) (< to from)))
      (cons from (getNumRangeList (+ from step) to step))
      (list)))

;; add a symbol into current level
(define (add-lenv (val : symbol) (lenv : LocalEnv)) : LocalEnv
  (hash-set lenv (getmaxnumber (hash-keys lenv)) (append (some-v (hash-ref lenv (getmaxnumber (hash-keys lenv)))) (list val))))

;; delete a ymbol from current level
(define (del-lenv (val : symbol) (lenv : LocalEnv)) : LocalEnv
  (hash-set lenv (getmaxnumber (hash-keys lenv)) (del-symbol val (some-v (hash-ref lenv (getmaxnumber (hash-keys lenv)))))))

;; use recursion to del element in the list
(define (del-symbol (val : symbol) (l : (listof symbol))) : (listof symbol)
  (if (empty? l)
      empty
      (if (equal? val (first l))
          (del-symbol val (rest l))
          (cons val (del-symbol val (rest l))))))

;; find the maxnumber
(define (getmaxnumber (vals : (listof number))) : number
  (if (empty? vals)
      0
      (foldl (lambda (x result) (if (> x result) x result)) 0 vals)))

;; get the current level
(define (getLevel (lenv : LocalEnv)) : number
  (getmaxnumber (hash-keys lenv)))

;; check whether current level is global
(define (inGlobalLevel (lenv : LocalEnv)) : boolean
  (if (equal? 1 (getLevel lenv)) true false))

;(define (resetLocalEnv (oldEnv : LocalEnv)) :  LocalEnv
;  (foldl (lambda (x result) (hash-set result x false)) (hash empty) (hash-keys oldEnv)))

;(define (getModifiedVars (oldEnv : LocalEnv)) : (listof symbol)
;  (filter (lambda (x) (some-v (hash-ref oldEnv x))) (hash-keys oldEnv)))

; take in a list of CExp (func args) and interp them to a list of answer
(define (interpArgs_Func [args : (listof CExp)] [starargs : (listof CExp)] [env : Env] [sto : Store] [lenv : LocalEnv]) : (listof CAns)
  (cond
    [(and (empty? args)
          (not (empty? starargs))) ; there is an "*args" when applying function
     ;; Here is a work-around, passing global_env instead of closure_env
     (let ([starargV (interp-env (first starargs) env sto lenv)])
       (type-case CAns starargV
         [AVal (v-sa e-sa s-sa le-sa)
               (let ([starargAns (map (lambda (x) (AVal x e-sa s-sa le-sa)) (VTuple-es (getObjVal v-sa)))]) ; unwrap the tuple, make it to a list of CAns
                 starargAns)]             
         [else (cons starargV empty)]))]
    [(empty? args) empty]
    [else (let ([argAns (interp-env (first args) env sto lenv)]) 
            (type-case CAns argAns
              [AVal (v-first e-first s-first le-first)
                    (cons argAns (interpArgs_Func (rest args) starargs e-first s-first le-first))]
              ;; If not exist in closure env ; find in global env
              [else (cons argAns empty)]))]
              ))

(define (bind-args (args : (listof symbol)) (varargs : (listof symbol)) (locs : (listof Location)) (anss : (listof CAns)) (dfts : (listof CVal)) (env : Env) (sto : Store) (lenv : LocalEnv)) : CAns
  (cond [(and (empty? args) (empty? anss)) (AVal (VStr "dummy") env sto lenv)]  ;if it's empty, return the original env and store 
        [(and (not (empty? anss)) (AExc? (first (reverse anss)))) (first (reverse anss))] ; last anss has exception
        [(and (not (empty? varargs))  ; if func-def has "*agrs" arguments 
              (<= (length args) (length anss)))
             (let ([latest (if (empty? anss) (AVal (VStr "dummy") env sto lenv) (first (reverse anss)))])
               (begin ;(display "came in here")
                      ;(display "\n")
                      ;(display (length args)) (display "\n") (display (length anss)) (display "\n")
                      ;(display (length locs))
               (let ([new_locs (cons (newLoc) locs)])
                 (AVal (VStr "dummy")
                       (extendEnv (append args varargs) new_locs env lenv)
                       (overrideStore new_locs 
                                      (Varargs_CVal (length args) (map AVal-val anss) (list))
                                      (AVal-sto latest))
                       (foldl (lambda (x result) (add-lenv x result)) lenv args)))))]
        [(and (and (<= (length args) (+ (length anss) (length dfts))) (>= (length args) (length anss))) (empty? varargs))
         (let ([latest (if (empty? anss) (AVal (VStr "dummy") env sto lenv) (first (reverse anss)))])
           (AVal (VStr "dummy")
                 (extendEnv args locs env lenv) ;; Use the env with closure
                 (overrideStore locs
                                (append (map AVal-val anss)
                                        (lastNVals (- (length args) (length anss)) dfts (list)))
                                (AVal-sto latest))
                 (let ([tstlen (foldl (lambda (x result) (add-lenv x result)) lenv args)])
                   (begin #| (display (to-string tstlen)) |#
                          tstlen))))]
        [else (interp-env (raise-error "TypeError" "Arity mismatch") env sto lenv)]))
 ; case :
 ; Varargs (3,(1,2,3,4,5),() )  
 ; return : 1,2,3,(4,5)<-tuple  
(define (Varargs_CVal (n : number) (input : (listof CVal)) (output : (listof CVal))) : (listof CVal)
  (if (= 0 n)
      (append (reverse output) (list (VObject "Tuple" (VTuple input) -1 (hash empty))))  
      (Varargs_CVal (- n 1) (rest input) (cons (first input) output))))
               
(define (lastNVals (n : number) (input : (listof CVal)) (output : (listof CVal))) : (listof CVal)
  (let ([revs (reverse input)])
    (if (= 0 n)
        output
        (lastNVals (- n 1) (rest input) (cons (first revs) output)))))

#| (define (grabValue (for : symbol) (env : Env) (sto : Store) (lenv : LocalEnv)) : CAns
  (type-case (optionof Location) (hash-ref env for)
    [some (loc) 
          (type-case (optionof CVal) (hash-ref sto loc)
            [some (v) (AVal v env sto lenv)]
            [none () (interp-error "Unbound value" env sto lenv)])]
    ;; Didn't exist in the current env & sto ; look up the built in library
    [none () (interp-env (lookup_lib-funcs for lib-functions) env sto lenv)])) |#

;; grab the value from the store, however, due to the change of design, we have to
;; traverse from high level of env to lower level of env to find the location of
;; value
(define (grabValue (for : symbol) (env : Env) (sto : Store) (lenv : LocalEnv)) : CAns
  (begin #| (if (equal? for 'y)
             (begin          
               (display "The grabEnv : \n")
               (display "  global-level: ")
               (display (to-string (hash-keys (some-v (hash-ref env global-level)))))
               (display "\n  nonlocal-level: ")
               (display (to-string (hash-keys (some-v (hash-ref env nonlocal-level)))))
               (display "\n  local-level: ")
               (display (to-string (hash-keys (some-v (hash-ref env local-level)))))
               (display "\n"))
             (display "")) |#
         (let ([loc (findTheLevel for env local-level)])
           (if (equal? -1 loc)
               (interp-env (lookup_lib-funcs for lib-functions) env sto lenv)
               (type-case (optionof CVal) (hash-ref sto loc)
                 [some (v) (AVal v env sto lenv)]
                 [none () (interp-error "Unbound value" env sto lenv)])))))

(define (findTheLevel (for : symbol) (env : Env) (level : number)) : number
  (if (< level global-level)
      -1
      (type-case (optionof Location) (hash-ref (some-v (hash-ref env level)) for)
        [some(n) n]
        [none() (findTheLevel for env (- level 1))])))

;; due to the env is levelified, I have to update the hash-set
(define (envSet (id : symbol) (loc : Location) (env : Env) (lenv : LocalEnv)) : Env
  (begin 
    #|(display "Current level : ")
    (display (getmaxnumber (hash-keys lenv))) 
    (display "\n") |#
    (if (inGlobalLevel lenv)
        (let ([tstglenv (hash-set env global-level (hash-set (some-v (hash-ref env global-level)) id loc))])
          (begin #| (display "The id :\n") (display (to-string id)) (display "\n") |#
                 tstglenv))
        (let ([tstenv (hash-set env local-level (hash-set (some-v (hash-ref env local-level)) id loc))])
          (begin
            #| (display "The Env : ") (display (to-string id)) (display "\n")
            (display "  global-level: ")
            (display (to-string (hash-keys (some-v (hash-ref tstenv global-level)))))
            (display "\n  nonlocal-level: ")
            (display (to-string (hash-keys (some-v (hash-ref tstenv nonlocal-level)))))
            (display "\n  local-level: ")
            (display (to-string (hash-keys (some-v (hash-ref tstenv local-level)))))
            (display "\n") |#
            tstenv)))))

;; due to the env is levelified, I have to update the hash-remove
(define (envRemove (id : symbol) (env : Env) (lenv : LocalEnv)) : Env
  (if (inGlobalLevel lenv)
      (hash-set env global-level (hash-remove (some-v (hash-ref env global-level)) id))
      (hash-set env local-level (hash-remove (some-v (hash-ref env local-level)) id)))) 

;; merge nonlocal and local into one hash table
;; The most important thing is to bind self into the env
(define (mergeNAndL (env : Env)) : LevelEnv
  (let ([nenv (some-v (hash-ref env nonlocal-level))]
        [lenv (some-v (hash-ref env local-level))])
    (let ([newenv (foldl (lambda (x result) (hash-set result x (some-v (hash-ref lenv x)))) nenv (hash-keys lenv))])
      (if (none? (hash-ref (some-v (hash-ref env global-level)) 'self))
          newenv
          (hash-set newenv 'self (some-v (hash-ref (some-v (hash-ref env global-level)) 'self)))))))

;; get a new memory addr
(define newLoc
  (local ([define n (box 0)])
    (lambda () 
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

; take in a list of CExp (func args) and interp them to a list of answer
(define (interpArgs [args : (listof CExp)] [env : Env] [store : Store] [lenv : LocalEnv]) : (listof CAns)
  (cond
    [(empty? args) empty]
    [else (let ([argAns (interp-env (first args) env store lenv)])
            (type-case CAns argAns
              [AVal (v-first e-first s-first le-first)
                    (cons argAns (interpArgs (rest args) e-first s-first le-first))]
              [else (cons argAns empty)]))]))

              
(define (CValtoCAns [element : CVal] [env : Env] [sto : Store] [lenv : LocalEnv]) : CAns
  (AVal element env sto lenv))
               
;; return a list of location with size given by param num
(define (allocLocList (count : number)) : (listof Location)
  (cond
    [(= 0 count) empty]
    [else (cons (newLoc) (allocLocList (- count 1)))]))

;; extend env with list of args and locations
(define (extendEnv (args : (listof symbol)) (locs : (listof Location)) (env : Env) (lenv : LocalEnv)) : Env
  (cond
    [(and (not (empty? args)) (not (empty? locs)))
     (begin #|
       (display "extendEnv : ")
       (display (to-string args))
       (display " || ")
       (display (to-string locs))
       (display "\n") |#
       (extendEnv (rest args) (rest locs) 
                  (envSet (first args) (first locs) env lenv) lenv))]
    [else env]))

;; extend store with list of locations and answers
(define (overrideStore (locs : (listof Location)) (vals : (listof CVal)) (sto : Store)) : Store
  (cond 
    [(and (not (empty? locs)) (not (empty? vals)))
     (begin #|
       (display "overrideStore : ")
       (display (to-string locs))
       (display " || ")
       (display (to-string vals))
       (display "\n") |#
       (overrideStore (rest locs) (rest vals) 
                      (hash-set sto (first locs) (first vals))))]
    [else sto]))

(define (getElement (values : (listof CVal)) (n : (listof number))) : CVal
  (if (equal? (first n) 0)
      (first values)
      (getElement (rest values) (rest n))))

(define (appindex (index : CVal) (x : CVal)) : CVal
  (let ([n (if (VList? (VObject-value x))
               (length (VList-es (VObject-value x)))
               (if (VTuple? (VObject-value x)) 
                   (length (VTuple-es (VObject-value x)))
                   (length (string->list (VStr-s (VObject-value x))))))]
        [i (VNum-n index)])
    (cond
      [(< i 0) (if (> 0 (+ n i))
                   (VNum 0)
                   (VNum (+ n i)))]
      [(>= i 0) (if (< n i)
                    (VNum n)
                    (VNum i))])))

(define (appindexm (index : CVal) (x : CVal)) : CVal
  (let ([n (if (VList? (VObject-value x))
               (length (VList-es (VObject-value x)))
               (if (VTuple? (VObject-value x)) 
                   (length (VTuple-es (VObject-value x)))
                   (length (string->list (VStr-s (VObject-value x))))))]
        [i (VNum-n index)])
    (cond
      [(< i 0) (if (> 0 (+ n i))
                   (VNum -1)
                   (VNum (+ n i)))]
      [(>= i 0) (if (< n i)
                    (VNum n)
                    (VNum i))])))

(define (getallelements (o : CAns) (f-i : CVal) (m-i : CVal) (s-i : CVal)) : CAns
  (let ([type (VObject-type (AVal-val o))]
        [where (newLoc)])
    (let ([target (case (string->symbol type)
                    [(Str) (VObject type 
                                    (VStr (let ([rst (getcharelements (makeflaglist (VNum-n f-i) (VNum-n m-i) (VNum-n s-i) 
                                                                                    (length (string->list (VStr-s (VObject-value (AVal-val o))))))
                                                                      (string->list (VStr-s (VObject-value (AVal-val o)))))])
                                            (if (>= (VNum-n s-i) 0)
                                                (list->string rst)
                                                (list->string (reverse rst)))))  where (VObject-field (AVal-val o)))]
                    [(List) (VObject type 
                                     (VList 
                                      (let ([rst (getspecelements (makeflaglist (VNum-n f-i) (VNum-n m-i) (VNum-n s-i) 
                                                                                (length (VList-es (VObject-value (AVal-val o)))))
                                                                  (VList-es (VObject-value (AVal-val o))))])
                                        (if (>= (VNum-n s-i) 0)
                                            rst
                                            (reverse rst)))) where (VObject-field (AVal-val o)))]
                    [(Tuple) (VObject type 
                                      (VTuple 
                                       (let ([rst (getspecelements (makeflaglist (VNum-n f-i) (VNum-n m-i) (VNum-n s-i) 
                                                                                 (length (VList-es (VObject-value (AVal-val o)))))
                                                                   (VList-es (VObject-value (AVal-val o))))])
                                         (if (>= (VNum-n s-i) 0)
                                             rst
                                             (reverse rst)))) where (VObject-field (AVal-val o)))])])
      (AVal target (AVal-env o) (hash-set (AVal-sto o) where target) (AVal-lenv o)))))

(define (getcharelements (flag : (listof boolean)) (vals : (listof char))) : (listof char)
  (if (empty? flag)
      empty
      (if (first flag)
          (cons (first vals) (getcharelements (rest flag) (rest vals)))
          (getcharelements (rest flag) (rest vals)))))


(define (getspecelements (flag : (listof boolean)) (vals : (listof CVal))) : (listof CVal)
  (if (empty? flag)
      empty
      (if (first flag)
          (cons (first vals) (getspecelements (rest flag) (rest vals)))
          (getspecelements (rest flag) (rest vals)))))

(define (makeflaglist (f-i : number) (m-i : number) (s-i : number) (l : number)) : (listof boolean)
  (build-list l (lambda(x) (if (> s-i 0)
                               (cond 
                                 [(equal? x f-i) true]
                                 [(and (equal? 0 (pmod (- x f-i) s-i))
                                       (< x m-i)) true]
                                 [else false])
                               (cond
                                 [(equal? x f-i) true]
                                 [(and (equal? 0 (pmod (- f-i x) (- 0 s-i)))
                                       (> x m-i)) true]
                                 [else false])))))

(define (pmod (val : number) (div : number)) : number
  (if (< val div)
      val
      (mod (- val div) div)))

;; get built-in functions library
(define lib_functions_env : Env
  (let ([lib_functions (interp-env (python-lib (CEmpty)) (getEmptyEnv) (hash empty) (hash (list (values 1 (list)))))])
    (AVal-env lib_functions)))
  

#|;;Merge Environment : Add new_Env to old_Env , if both contains similar symobl new_Env replace old_Env 
(define (MergeEnv (new_Env : Env) (old_Env : Env) (lenv : LocalEnv)) : Env
  (foldl (lambda (x result) 
           (let ([location (if (and (inGlobal x lenv) (notInLocal x lenv))
                               (if (none? (hash-ref old_Env x))
                                   (some-v (hash-ref new_Env x))
                                   (some-v (hash-ref old_Env x)))
                               (some-v (hash-ref new_Env x)))])
             (hash-set result x location)))
         old_Env 
         (hash-keys new_Env)))|#

(define (inGlobal (x : symbol) (lenv : LocalEnv)) : boolean
  (findElement x (some-v (hash-ref lenv 1))))

(define (findElement (x : symbol) (l : (listof symbol))) : boolean
  (if (empty? l)
      false
      (if (equal? x (first l))
          true
          (findElement x (rest l)))))

(define (notInLocal (x : symbol) (lenv : LocalEnv)) : boolean
  (let ([level (getmaxnumber (hash-keys lenv))])
    (if (equal? level 1)
        true
        (not (foldl (lambda (z result) (or z result)) false 
                    (map (lambda (y) (findElement x (some-v (hash-ref lenv y)))) (rest (rest (build-list (+ 1 level) identity)))))))))
  
#| ;;Merge Store :  Add new_Sto to old_Sto , if both contains similar symobl new_Sto replace old_Sto 
(define (MergeSto (new_Sto : Store) (old_Sto : Store) (env : Env)) : Store
  (foldl (lambda (x result) (let ([loc (some-v (hash-ref env x))]) 
                              (if (none? (hash-ref old_Sto loc))
                                  (hash-set result loc (some-v (hash-ref new_Sto loc)))
                                  (hash-set result loc (some-v (hash-ref old_Sto loc)))))) old_Sto (hash-keys env)))

;;Merge lenv : Add new_lenv to old_lenv , if both contains similar symobl new_Sto replace old_Sto 
(define (MergeLenv (new_lenv : LocalEnv) (old_lenv : LocalEnv)) : LocalEnv
  (foldl (lambda (x result)
           (let ([value (some-v (hash-ref new_lenv x))])
             (hash-set result x value)))
         old_lenv
         (hash-keys new_lenv)))
|#
