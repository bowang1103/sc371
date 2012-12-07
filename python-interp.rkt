#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "python-objects.rkt"
         "python-lib.rkt")

;; define the number to represent the level of different scope
;; 3 for global, 2 for nonlocal, 1 for local
(define scan_var 4)
(define local-level 3)
(define nonlocal-level 2)
(define global-level 1)
(define unbound-local-val-loc -404)

(define (getEmptyEnv) : Env
  (hash-set (hash-set (hash-set (hash-set (hash empty) global-level (hash empty)) nonlocal-level (hash empty)) local-level (hash empty)) scan_var (hash empty)))

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
                   (type-case CAns listAns
                     [AVal (obj-lst obj-e obj-s obj-le)
                           (AVal (VIter 0 (VList-es (VObject-value obj-lst))) obj-e obj-s obj-le)]
                     [else listAns]))]
    
    [CCalIter (call stn) (let ([stnAns (interp-env stn env store lenv)])
                           (type-case CAns stnAns
                             [AVal (stn-obj stn-e stn-s stn-le)
                                   (AVal (VCalIter false call (getNoneObjectVal stn-obj store)) stn-e stn-s stn-le)]
                             [else stnAns]))]
    [CFilter (iter filter) (let ([iterAns (interp-env iter env store lenv)])
                             (type-case CAns iterAns
                               [AVal (it-obj it-e it-s it-le)
                                     (let ([filterAns (interp-env filter it-e it-s it-le)])
                                       (type-case CAns filterAns
                                         [AVal (fil-obj fil-e fil-s fil-le)
                                               (AVal (VFilter (VObject-loc it-obj) (VObject-loc fil-obj)) fil-e fil-s fil-le)]
                                         [else filterAns]))]
                               [else iterAns]))]
    
    [CCopy (obj) (AVal obj env store lenv)]
    [CTrue () (AVal (VTrue) env store lenv)]
    [CFalse () (AVal (VFalse) env store lenv)]
    [CEmpty () (AVal (VEmpty) env store lenv)]
    
    [CWrap (type obj) (case (string->symbol type)
                        [(List) (letrec ([rst (interp-env ($to-object (CList (list))) env store lenv)]
                                         [rstSto (AVal-sto rst)])
                                  (AVal (VObject type (type-case CVal (VObject-value obj)
                                                        [VStr (s) (VList (map (lambda(x) (AVal-val (interp-env ($to-object (CStr (list->string (list x)))) env store lenv))) 
                                                                              (string->list s)))]
                                                        [VList (es) (VList es)]
                                                        [VTuple (es) (VList es)]
                                                        [VDict (dict) (VList (map (lambda(x) (let ([t (AVal-val (interp-env ($to-object (valueToObjectCExp x)) env store lenv))])
                                                                                               (VObject (VObject-type t) x (VObject-loc t) (VObject-field t)))) (hash-keys dict)))]
                                                        [VRange (from to step es) (VList es)]
                                                        [VSet (es) (VList (map (lambda(x) (let ([t (AVal-val (interp-env ($to-object (valueToObjectCExp x)) env store lenv))])
                                                                                            (VObject (VObject-type t) x (VObject-loc t) (VObject-field t)))) (hash-keys es)))]
                                                        [VIter (at es) (begin
                                                                         (let ([updateIter (VObject "Iter" (VIter (length es) es) (VObject-loc obj) (VObject-field obj))])
                                                                           (set! rstSto (hash-set rstSto (VObject-loc obj) updateIter)))
                                                                         (VList (foldr (lambda (index rst) (cons (list-ref es index) rst))
                                                                                       (list) (build-list (- (length es) at) (lambda(x) (+ at x))))))]
                                                        [VCalIter (stop call stn)
                                                                  (let ([lstAns (iter2List (CCopy (some-v (hash-ref (VObject-field obj) "__next__"))) env rstSto lenv)]
                                                                        [updateIter (VObject "CalIter" (VCalIter true call stn) (VObject-loc obj) (VObject-field obj))])
                                                                    (begin (set! rstSto 
                                                                                 (hash-set (if (empty? lstAns) rstSto (AVal-sto (first (reverse lstAns))))
                                                                                           (VObject-loc obj) updateIter))
                                                                           (VList (foldr (lambda (ans rst) (cons (AVal-val ans) rst)) (list) lstAns))))]
                                                                  
                                                        [else (begin (display "fuck bo\n") (VList (list (VEmpty))))]) (VObject-loc (AVal-val rst)) (VObject-field (AVal-val rst)))
                                        (AVal-env rst) rstSto (AVal-lenv rst)))]
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
                           (if (isObjTrue v-i s-i)
                               (interp-env t e-i s-i le-i)
                               (interp-env e e-i s-i le-i))]
                     [else ians]))]
    
    [CId (id) (let ([rst (grabValue id env store lenv)])
                (begin #| (display "In CId : \n") (display (to-string id)) (display (to-string (hash-keys store))) (display "\n") |#
                  (type-case CAns rst
                    [AVal (v-v e-v s-v le-v) 
                          (let ([val (VObject-value v-v)])
                            (begin #| (if (equal? id 'x) (begin (display "AAAAA") (display (to-string (add-lenv id le-v))) (display "\n")) (display "")) |# 
                              (if (VPoint? val)
                                  (interp-env (CGetfield (VPoint-obj val) (VPoint-field val)) e-v s-v (if (equal? -1 (findTheLoc id e-v (if (>= (getLevel le-v) nonlocal-level)
                                                                                                                                            nonlocal-level
                                                                                                                                            (- (getLevel le-v) 1)))) le-v (add-lenv id le-v)))
                                  (if (VMPoint? val)
                                      (AVal (some-v (hash-ref store (VMPoint-loc val))) e-v s-v (if (equal? -1 (findTheLoc id e-v (if (>= (getLevel le-v) nonlocal-level)
                                                                                                                                      nonlocal-level
                                                                                                                                      (- (getLevel le-v) 1)))) le-v (add-lenv id le-v)))
                                      (AVal v-v e-v s-v (if (equal? -1 (findTheLoc id e-v (if (>= (getLevel le-v) nonlocal-level) 
                                                                                              nonlocal-level
                                                                                              (- (getLevel le-v) 1)))) le-v (add-lenv id le-v)))))))]
                    [else rst])))]
    
    [CLet (id bind body) (let ([bindAns (interp-env bind env store lenv)])
                           (type-case CAns bindAns
                             [AVal (v-bind e-bind s-bind le-bind)
                                   (begin ;(display "In CLet: ") (display id) (display " -> ") (display (to-string v-bind)) (display "\n")
                                     (let ([where (VObject-loc v-bind)])
                                       (interp-env body
                                                   (envSet id where e-bind le-bind)
                                                   (hash-set s-bind where v-bind)
                                                   le-bind)))]
                             [else bindAns]))]
    [CSet (id value) (let ([vans (interp-env value env store lenv)])
                       (type-case CAns vans
                         [AVal (v-v e-v s-v le-v) 
                               (let ([where (newLoc)])
                                 (if (isImmutable (VObject-type v-v))
                                     (AVal (VObject (VObject-type v-v) (VObject-value v-v) where (VObject-field v-v)) 
                                           (envSet id where e-v le-v)
                                           (hash-set s-v where v-v)
                                           ;(if (AVal? (grabValue id e-v s-v le-v))
                                            ;   le-v
                                             ;  (add-lenv id le-v))
                                           (add-lenv id le-v))
                                     (AVal v-v 
                                           (envSet id (VObject-loc v-v) e-v le-v)
                                           (hash-set s-v (VObject-loc v-v) v-v) 
                                           ;(if (AVal? (grabValue id e-v s-v le-v))
                                            ;   le-v
                                             ;  (add-lenv id le-v))
                                           (add-lenv id le-v))))]
                         [else vans]))]
    
    [CSetMore (ids value) (let ([vans (interp-env value env store lenv)])
                            (type-case CAns vans
                              [AVal (v-v e-v s-v le-v) 
                                    (type-case CVal (VObject-value v-v)
                                      [VTuple (es) (letrec ([newlocs (allocLocList (length es))]
                                                            [newanss (map (lambda (x) (AVal x e-v s-v le-v)) es)])
                                                     (bind-args ids (list) newlocs newanss (list) e-v s-v le-v))]
                                      [else (interp-error "It's not a tuple" e-v s-v le-v)])]
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
    
    [CGlobal (names) (AVal (VEmpty) 
                           (begin (let ([tstenv (foldl (lambda (x result) (if (AVal? (grabValue x result store lenv))
                                                                              result
                                                                              (let ([where (newLoc)])
                                                                                (envSetGlobal x where result)))) env names)])
                                    (begin #| (display (to-string (hash-keys (some-v (hash-ref tstenv 1))))) |#
                                           tstenv)))
                           store
                           (foldl (lambda (x result) (if (AVal? (grabValue x env store result))
                                                         result
                                                         (add-lenv-global x result))) lenv names))]
    
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
                              [else (type-case CVal v-a
                                      [VObject (t-o v-o l-o f-o)
                                               (type-case CVal v-o
                                                 [VTuple (es) (let ([newloc (newLoc)]) 
                                                                   (AExc (VRet (VObject t-o 
                                                                                        (VTuple (map (lambda (x) (type-case CVal (VObject-value x)
                                                                                                                   [VClosure (args varargs defaults body e-v)
                                                                                                                             (let ([rst (AVal-val (interp-env ($to-object (CFunc (list) (list) (list) (CEmpty))) e-a s-a l-a))])
                                                                                                                               (VObject (VObject-type rst) (VClosure args varargs defaults body newloc) (VObject-loc rst) (VObject-field rst)))]
                                                                                                                   [else x])) es)) 
                                                                                        l-o f-o)) e-a (hash-set s-a newloc (VEnv (mergeNAndL e-a))) l-a))]
                                                 [else retVal])]
                                      [else retVal])]))]
                    [else result]))]
#|                    [AVal (v-a e-a s-a l-a)
                          (let ([retVal (AExc (VRet v-a) e-a s-a l-a)])
                            (begin (display "Begin:\n")
                                   (display (to-string ret))
                                   (display "\n")
                                   (display (to-string v-a))
                                   (display "\n")
                                     (type-case CVal v-a
                                       [VObject (t-o v-o l-o f-o)
                                                (type-case CVal v-o
                                                  [VClosure (args varags defaults body e-v)
                                                            (AExc (VRet (VObject t-o v-o l-o f-o)) e-a (hash-set s-a e-v (VEnv (mergeNAndL e-a))) l-a)]
                                                  [else retVal])]
                                       [else retVal])))]
                    [else result] |#
    
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
                                                                          newLocList
                                                                          ;; interp arguments with closure environment
                                                                          (begin
                                                                            (append self (let ([test-val (interpArgs_Func 
                                                                                                          args 
                                                                                                          starargs 
                                                                                                          e-fobj
                                                                                                          s-fobj
                                                                                                          le-fobj)])
                                                                                           (begin
                                                                                             test-val))))
                                                                          cldfts
                                                                          (type-case CExp fun
                                                                            [CId (id) (if (checklibfun id)
                                                                                          e-fobj
                                                                                          (hash-set e-fobj nonlocal-level (VEnv-e (some-v (hash-ref s-fobj clenv)))))]
                                                                            [else (hash-set e-fobj nonlocal-level (VEnv-e (some-v (hash-ref s-fobj clenv))))])
                                                                          s-fobj (hash-set le-fobj (+ 1 (getmaxnumber (hash-keys lenv))) (list)))]) ;; extend closure-env (if no arguments)
                                               (type-case CAns bind-es
                                                 [AVal (v-es e-es s-es le-es) ;(Scanfb clbody (LocalValRemove e-es le-es) le-es)
                                                       (begin 
                                                         (type-case CAns (begin (interp-env clbody (Scanfb clbody (LocalValRemove e-es le-es) le-es) s-es le-es))
#|=======
                                                 [AVal (v-es e-es s-es le-es)
                                                       (begin #| (display "In CApp:\n") (display (to-string clenv)) (display "\n") |#
                                                         (type-case CAns (begin (interp-env clbody (begin #| (display "In CApp\n") (display (to-string fun)) (display (to-string (hash-keys (some-v (hash-ref env local-level))))) (display "\n") |# e-es) s-es le-es))
>>>>>>> 54af1cf75506d9cc2c15df0afcc05767215fb77c|#
                                                           [AVal (v-clbody e-clbody s-clbody le-clbody) (AVal v-clbody 
                                                                                                              (hash-set env global-level (some-v (hash-ref e-clbody global-level))) 
                                                                                                              (type-case CExp fun
                                                                                                                [CId (id) (if (checklibfun id)
                                                                                                                              s-clbody
                                                                                                                              (hash-set s-clbody clenv (VEnv (some-v (hash-ref e-clbody nonlocal-level)))))]
                                                                                                                [else (hash-set s-clbody nonlocal-level (VEnv (some-v (hash-ref e-clbody nonlocal-level))))])
                                                                                                              (hash-set lenv 1 (some-v (hash-ref le-clbody 1))))]
                                                           [AExc (v-clbody e-clbody s-clbody le-clbody) 
                                                               (if (VRet? v-clbody)
                                                                   (begin #| (display (to-string v-clbody)) (display "\n") |#
                                                                          (AVal (VRet-ret v-clbody) 
                                                                                (hash-set env global-level (some-v (hash-ref e-clbody global-level))) 
                                                                                (type-case CExp fun
                                                                                  [CId (id) (if (checklibfun id)
                                                                                                s-clbody
                                                                                                (begin (hash-set s-clbody clenv (VEnv (some-v (hash-ref e-clbody nonlocal-level))))))]
                                                                                  [CGetfield (id fld) (hash-set s-clbody clenv (VEnv (some-v (hash-ref e-clbody nonlocal-level))))]
                                                                                  [else (begin (hash-set s-clbody nonlocal-level (VEnv (some-v (hash-ref e-clbody nonlocal-level)))))])
                                                                                (hash-set lenv 1 (some-v (hash-ref le-clbody 1)))))
                                                                   (AExc v-clbody 
                                                                         (hash-set env global-level (some-v (hash-ref e-clbody global-level))) 
                                                                         (type-case CExp fun
                                                                           [CId (id) (if (checklibfun id)
                                                                                         s-clbody
                                                                                         (hash-set s-clbody clenv (VEnv (some-v (hash-ref e-clbody nonlocal-level)))))]
                                                                           [CGetfield (id fld) (hash-set s-clbody clenv (VEnv (some-v (hash-ref e-clbody nonlocal-level))))]
                                                                           [else (hash-set s-clbody nonlocal-level (VEnv (some-v (hash-ref e-clbody nonlocal-level))))]) 
                                                                         (hash-set lenv 1 (some-v (hash-ref le-clbody 1)))))]))]
                                                 [AExc (v-es e-es s-es le-se) (AExc v-es env s-es lenv)]))]
                                   [else (case (string->symbol (VObject-type v-fobj))
                                           [(Class) (let ([rst (interp-env (CApp (CGetfield fun "__new__") (list) (list)) e-fobj s-fobj le-fobj)])
                                                      (let ([ret (interp-env (CApp (CGetfield fun "__init__") (list) (list)) (AVal-env rst) (AVal-sto rst) (AVal-lenv rst))])
                                                        (if (AVal? ret)
                                                            ret
                                                            rst)))]
                                           [(Instance) (interp-env (CApp (CGetfield fun "__call__") args starargs) e-fobj s-fobj le-fobj)]
                                           [else (interp-env (raise-error "TypeError" "Not callable") e-fobj s-fobj le-fobj)])]))]
                         [else funAns]))]
    
    [CFunc (args varargs defaults body) (let ([dftAns (interpArgs defaults env store lenv)])
                                          (let ([where (newLoc)])
                                            (cond [(empty? dftAns) (AVal (VClosure args varargs (list) body where) env (begin #| (display "The Location: ") (display (to-string where)) |#
                                                                                                                         (let ([tstven (hash-set store where (VEnv (mergeNAndL env)))])
                                                                                                                           (begin #| (display "\nIn Func\n")
                                                                                                                                  (display (to-string where))
                                                                                                                                  (display (to-string args))
                                                                                                                                  (display "\n")
                                                                                                                                  (display (to-string (VEnv (mergeNAndL env)))) |# tstven))) lenv)]
                                                  [else (let ([lastAns (first (reverse dftAns))])
                                                          (begin ;(display (to-string (AVal-val lastAns))) (display "\n##########################\n")
                                                          (type-case CAns (first (reverse dftAns))
                                                            [AVal (v e s le) (AVal (VClosure args varargs (map (lambda (ans) (VObject-loc (AVal-val ans))) dftAns) body where) e (hash-set s where (VEnv (mergeNAndL env))) le)]
                                                            [else lastAns])))])))]
    
    [CPrim0 (prim) (case prim
                     [(locals) (letrec ([ret (interp-env ($to-object (CDict (list) (list))) env store lenv)]
                                        [rst (AVal-val ret)]
                                        [fin (VObject (VObject-type rst) 
                                                (VDict 
                                                 (foldl (lambda(x result) (let ([newkey (VStr (symbol->string x))]) 
                                                                            (hash-set result newkey (AVal-val (grabValue x env store lenv)))))
                                                        (hash empty) (get-local lenv))) (VObject-loc rst) (VObject-field rst))])
                                 (AVal fin (AVal-env ret) (hash-set (AVal-sto ret) (VObject-loc rst) fin) (AVal-lenv ret)))]
                     [else (interp-error "There is no such built-in function" env store lenv)])]
    
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
                                       [(none? result) (type-case CVal (VObject-value v-obj)
                                                         [VBases (ids) (if (equal? -1 (first ids))
                                                                           (interp-env (raise-error "AttributeError" (string-append "Unbound identifier: " fld)) e-obj s-obj le-obj)
                                                                           (let ([rst (map (lambda(x) (interp-env (CGetbasefld x fld) e-obj s-obj le-obj)) ids)])
                                                                             (get-first-nonexception rst)))]
                                                         [else (interp-env (raise-error "AttributeError" (string-append "Unbound identifier: " fld)) e-obj s-obj le-obj)])]
                                       [else (AVal (some-v result) e-obj s-obj le-obj)]))]
                             [else objv]))]
    
    [CGetbasefld (loc fld) (let ([v-obj (some-v (hash-ref store loc))])
                             (case (string->symbol (VObject-type v-obj))
                               [(Class) (let ([result (hash-ref (VObject-field v-obj) fld)])
                                          (cond 
                                            [(none? result) (type-case CVal (VObject-value v-obj)
                                                              [VBases (ids) (if (equal? -1 (first ids))
                                                                                (interp-env (raise-error "AttributeError" (string-append "Unbound identifier: " fld)) env store lenv)
                                                                                (let ([rst (map (lambda(x) (interp-env (CGetbasefld x fld) env store lenv)) ids)])
                                                                                  (get-first-nonexception rst)))]
                                                              [else (interp-env (raise-error "AttributeError" (string-append "Unbound identifier: " fld)) env store lenv)])]
                                            [else (AVal (some-v result) env store lenv)]))]
                               [else (interp-error "It's not a class" env store lenv)]))]
    
    ;; initialize the object
    [CObject (type prim exp)
             (let ([primVal (interp-env prim env store lenv)])
               (type-case CAns primVal
                 [AVal (v-pv e-pv s-pv le-pv)
                       (let ([rs (interp-env exp e-pv s-pv (hash-set le-pv (+ 1 (getmaxnumber (hash-keys le-pv))) (list)))]
                             [where (newLoc)])
                         (type-case CAns rs
                           [AVal (v-rs e-rs s-rs le-rs) 
                                 (let ([newloc (newLoc)])
                                   (AVal (VObject type (case (string->symbol type)
                                                         [(Class) (if (VObject? v-pv)
                                                                      (VBases (list -1))
                                                                      v-pv)]
                                                         [else (begin v-pv)]) where
                                                  (let ([rst (make-hash empty)])
                                                    (begin (map 
                                                            (lambda (x) (hash-set! rst (symbol->string x) (let ([val (AVal-val (grabValue x e-rs s-rs le-rs))])
                                                                                                            (if (equal? type "Class")
                                                                                                                (type-case CVal (VObject-value val)
                                                                                                                  [VClosure (a v d b e) (begin (VObject (VObject-type val)
                                                                                                                                                        (VClosure a v d b newloc)
                                                                                                                                                        (VObject-loc val)
                                                                                                                                                        (VObject-field val)))]
                                                                                                                  [else val])
                                                                                                                val))))
                                                            (some-v (hash-ref le-rs (getmaxnumber (hash-keys le-rs))))) rst))) e-pv ;; Check here 
                                                                                                                               (if (equal? type "Class")
                                                                                                                                   (begin #|(display (to-string (hash-keys (some-v (hash-ref e-rs local-level))))) (display "\n") |# (hash-set s-rs newloc (VEnv (mergeNAndL env))))
                                                                                                                                   s-rs)
                                                                                                                               le-pv))] ;; Check here
                           [else rs]))]
                 [else primVal]))]
    
    [CBases (bases) (let ([rst (map (lambda(x) (findTheLoc (CId-id x) env local-level)) bases)])
                      (if (check-not-negative rst)
                          (AVal (VBases rst) env store lenv)
                          (interp-error "The base is not exist" env store lenv)))]
    
    ;; operations of different class
    [COperation (obj type op args)
                (let ([o-val (interp-env obj env store lenv)])
                  (type-case CAns o-val
                      [AVal (v-o e-o s-o le-o)
                            (case (string->symbol type)
                              [(Str) (case (string->symbol op)
                                       [(iter) (interp-env ($to-object (CIter (CWrap "List" v-o))) e-o s-o le-o)])]
                              [(List) (case (string->symbol op)
                                        [(iter) (interp-env ($to-object (CIter (CWrap "List" v-o))) e-o s-o le-o)]
                                        [(append) (letrec ([appele (AVal-val (interp-env (first args) e-o s-o le-o))]
                                                           [rst (append (VList-es (VObject-value v-o)) (list appele))]
                                                           [ret (VObject (VObject-type v-o)
                                                                         (VList rst)
                                                                         (VObject-loc v-o)
                                                                         (VObject-field v-o))])
                                                    (AVal ret e-o (hash-set s-o (VObject-loc v-o) ret) le-o))]
                                        [(extend) (letrec ([applst (AVal-val (interp-env (first args) e-o s-o le-o))]
                                                           [tolist (VObject-value (AVal-val (interp-env (CWrap "List" applst) e-o s-o le-o)))]
                                                           [rst (append (VList-es (VObject-value v-o)) (VList-es tolist))]
                                                           [ret (VObject (VObject-type v-o)
                                                                         (VList rst)
                                                                         (VObject-loc v-o)
                                                                         (VObject-field v-o))])
                                                    (AVal ret e-o (hash-set s-o (VObject-loc v-o) ret) le-o))])]
                              [(Range) (case (string->symbol op)
                                         [(iter) (interp-env ($to-object (CIter (CWrap "List" v-o))) e-o s-o le-o)])]
                              [(Tuple) (case (string->symbol op)
                                         [(iter) (interp-env ($to-object (CIter (CWrap "List" v-o))) e-o s-o le-o)])]
                              [(Filter) (case (string->symbol op)
                                          [(next) (letrec ([filval (VObject-value v-o)]
                                                           [iterobj (some-v (hash-ref s-o (VFilter-iter filval)))]
                                                           [filterobj (some-v (hash-ref s-o (VFilter-filter filval)))])
                                                    (interp-env (CApp (CCopy filterobj) (list (CCopy iterobj)) (list)) e-o s-o le-o))])]
                                                    
                                                    
                              [(Iter) (case (string->symbol op)
                                        ;[(iter) o-val]
                                        [(next) (letrec ([iter (VObject-value v-o)]
                                                         [itat (VIter-at iter)]
                                                         [ites (VIter-es iter)])
                                                  (if (>= itat (length ites))
                                                      (interp-env (raise-error "StopIteration" "iterator end") e-o s-o le-o)
                                                      (let ([updateIter (VObject (VObject-type v-o) (VIter (add1 itat) ites) (VObject-loc v-o) (VObject-field v-o))])
                                                        (AVal (list-ref ites itat) e-o (hash-set s-o (VObject-loc v-o) updateIter) le-o))))])]
                                        ;[(all) (iterAll (CGetfield obj "__next__") e-o s-o le-o)]
                                        ;[(any) (iterAny (CGetfield obj "__next__") e-o s-o le-o)]
                                        ;[(filter) (letrec ([lstAns (iterFilter (CGetfield obj "__next__") (first args) e-o s-o le-o)]
                                         ;                  [last (if (empty? lstAns) (AVal (VStr "dummy") e-o s-o le-o) (first (reverse lstAns)))])
                                          ;          (interp-env ($to-object (CIter 
                                           ;               (CCopy (VObject "List" (VList (foldr (lambda (ans rst) (cons (AVal-val ans) rst)) (list) lstAns)) -1 (hash empty)))))
                                            ;                    (AVal-env last) (AVal-sto last) (AVal-lenv last))
                                             ;                   )])]
                              [(CalIter) (case (string->symbol op)
                                           ;[(iter) o-val]
                                           [(next) (let ([caliter (VObject-value v-o)])
                                                     (if (VCalIter-stop caliter)
                                                         (interp-env (raise-error "StopIteration" "callable iterator end") e-o s-o le-o)
                                                         (let ([nextAns (interp-env (CApp (VCalIter-call caliter) (list) (list)) e-o s-o le-o)])
                                                           (type-case CAns nextAns
                                                             [AVal (obj-next e-next s-next le-next) 
                                                                   (if (equal? (getNoneObjectVal obj-next s-next) (VCalIter-stn caliter))
                                                                       (let ([updateIter (VObject "CalIter" 
                                                                                                  (VCalIter true (VCalIter-call caliter) (VCalIter-stn caliter)) 
                                                                                                  (VObject-loc v-o) (VObject-field v-o))])
                                                                         (interp-env (raise-error "StopIteration" "callable iterator end") e-next (hash-set s-next (VObject-loc v-o) updateIter) le-next))
                                                                       nextAns)]
                                                             [else nextAns]))))])]
                              [(Class) (case (string->symbol op)
                                         [(__new__) (letrec ([where (newLoc)]
                                                             [rst (VObject "Instance" 
                                                                           (VBases (list (VObject-loc v-o)))
                                                                           where (hash empty))])
                                                      (AVal rst e-o (hash-set s-o where rst) le-o))]
                                         [(__class__) (AVal (some-v (hash-ref s-o (first (VBases-ids (VObject-value v-o))))) e-o s-o le-o)]
                                         [(__dict__) (letrec ([rstAns (interp-env ($to-object (CDict (list) (list))) e-o s-o le-o)]
                                                              [rstObj (VObject "Dict"
                                                                               (VDict (hash empty))
                                                                               (VObject-loc (AVal-val rstAns)) (VObject-field (AVal-val rstAns)))])
                                                       (AVal rstObj (AVal-env rstAns) (hash-set (AVal-sto rstAns) (VObject-loc (AVal-val rstAns)) rstObj) (AVal-lenv rstAns)))])]
                              [(Set) (case (string->symbol op)
                                       [(iter) (interp-env ($to-object (CIter (CWrap "List" v-o))) e-o s-o le-o)])]
                              [(Dict) (case (string->symbol op)
                                        [(iter) (interp-env ($to-object (CIter (CWrap "List" v-o))) e-o s-o le-o)]
                                        [(clear) (let ([rst (VObject (VObject-type v-o) (VDict (hash empty)) (VObject-loc v-o) (VObject-field v-o))])
                                                   (AVal rst e-o (hash-set s-o (VObject-loc v-o) rst) le-o))]
                                        ;[(keys) (let ([rst (AVal-val (interp-env ($to-object (CSetV (list))) e-o s-o le-o))])
                                         ;        (AVal (VObject (VObject-type rst) 
                                          ;                        (VSet (foldl (lambda (x ht) (hash-set ht x true)) (hash empty) (hash-keys (VDict-dict (VObject-value v-o)))))
                                           ;                      (VObject-loc rst) (VObject-field rst)) e-o s-o le-o))]
                                        [(keys) (letrec ([rstAns (interp-env ($to-object (CSetV (list))) e-o s-o le-o)]
                                                         [rstObj (VObject "Set"
                                                                 (VSet (foldl (lambda (x ht) (hash-set ht x true)) (hash empty) (hash-keys (VDict-dict (VObject-value v-o)))))
                                                                 (VObject-loc (AVal-val rstAns)) (VObject-field (AVal-val rstAns)))])
                                                  (AVal rstObj (AVal-env rstAns) (hash-set (AVal-sto rstAns) (VObject-loc (AVal-val rstAns)) rstObj) (AVal-lenv rstAns)))]
                                        [(pop) (let ([index (AVal-val (interp-env (first args) e-o s-o le-o))])
                                                 (AVal (VEmpty) e-o (hash-set s-o (VObject-loc v-o) 
                                                                              (VObject (VObject-type v-o) 
                                                                                       (VDict (hash-remove (VDict-dict (VObject-value v-o)) (getNoneObjectVal index store))) 
                                                                                       (VObject-loc v-o)
                                                                                       (VObject-field v-o))) le-o))]
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
                                                              
                                                              [(None) (let ([none (AVal-val (interp-env (CId 'None) e-a s-a le-a))])
                                                                        (if (= (VObject-loc none) (VObject-loc v-a))
                                                                            (interp-env (raise-error "TypeError" "dict update takes an dict object") e-a s-a le-a)
                                                                            arg))]
                                                              
                                                              [else (interp-error "argument must be dict or empty" e-a s-a le-a)])]
                                                      [else arg]))])])]
                      [else o-val]))]

    [CTryExn (body hdlers els) (let ([bodyv (interp-env body env store lenv)])
                                 (type-case CAns bodyv
                                   ;; if no exception, interp "else" part
                                   [AVal (v-bv e-bv s-bv le-bv) (let ([elsv (interp-env els e-bv s-bv le-bv)])
                                                                  (if (VEmpty? (getObjVal (AVal-val elsv)))
                                                                      bodyv
                                                                      elsv))]
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
    
    [CExceptHandler (name body type) (let ([excVal (AVal-val (grabValue 'exception_symbol env store lenv))])
                                       (if (VRet? excVal) ; if return or break cause the exception type, return the return value with AVal
                                           (AExc excVal env store lenv)
                                       (let ([ErrorType (VException-type (getObjVal excVal))]
                                             [typev (let ([typev-val (interp-env type env store lenv)])
                                                      (if (or (not (CId? type)) (VTuple? (getObjVal (AVal-val typev-val))))
                                                          (interp-env type env store lenv)
                                                          (interp-env (ContructExc type "") env store lenv)))]
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
                                                   (if (VTuple? (getObjVal (AVal-val typev)))
                                                       (isInExc ErrorType 
                                                                (map (lambda (x) (interp-env (CApp (CCopy x) (list ($to-object (CStr ""))) (list)) env store lenv)) 
                                                                                             (VTuple-es (getObjVal (AVal-val typev)))))
                                                       (equal? ErrorType (VException-type (getObjVal (AVal-val typev)))))))
                                             (if (VEmpty? (getObjVal (AVal-val namev)))
                                                 (interp-env body env store lenv)
                                                 (let ([where (newLoc)])
                                                   (interp-env body
                                                               (envSet (string->symbol (VStr-s (getObjVal (AVal-val namev)))) where env lenv)
                                                               (hash-set store where excVal)
                                                               lenv)))
                                             ;;Didn't go inside the Except body
                                             (AExc excVal env store lenv)))))]  
    
    
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

;; Scan for local Variable
(define (Scanfb [expr : CExp] [env : Env] [lenv : LocalEnv]) : Env
  (type-case CExp expr
    [CSeq (e1 e2) (let ([elenv (Scanfb e1 env lenv)])
                    (Scanfb e2 elenv lenv))]
    [CSet (id value) (envSetScanVar id unbound-local-val-loc env)]
    [CLet (id bind body) (Scanfb body env lenv)]
    [else env]))
          
;; Remove unbound local variable flag in scan_var level                                           
(define (LocalValRemove [env : Env] [lenv : LocalEnv]) : Env
  (hash-set env scan_var (hash empty)))

(define (update-venv (before : LevelEnv) (later : LevelEnv)) : CVal
  (let ([newenv (hash empty)])
    (VEnv (foldl (lambda(x result) (hash-set result x (if (none? (hash-ref later x))
                                                          (some-v (hash-ref before x))
                                                          (some-v (hash-ref later x))))) newenv (hash-keys before)))))

;; return the first result is not excpetion
(define (get-first-nonexception (anss : (listof CAns))) : CAns
  (if (equal? 1 (length anss))
      (first anss)
      (type-case CAns (first anss)
        [AVal (val env sto lenv) (first anss)]
        [else (get-first-nonexception (rest anss))])))

;; check whether the function is a lib function
(define (checklibfun (id : symbol )) : boolean
  (foldl (lambda (x result) (or result (equal? id (bind-left x)))) false lib-functions))

;; produece a range list for range object by given start num, end num and step num
(define (getRangeList (range : (listof CVal))) : (listof CExp)
  (map (lambda (el) ($to-object (CNum el))) (getNumRangeList (VNum-n (first range)) (VNum-n (second range)) (VNum-n (third range)))))

(define (getNumRangeList (from : number) (to : number) (step : number)) : (listof number)
  (if (or (and (> step 0) (> to from)) 
          (and (< step 0) (< to from)))
      (cons from (getNumRangeList (+ from step) to step))
      (list)))

;; get local variable
(define (get-local (lenv : LocalEnv)) : (listof symbol)
  (begin #| (display (to-string (some-v (hash-ref lenv (- (getmaxnumber (hash-keys lenv)) 1))))) |#
         (some-v (hash-ref lenv (- (getmaxnumber (hash-keys lenv)) 1)))))

;; add a symbol into current level
(define (add-lenv (val : symbol) (lenv : LocalEnv)) : LocalEnv
  (let ([maxlevel (getmaxnumber (hash-keys lenv))])
    (if (or (findElement val (some-v (hash-ref lenv maxlevel))) (checklibfun val) (checktempval val))
        lenv
        (hash-set lenv maxlevel (append (some-v (hash-ref lenv maxlevel)) (list val))))))

(define (add-lenv-global (val : symbol) (lenv : LocalEnv)) : LocalEnv
  (hash-set lenv 1 (append (some-v (hash-ref lenv 1)) (list val))))

(define (checktempval (val : symbol)) : boolean
  (if (< (length (string->list (symbol->string val))) 6)
      false
      (let ([rst (string->list (symbol->string val))])
        (equal? "newObj" (list->string (build-list 6 (lambda (x) (list-ref rst x))))))))

;; delete a symbol from current level
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

(define (inLenv (id : symbol) (lenv : LocalEnv)) : boolean
  (foldl (lambda(x result) (or result (findElement id (some-v (hash-ref lenv x))))) false (build-list (getmaxnumber (hash-keys lenv)) (lambda (x) (+ 1 x)))))

(define (findTheLoc (for : symbol) (env : Env) (level : number)) : number
  (if (< level global-level)
      -1
      (type-case (optionof Location) (hash-ref (some-v (hash-ref env level)) for)
        [some(n) n]
        [none() (findTheLoc for env (- level 1))])))

(define (getTheLevel (for : symbol) (env : Env) (level : number)) : number
  (if (< level global-level)
      -1
      (type-case (optionof Location) (hash-ref (some-v (hash-ref env level)) for)
        [some(n) level]
        [none() (getTheLevel for env (- level 1))])))


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

(define (bind-args (args : (listof symbol)) (varargs : (listof symbol)) (locs : (listof Location)) (anss : (listof CAns)) (dfts : (listof Location)) (env : Env) (sto : Store) (lenv : LocalEnv)) : CAns
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
                                        (let ([defaultLastN (lastN (- (length args) (length anss)) dfts (list))])
                                          (map (lambda (loc) (some-v (hash-ref (AVal-sto latest) loc))) defaultLastN)))
                                (AVal-sto latest))
                 (let ([tstlen (foldl (lambda (x result) (add-lenv x result)) lenv args)])
                   (begin #| (display (to-string tstlen)) |#
                          tstlen))))]
        [else (begin #| (display (to-string args)) (display (to-string (length anss))) |# (interp-env (raise-error "TypeError" "Arity mismatch") env sto lenv))]))

 ; case :
 ; Varargs (3,(1,2,3,4,5),() )  
 ; return : 1,2,3,(4,5)<-tuple  
(define (Varargs_CVal (n : number) (input : (listof CVal)) (output : (listof CVal))) : (listof CVal)
  (if (= 0 n)
      (append (reverse output) (list (VObject "Tuple" (VTuple input) -1 (hash empty))))  
      (Varargs_CVal (- n 1) (rest input) (cons (first input) output))))
               
(define (lastN n input output)
  (let ([revs (reverse input)])
    (if (= 0 n)
        output
        (lastN (- n 1) (rest input) (cons (first revs) output)))))

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
         (let ([loc (findTheLoc for env local-level)])
           (if (equal? -1 loc)
               (if (equal? (findTheLoc for env scan_var) unbound-local-val-loc)
                   (interp-env (raise-error "UnboundLocalError" (string-append (symbol->string for) " : unbound local variable")) env sto lenv)
                   (interp-env (lookup_lib-funcs for lib-functions) env sto lenv))
               (type-case (optionof CVal) (hash-ref sto loc)
                 [some (v) (AVal v env sto lenv)]
                 [none () (interp-error "Unbound value" env sto lenv)])))))

;; check whether all numbers are not negative
(define (check-not-negative (locs : (listof Location))) : boolean
  (if (empty? locs)
      true
      (foldl (lambda (x result) (and result (if (< x 0) false true))) true locs)))

;; due to the env is levelified, I have to update the hash-set
(define (envSet (id : symbol) (loc : Location) (env : Env) (lenv : LocalEnv)) : Env
  (begin 
    #|(display "Current level : ")
    (display (getmaxnumber (hash-keys lenv))) 
    (display "\n") |#
    (if (equal? -1 (findTheLoc id env local-level))
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
                tstenv)))
        (hash-set env (getTheLevel id env local-level) (hash-set (some-v (hash-ref env (getTheLevel id env local-level))) id loc)))
    ))

(define (envSetGlobal (id : symbol) (loc : Location) (env : Env)) : Env
  (hash-set env global-level (hash-set (some-v (hash-ref env global-level)) id loc)))


(define (envSetScanVar (id : symbol) (loc : Location) (env : Env)) : Env
  (hash-set env scan_var (hash-set (some-v (hash-ref env scan_var)) id loc)))


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
      (if (none? (hash-ref newenv 'self))
          (if (none? (hash-ref (some-v (hash-ref env global-level)) 'self))
              newenv
              (hash-set newenv 'self (some-v (hash-ref (some-v (hash-ref env global-level)) 'self))))
          newenv))))

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

; iter call next to return a listof CAns for the VList
(define (iter2List (nextFunc : CExp) (env : Env) (store : Store) (lenv : LocalEnv)) : (listof CAns)
  (let ([nextAns (interp-env (CApp nextFunc (list) (list)) env store lenv)])
    (type-case CAns nextAns
      [AVal (obj-next e-next s-next le-next)
            (cons nextAns (iter2List nextFunc e-next s-next le-next))]
      [else empty])))

; iter call next until one false appear or reach end
(define (iterAll (nextFunc : CExp) (env : Env) (store : Store) (lenv : LocalEnv)) : CAns
  (let ([nextAns (interp-env (CApp nextFunc (list) (list)) env store lenv)])
    (type-case CAns nextAns
      [AVal (obj-next e-next s-next le-next)
            (if (isObjTrue obj-next s-next) (iterAll nextFunc e-next s-next le-next) (interp-env (CId 'False) e-next s-next le-next))]
      [AExc (exc-next e-next s-next le-next)
            (interp-env (CId 'True) e-next s-next le-next)])))

; iter call next until one true appear or reach end
(define (iterAny (nextFunc : CExp) (env : Env) (store : Store) (lenv : LocalEnv)) : CAns
  (let ([nextAns (interp-env (CApp nextFunc (list) (list)) env store lenv)])
    (type-case CAns nextAns
      [AVal (obj-next e-next s-next le-next)
            (if (isObjTrue obj-next s-next) (interp-env (CId 'True) e-next s-next le-next) (iterAny nextFunc e-next s-next le-next))]
      [AExc (exc-next e-next s-next le-next)
            (interp-env (CId 'False) e-next s-next le-next)])))

; iter call next until one true appear or reach end
(define (iterFilter (nextFunc : CExp) (filterFunc : CExp) (env : Env) (store : Store) (lenv : LocalEnv)) : (listof CAns)
  (let ([nextAns (interp-env (CApp nextFunc (list) (list)) env store lenv)])
    (type-case CAns nextAns
      [AVal (obj-next e-next s-next le-next)
            (let ([filfuncAns (interp-env filterFunc e-next s-next le-next)])
              (if (equal? (getObjType (AVal-val filfuncAns)) "None")
                  (if (isObjTrue obj-next s-next) (cons nextAns (iterFilter nextFunc filterFunc e-next s-next le-next)) (iterFilter nextFunc filterFunc e-next s-next le-next))
                  (let ([filterAns (interp-env (CApp filterFunc (list (CCopy obj-next)) (list)) e-next s-next le-next)])
                    (type-case CAns filterAns
                      [AVal (obj-fr e-fr s-fr le-fr)
                            (if (isObjTrue obj-fr s-fr)
                                (cons nextAns (iterFilter nextFunc filterFunc e-fr s-fr le-fr)) (iterFilter nextFunc filterFunc e-fr s-fr le-fr))]
                      [else (list filterAns)]))))]
      [else empty])))
              
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
