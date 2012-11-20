#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "python-objects.rkt"
         "python-lib.rkt")

(define (interp [expr : CExp]) : CVal
  (type-case CAns (interp-env expr (hash empty) (hash empty) (hash empty))
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
                                   (AVal (VEmpty) env store lenv)
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
                                    (AVal (VEmpty) env store lenv)
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
                                    (AVal (VEmpty) env store lenv)
                                    (first (reverse rst)))])
                   (if (AExc? last)
                       last
                       (AVal (VSet (let ([keys (map (lambda (x) (if (isImmutable (VObject-type (AVal-val x)))
                                                                   (VObject-value (AVal-val x))
                                                                   (VMPoint (VObject-loc (AVal-val x)))))
                                                    rst)])
                                     (foldl (lambda (x ht) (hash-set ht x true)) (hash empty) keys))) 
                             (AVal-env last) (AVal-sto last) (AVal-lenv last))))]
    [CDict (keys values) (if (equal? (length keys) (length values))
                             (letrec ([keyrst (interpArgs keys env store lenv)]
                                      [keylast (if (empty? keyrst)
                                                   (AVal (VEmpty) env store lenv)
                                                   (first (reverse keyrst)))])
                               (if (AExc? keylast)
                                   keylast
                                   (if (not (equal? (length (filter isImmutable (map (lambda (x) (VObject-type (AVal-val x))) keyrst)) )
                                                    (length keyrst)))
                                       (interp-error "key should be immutable" env store lenv)
                                       (letrec ([valuesrst (interpArgs values (AVal-env keylast) (AVal-sto keylast) (AVal-lenv keylast))]
                                                [valueslast (if (empty? valuesrst)
                                                                (AVal (VEmpty) env store lenv)
                                                                (first (reverse valuesrst)))])
                                         (if (AExc? valueslast)
                                             valueslast
                                             (AVal (VDict (valfoldl2 keyrst valuesrst (hash empty) store))
                                               (AVal-env valueslast) (AVal-sto valueslast) (AVal-lenv valueslast)))))))
                             (interp-error "length is not same" env store lenv))]
    [CTrue () (AVal (VTrue) env store lenv)]
    [CFalse () (AVal (VFalse) env store lenv)]
    [CEmpty () (AVal (VEmpty) env store lenv)]
    
    [CWrap (type obj) (case (string->symbol type)
                        [(List) (let ([rst (AVal-val (interp-env ($to-object (CList (list))) env store lenv))])
                                  (AVal (VObject type (type-case CVal (VObject-value obj)
                                                        [VStr (s) (VList (map (lambda(x) (AVal-val (interp-env ($to-object (CStr (list->string (list x)))) env store lenv))) 
                                                                              (string->list s)))]
                                                        [VList (es) (VList es)]
                                                        [VTuple (es) (VList es)]
                                                        [VDict (dict) (VList (hash-keys dict))]
                                                        [else (VList (list (VEmpty)))]) (VObject-loc rst) (VObject-field rst)) env store lenv))]
                        [(Tuple) (let ([rst (AVal-val (interp-env ($to-object (CTuple (list))) env store lenv))])
                                   (AVal (VObject type (type-case CVal (VObject-value obj)
                                                         [VStr (s) (VTuple (map (lambda(x) (AVal-val (interp-env ($to-object (CStr (list->string (list x)))) env store lenv))) 
                                                                                (string->list s)))]
                                                         [VList (es) (VTuple es)]
                                                         [VTuple (es) (VTuple es)]
                                                         [VDict (dict) (VTuple (hash-keys dict))]
                                                         [else (VTuple (list (VEmpty)))]) (VObject-loc rst) (VObject-field rst)) env store lenv))]
                        [(Set) (let ([rst (AVal-val (interp-env ($to-object (CSetV (list))) env store lenv))])
                                 (AVal (VObject type (type-case CVal (VObject-value obj)
                                                       [VStr (s) (VSet (let ([keys (map (lambda(x) (VObject-value (AVal-val (interp-env ($to-object (CStr (list->string (list x)))) env store lenv)))) 
                                                                                        (string->list s))])
                                                                         (foldl (lambda (x ht) (hash-set ht x true)) (hash empty) keys)))]
                                                       [VList (es) (VSet (foldl (lambda (x ht) (hash-set ht x true)) (hash empty) (VList-es (getNoneObjectVal obj store))))]
                                                       [VTuple (es) (VSet (foldl (lambda (x ht) (hash-set ht x true)) (hash empty) (VTuple-es (getNoneObjectVal obj store))))]
                                                       [VDict (dict) (VSet (foldl (lambda (x ht) (hash-set ht x true)) (hash empty) (hash-keys dict)))]
                                                       [else (VSet (hash empty))]) (VObject-loc rst) (VObject-field rst)) env store lenv))])]
    
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
                (type-case CAns rst
                  [AVal (v-v e-v s-v le-v) 
                        (let ([val (VObject-value v-v)])
                          (if (VPoint? val)
                              (interp-env (CGetfield (VPoint-obj val) (VPoint-field val)) e-v s-v le-v)
                              (if (VMPoint? val)
                                  (AVal (some-v (hash-ref store (VMPoint-loc val))) e-v s-v le-v)
                                  rst)))]
                  [else rst]))]

    [CLet (id bind body) (let ([bindAns (interp-env bind env store lenv)])
                           (type-case CAns bindAns
                             [AVal (v-bind e-bind s-bind le-bind)
                                   (let ([where (VObject-loc v-bind)])
                                     (interp-env body
                                       (hash-set e-bind id where)
                                       (hash-set s-bind where v-bind)
                                       (hash-set le-bind id true)))]
                             [else bindAns]))]
    
    [CSet (id value) (let ([vans (interp-env value env store lenv)])
                       (type-case CAns vans
                         [AVal (v-v e-v s-v le-v) 
                               (let ([where (newLoc)])
                                 (if (isImmutable (VObject-type v-v))
                                     (AVal (VObject (VObject-type v-v) (VObject-value v-v) where (VObject-field v-v)) 
                                           (hash-set e-v id where)
                                           (hash-set s-v where v-v)
                                           (hash-set le-v id true))
                                     (AVal v-v (hash-set e-v id (VObject-loc v-v)) s-v le-v)))]
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
                                                                  (VObject-value (AVal-val vval))))
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
                                                            (equal? (VObject-type (AVal-val (first ival))) "Empty"))
                                                        (or (equal? (VObject-type (AVal-val (second ival))) "Int")
                                                            (equal? (VObject-type (AVal-val (second ival))) "Empty"))
                                                        (or (equal? (VObject-type (AVal-val (third ival))) "Int")
                                                            (equal? (VObject-type (AVal-val (third ival))) "Empty"))
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
    
    [CApp (fun args) (let ([funAns (interp-env fun env store lenv)])
                       ;; function answer
                       (type-case CAns funAns
                         [AVal (v-fobj e-fobj s-fobj le-fobj)
                           ;; function value
                           (type-case CVal (VObject-value v-fobj)

                             #|[VClosure (clargs cldfts clbody clenv csto)
                               (let ([bind-es (bind-args clargs 
                                                         (allocLocList (length clargs)) 
                                                         (interpArgs args e-fobj s-fobj le-fobj)
                                                         cldfts
                                                         clenv csto lenv)]) ;; extend env using global instead closure-env
                                                                           ;; Still need to extend closure-env (deal with situation of closure over local variales|#

                             [VClosure (clargs cldfts clbody clenv csto)
                               (letrec ([self (if (and (not (equal? clargs empty))  
                                                       (symbol=? 'self (first clargs))) 
                                                  (list (interp-env (CId 'self) e-fobj s-fobj le-fobj)) 
                                                  (list))]
                                        [bind-es (bind-args (begin ;(display clargs )
                                                                   clargs)
                                                            (allocLocList (length clargs))
                                                            ;(append self (interpArgs args e-fobj s-fobj le-fobj))
                                                            ;; interp arguments with closure environment
                                                            (begin ;(display "print global env \n")
                                                                   ;(display e-fobj)
                                                                   ;(display "\n")
                                                                   (append self (let ([test-val (interpArgs_Func args clenv csto e-fobj s-fobj le-fobj)])
                                                                                  (begin; (display "interpArgs_Func args \n") 
                                                                                        ; (display test-val)
                                                                                        ; (display "\n")
                                                                                         test-val)))
	)
                                                            cldfts
                                                            e-fobj s-fobj lenv)]) ;; extend env using global instead closure-env
                                 (type-case CAns bind-es
                                   ;	[AVal (v-es e-es s-es le-es) (interp-env clbody e-es s-es le-es)]
                                   [AVal (v-es e-es s-es le-es) 
                                                                       (type-case CAns (begin ;(display " enter here \n")
                                                                                              ;(display e-es)
                                                                                              ;(display "\n")
                                                                                              (interp-env clbody e-es s-es le-es))
                                                                         [AVal (v-clbody e-clbody s-clbody le-clbody) (begin ;(display (VClosure-env (getObjVal v-clbody)))
                                                                                                                        ;(display "\n")
                                                                                                                        ;(display (VClosure-sto (getObjVal v-clbody)))
                                                                                                                        ;(display "Env leave App\n")
                                                                                                                        ;(display env)
                                                                                                                        ;(display "\n")
                                                                                                                        (AVal v-clbody env store lenv))]
                                                                         [AExc (v-clbody e-clbody s-clbody le-clbody) (AExc v-clbody env store lenv)])]
                                   [else bind-es]))]
                             [else (interp-error "Not a function" e-fobj s-fobj le-fobj)])]
                         [else funAns]))]

    [CFunc (args defaults body) (let ([dftAns (interpArgs defaults env store lenv)])
                                  (cond [(empty? dftAns) (AVal (VClosure args (list) body env store) env store lenv)]
                                        [else (let ([lastAns (first (reverse dftAns))])
                                                (type-case CAns (first (reverse dftAns))
                                                  [AVal (v e s le) (AVal (VClosure args (map AVal-val dftAns) body env store) e s le)]
                                                  [else lastAns]))]))]

    [CPrim1 (prim arg) (let ([argAns (interp-env arg env store lenv)])
                         (type-case CAns argAns
                           [AVal (v-obj e-obj s-obj le-obj) 
                                 (begin ;(display "Prim1\n")
                                        ;(display e-obj)
                                        ;(display "\n")                                                 
                                        (interp-env (python-prim1 prim argAns) e-obj s-obj le-obj))]
                           [else argAns]))]
    
    [CPrim2 (prim arg1 arg2) (interp-env (python-prim2 prim (interp-env arg1 env store lenv) (interp-env arg2 env store lenv)) env store lenv)]
     
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
                       (let ([rs (interp-env exp e-pv s-pv (resetLocalEnv le-pv))]
                             [where (newLoc)])
                         (type-case CAns rs
                           [AVal (v-rs e-rs s-rs le-rs) 
                                 (AVal (VObject type v-pv where
                                                (let ([rst (make-hash empty)])
                                                  (begin (map (lambda (x) (hash-set! rst (symbol->string x) (AVal-val (grabValue x e-rs s-rs le-rs))))
                                                              (getModifiedVars le-rs)) rst))) e-rs s-rs le-pv)]
                           [else rs]))]
                 [else primVal]))]
    ;; operations of different class
    [COperation (obj type op)
                (let ([o-val (interp-env obj env store lenv)])
                  (type-case CAns o-val
                      [AVal (v-o e-o s-o le-o)
                            (case (string->symbol type)
                              [(Dict) (case (string->symbol op)
                                        [(clear) (let ([rst (VObject (VObject-type v-o) (VDict (hash empty)) (VObject-loc v-o) (VObject-field v-o))])
                                                   (AVal rst e-o (hash-set s-o (VObject-loc v-o) rst) le-o))])])]
                      [else o-val]))]

    [CTryExn (body hdlers els) (let ([bodyv (interp-env body env store lenv)])
                                 (type-case CAns bodyv
                                   ;; if no exception, interp "else" part
                                   [AVal (v-bv e-bv s-bv le-bv) (interp-env els e-bv s-bv le-bv)]
                                   ;; Exception, interp "Exception Handler" part
                                   ;; bind exception object to Env & Store
                                   [AExc (v-bv e-bv s-bv le-bv) 
                                         (let ([where (newLoc)])
                                           (type-case CAns (interp-env hdlers (hash-set e-bv 'exception_symbol where) (hash-set s-bv where v-bv) le-bv)
                                             ;; leave the Except body ; remove 'exception_symbol in environment
                                             ;; "return" previous env,store,lenv
                                             [AVal (v-hd e-hd s-hd le-hs) (AVal v-hd (hash-remove e-hd 'exception_symbol) (hash-remove s-hd where) le-hs)]
                                             [AExc (v-hd e-hd s-hd le-hs) (AExc v-hd (hash-remove e-hd 'exception_symbol) (hash-remove s-hd where) le-hs)]))]))]
                                                        
                                                                     
    [CTryFinally (b fb) (let ([bv (interp-env b env store lenv)])
                          ;;A finally clause is always executed before leaving the try statement, 
                          ;;whether an exception has occurred or not
                          (type-case CAns bv
                            [AVal (v-bv e-bv s-bv le-bv) (interp-env fb e-bv s-bv le-bv)]
                            [AExc (v-bv e-bv s-bv le-bv) (interp-env fb e-bv s-bv le-bv)]))]
    
    [CExceptHandler (name body type) (let ([value (grabValue 'exception_symbol env store lenv)])
                                       (let ([ErrorType (VException-type (getObjVal (AVal-val value)))]
                                             [typev (if (not (CId? type))
                                                        (interp-env type env store lenv)
                                                        (interp-env (ContructExc type "") env store lenv))]
                                             [namev (interp-env name env store lenv)])
                                           ;; Two condition for entering the Except body 
                                           ;; (a) except "nothing" : (b) except "certain Exception"
                                           (if (or (VEmpty? (getObjVal (AVal-val typev)))       
                                                   (equal? ErrorType (VException-type (getObjVal (AVal-val typev)))))
                                               (if (VEmpty? (getObjVal (AVal-val namev)))
                                                   (interp-env body env store lenv)
                                                   (let ([where (newLoc)])
                                                     (interp-env body 
                                                                 (hash-set env (string->symbol (VStr-s (getObjVal (AVal-val namev)))) where)
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
                              ;(let ([where (newLoc)])
                                (AExc (AVal-val runTimeError) e-v s-v le-v))]))
                  ;; hande case : raie with argument (i.e. raise TypeError("foo")
                  (AExc (AVal-val excv) (AVal-env excv) (AVal-sto excv) (AVal-lenv excv))))]
    
    
    
    [else (begin (display expr)
                 (error 'interp "no case"))]))
                                                 


(define (resetLocalEnv (oldEnv : LocalEnv)) :  LocalEnv
  (foldl (lambda (x result) (hash-set result x false)) (hash empty) (hash-keys oldEnv)))

(define (getModifiedVars (oldEnv : LocalEnv)) : (listof symbol)
  (filter (lambda (x) (some-v (hash-ref oldEnv x))) (hash-keys oldEnv)))

(define (bind-args (args : (listof symbol)) (locs : (listof Location)) (anss : (listof CAns)) (dfts : (listof CVal)) (env : Env) (sto : Store) (lenv : LocalEnv)) : CAns
  (cond [(and (empty? args) (empty? anss)) (AVal (VStr "dummy") env sto lenv)]
        [(and (not (empty? anss)) (AExc? (first (reverse anss)))) (first (reverse anss))] ; last anss has exception
        [(<= (length args) (+ (length anss) (length dfts)))
         (let ([latest (if (empty? anss) (AVal (VStr "dummy") env sto lenv) (first (reverse anss)))])
           (AVal (VStr "dummy")
                 (extendEnv args locs (AVal-env latest))
                 (overrideStore locs
                                (append (map AVal-val anss)
                                        (lastNVals (- (length args) (length anss)) dfts (list)))
                                (AVal-sto latest))
                 (AVal-lenv latest)))]
        [else (interp-error "Arity mismatch" env sto lenv)]))

(define (lastNVals (n : number) (input : (listof CVal)) (output : (listof CVal))) : (listof CVal)
  (let ([revs (reverse input)])
    (if (= 0 n)
        output
        (lastNVals (- n 1) (rest input) (cons (first revs) output)))))

(define (grabValue (for : symbol) (env : Env) (sto : Store) (lenv : LocalEnv)) : CAns
  (type-case (optionof Location) (hash-ref env for)
    [some (loc) 
          (type-case (optionof CVal) (hash-ref sto loc)
            [some (v) (AVal v env sto lenv)]
            [none () (interp-error "Unbound identifier" env sto lenv)])]
    ;; Didn't exist in the current env & sto ; look up the built in library
    [none () (interp-env (lookup_lib-funcs for lib-functions) env sto lenv)]))

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
    [(empty? args) (begin ;(display "leave normally")
                            empty)]
    [else (let ([argAns (interp-env (first args) env store lenv)])
            (type-case CAns argAns
              [AVal (v-first e-first s-first le-first)
                      (cons argAns (interpArgs (rest args) e-first s-first le-first))]
              [else (cons argAns empty)]))]))

; take in a list of CExp (func args) and interp them to a list of answer
(define (interpArgs_Func [args : (listof CExp)] [closure_env : Env] [closure_sto : Store] [global_env : Env] [global_sto : Store] [lenv : LocalEnv]) : (listof CAns)
  (cond
    [(empty? args) (begin ;(display "leave normally")
                          empty)]
    [else (let ([argAns (interp-env (first args) closure_env closure_sto lenv)]) ;;buggy part if I replace closure env store to global, it works fine
            (type-case CAns argAns
              [AVal (v-first e-first s-first le-first)
                    (begin ;(display "come AVal\n")
                           (cons argAns (interpArgs_Func (rest args) e-first s-first global_env global_sto le-first)))]
              [AExc (v-first e-first s-first le-first) 
                    (begin ;(display "come AExc\n")
	                 (let ([argAns_gloenv (interp-env (first args) global_env global_sto lenv)])
                      (type-case CAns argAns_gloenv
                        [AVal (v2-first e2-first s2-first le2-first)
                              (cons argAns_gloenv (interpArgs_Func (rest args) e-first s-first global_env global_sto le-first))]
                        [else (cons argAns empty)])))]
              
              ))]))


;; return a list of location with size given by param num
(define (allocLocList (count : number)) : (listof Location)
  (cond
    [(= 0 count) empty]
    [else (cons (newLoc) (allocLocList (- count 1)))]))

;; extend env with list of args and locations
(define (extendEnv (args : (listof symbol)) (locs : (listof Location)) (env : Env)) : Env
  (cond
    [(and (not (empty? args)) (not (empty? locs)))
     (extendEnv (rest args) (rest locs) 
                (hash-set env (first args) (first locs)))]
    [else env]))

;; extend store with list of locations and answers
(define (overrideStore (locs : (listof Location)) (vals : (listof CVal)) (sto : Store)) : Store
  (cond 
    [(and (not (empty? locs)) (not (empty? vals)))
     (overrideStore (rest locs) (rest vals) 
                    (hash-set sto (first locs) (first vals)))]
    [else sto]))

;; interp a list and return the result, if there is exception in the list it will be on the last element
(define (interpList (expr : (listof CExp)) (env : Env) (store : Store) (lenv : LocalEnv)) : (listof CAns)
  (if (empty? expr)
      empty
      (let ([rst (interp-env (first expr) env store lenv)])
        (if (AExc? rst)
            (cons rst empty)
            (cons rst (interpList (rest expr) (AVal-env rst) (AVal-sto rst) (AVal-lenv rst)))))))

;; specific function to app values into hash table
(define (valfoldl2 (keys : (listof CAns)) (values : (listof CAns)) (h : (hashof CVal CVal)) (store : Store)) : (hashof CVal CVal)
  (if (empty? keys)
      h            
      (valfoldl2 (rest keys) (rest values) (hash-set h (getNoneObjectVal (AVal-val (first keys)) store) 
                                                     (if (isImmutable (VObject-type (AVal-val (first values))))
                                                         (AVal-val (first values))
                                                         (VObject "MPoint" (VMPoint (VObject-loc (AVal-val (first values)))) -1 (hash empty)))) store)))

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
  (let ([lib_functions (interp-env (python-lib (CEmpty)) (hash empty) (hash empty) (hash empty))])
    (AVal-env lib_functions)))
  

;;Merge Environment : Add new_Env to old_Env , if both contains similar symobl new_Env replace old_Env 
(define (MergeEnv (new_Env : Env) (old_Env : Env)) : Env
  (foldl (lambda (x result) 
           (let ([location (some-v (hash-ref new_Env x))])
             (hash-set result x location)))
         old_Env 
         (hash-keys new_Env)))
           
           
  
;;Merge Store :  Add new_Sto to old_Sto , if both contains similar symobl new_Sto replace old_Sto 
(define (MergeSto (new_Sto : Store) (old_Sto : Store)) : Store
  (foldl (lambda (x result)
           (let ([value (some-v (hash-ref new_Sto x))])
             (hash-set result x value)))
         old_Sto
         (hash-keys new_Sto)))
  
  
  
  
  
  
  
  
  
  
  



