#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "python-objects.rkt")

(define (interp [expr : CExp]) : CVal
  (type-case CAns (interp-env expr (hash empty) (hash empty) (hash empty))
    [AExc (excpt env sto lenv) (begin (print excpt) excpt)]
    [AVal (value env sto lenv) value]))

(define (interp-env [expr : CExp] [env : Env] [store : Store] [lenv : LocalEnv]) : CAns
  (type-case CExp expr 
    [CNum (s) (AVal (VNum s) env store lenv)]
    [CStr (s) (AVal (VStr s) env store lenv)]
    [CList (es) (AVal (VList (map (lambda(x) (AVal-val (interp-env x env store lenv))) es)) env store lenv)]
    [CTrue () (AVal (VTrue) env store lenv)]
    [CFalse () (AVal (VFalse) env store lenv)]
    [CEmpty () (AVal (VEmpty) env store lenv)]
    
    [CError (e) (let ([ans (interp-env e env store lenv)])
                  (AExc (AVal-val ans) (AVal-env ans) (AVal-sto ans) (AVal-lenv ans)))]

    [CIf (i t e) (let ([ians (interp-env i env store lenv)])
                   (type-case CAns ians
                     [AVal (v-i e-i s-i le-i)
                           (if (VFalse? (trueOrFalse (getPrimVal v-i)))  ;; this should modify to fit false condition
                               (interp-env e e-i s-i le-i)
                               (interp-env t e-i s-i le-i))]
                     [else ians]))]

    [CId (id) (let ([rst (grabValue id env store lenv)])
                (type-case CAns rst
                  [AVal (v-v e-v s-v le-v) 
                        (if (VPoint? v-v)
                            (interp-env (CGetfield (VPoint-obj v-v) (VPoint-field v-v)) e-v s-v le-v)
                            rst)]
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
                                     (type-case CExp value
                                       [CId (c-id) (AVal v-v (hash-set e-v id (some-v (hash-ref e-v c-id))) s-v le-v)]
                                       [CGetfield (c-obj c-fd) (AVal v-v (hash-set e-v id where)
                                                                     (hash-set s-v where (VPoint c-obj c-fd))
                                                                     (hash-set le-v id true))]
                                       [else (AVal v-v (hash-set e-v id (VObject-loc v-v))
                                           (hash-set s-v (VObject-loc v-v) v-v)
                                           (hash-set le-v id true))])))]
                         [else vans]))]
    
    [CSetelement (obj index value) 
                 (let ([oval (interp-env obj env store lenv)] 
                       [ival (interp-env index env store lenv)]
                       [vval (interp-env value env store lenv)])
                   (if (and (AVal? oval) (and (AVal? ival) (AVal? vval)))
                       (if (equal? (VObject-type (AVal-val ival)) "Int")
                           (cond
                             [(equal? (VObject-type (AVal-val oval)) "List") 
                              (AVal (AVal-val vval)
                                    env
                                    (type-case CExp obj 
                                      [CId (c-id) (hash-set store 
                                                            (some-v (hash-ref env c-id)) 
                                                            (VObject "List" 
                                                                     (VList 
                                                                      (map2 (lambda (x y) 
                                                                              (if (equal? y (VNum-n (VObject-value (AVal-val ival))))
                                                                                  (AVal-val vval)
                                                                                  x)) 
                                                                            (VList-es (VObject-value (AVal-val oval)))
                                                                            (build-list (length (VList-es (VObject-value (AVal-val oval)))) (lambda(x) x))))
                                                                     (VObject-loc (AVal-val oval))
                                                                     (VObject-field (AVal-val oval))))]
                                      [else store])
                                    lenv)]
                             [else (interp-error "I don't address" env store lenv)])
                           (interp-error "Index is not a number" env store lenv))
                       (cond
                         [(AExc? oval) oval]
                         [(AExc? ival) ival]
                         [(AExc? vval) vval]
                         [else (interp-error "You should not been here" env store lenv)])))]
    ;[CGetelement (id index)]|#

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
                             [VClosure (clargs clbody clenv)
                               (let ([bind-es (bind-args clargs 
                                                         (allocLocList (length clargs)) 
                                                         (interpArgs args e-fobj s-fobj le-fobj) 
                                                         env store lenv)]) ;; extend env using global instead closure-env
                                 (type-case CAns bind-es
                                   [AVal (v-es e-es s-es le-es) (interp-env clbody e-es s-es le-es)]
                                   [else bind-es]))]
                             [else (interp-error "Not a function" e-fobj s-fobj le-fobj)])]
                         [else funAns]))]

    [CFunc (args body) (AVal (VClosure args body env) env store lenv)]

    [CPrim1 (prim arg) (let ([argAns (interp-env arg env store lenv)])
                         (if (AVal? argAns)
                             (python-prim1 prim argAns)
                             argAns))]
    
    [CPrim2 (prim arg1 arg2) (interp-env (python-prim2 prim (interp-env arg1 env store lenv) (interp-env arg2 env store lenv)) env store lenv)]
    
    ;[CPrim2Seq (left prims args) ]
     
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
                                                                val)])]
                                           [else val]))]
                                   [else objv]))]
    ;;getfield for object
    [CGetfield (obj fld) (let ([objv (interp-env obj env store lenv)])
                           (type-case CAns objv
                             [AVal (v-obj e-obj s-obj le-obj)
                                   (cond 
                                     [(not (VObject? v-obj)) (interp-error (string-append "Non-object in field update: " (pretty v-obj)) e-obj s-obj le-obj)]
                                     [else (let ([result (hash-ref (VObject-field v-obj) fld)])
                                             (cond [(none? result) (interp-error (string-append "Unbound identifier: "  fld) e-obj s-obj le-obj)]
                                                   [else (AVal (some-v result) e-obj s-obj le-obj)]))])]
                             [else objv]))]
    
    [CObject (type prim exp)
             (let ([primVal (interp-env prim env store lenv)]
                   [rs (interp-env exp env store (resetLocalEnv lenv))]
                   [where (newLoc)])
               (type-case CAns primVal
                 [AVal (v-pv e-pv s-pv le-pv)
                       (type-case CAns rs
                         [AVal (v-rs e-rs s-rs le-rs)
                               (AVal (VObject type v-pv where
                                       (let ([rst (make-hash empty)])
                                         (begin (map (lambda (x) (hash-set! rst (symbol->string x) (AVal-val (grabValue x e-rs s-rs le-rs))))
                                                     (getModifiedVars le-rs))
                                                rst)))
                                     env store lenv)]
                         [else rs])]
                 [else primVal]))]

    [else (begin (display expr)
                 (error 'interp "no case"))]))

(define (resetLocalEnv (oldEnv : LocalEnv)) :  LocalEnv
  (foldl (lambda (x result) (hash-set result x false)) (hash empty) (hash-keys oldEnv)))

(define (getModifiedVars (oldEnv : LocalEnv)) : (listof symbol)
  (filter (lambda (x) (some-v (hash-ref oldEnv x))) (hash-keys oldEnv)))


(define (bind-args (args : (listof symbol)) (locs : (listof Location)) (anss : (listof CAns)) (env : Env) (sto : Store) (lenv : LocalEnv)) : CAns
  (cond [(and (empty? args) (empty? anss)) (AVal (VStr "dummy") env sto lenv)]
        [(= (length args) (length anss))
         (let ([lastAns (first (reverse anss))])
           (if (AVal? lastAns)
               (AVal (VStr "dummy")
                     (extendEnv args locs (AVal-env lastAns)) ;; use passin env instead of closure-env
                     (overrideStore locs anss (AVal-sto lastAns))
                     (AVal-lenv lastAns))
               lastAns))]
        [else (interp-error "Arity mismatch" env sto lenv)]))

(define (grabValue (for : symbol) (env : Env) (sto : Store) (lenv : LocalEnv)) : CAns
  (type-case (optionof Location) (hash-ref env for)
    [some (loc) 
          (type-case (optionof CVal) (hash-ref sto loc)
            [some (v) (AVal v env sto lenv)]
            [none () (interp-error "Unbound identifier" env sto lenv)])]
    [none () (interp-error "location not found in store" env sto lenv)]))

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
(define (overrideStore (locs : (listof Location)) (anss : (listof CAns)) (sto : Store)) : Store
  (cond 
    [(and (not (empty? locs)) (not (empty? anss)))
     (overrideStore (rest locs) (rest anss) 
                    (hash-set sto (first locs) (AVal-val (first anss))))]
    [else sto]))
