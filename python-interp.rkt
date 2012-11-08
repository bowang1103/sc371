#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt")

(define (interp [expr : CExp]) : CAns
  (interp-env expr (hash empty) (hash empty) (hash empty)))

(define (interp-env [expr : CExp] [env : Env] [store : Store] [lenv : LocalEnv]) : CAns
  (type-case CExp expr 
    [CNum (s) (AVal (VNum s) env store lenv)]
    [CStr (s) (AVal (VStr s) env store lenv)]
    [CTrue () (AVal (VTrue) env store lenv)]
    [CFalse () (AVal (VFalse) env store lenv)]
    [CEmpty () (AVal (VEmpty) env store lenv)]
    
    [CError (e) (let ([ans (interp-env e env store lenv)])
                  (AExc (AVal-val ans) (AVal-env ans) (AVal-sto ans) (AVal-lenv ans)))]

    [CIf (i t e) (let ([ians (interp-env i env store lenv)])
                   (type-case CAns ians
                     [AVal (v-i e-i s-i le-i)
                           (if (VFalse? v-i) 
                               (interp-env e e-i s-i le-i)
                               (interp-env t e-i s-i le-i))]
                     [else ians]))]

    [CId (id) (grabValue id env store lenv)]

    [CLet (id bind body) (let ([bindAns (interp-env bind env store lenv)]
                               [where (newLoc)])
                           (type-case CAns bindAns
                             [AVal (v-bind e-bind s-bind le-bind)
                                   (interp-env body
                                     (hash-set e-bind id where)
                                     (hash-set s-bind where v-bind)
                                     (hash-set le-bind id true))]
                             [else bindAns]))]

    [CSeq (e1 e2) (let ([e1Ans (interp-env e1 env store lenv)])
                    (type-case CAns e1Ans
                      [AVal (v-e1 e-e1 s-e1 le-e1) (interp-env e2 e-e1 s-e1 le-e1)]
                      [else e1Ans]))]
    
    [CApp (fun args) (let ([funAns (interp-env fun env store lenv)])
                       ;; function answer
                       (type-case CAns funAns
                         [AVal (v-f e-f s-f le-f)
                           ;; function value
                           (type-case CVal v-f
                             [VClosure (clargs clbody clenv)
                               (let ([bind-es (bind-args clargs 
                                                         (allocLocList (length (VClosure-args v-f))) 
                                                         (interpArgs args e-f s-f le-f) 
                                                         env store lenv)]) ;; extend env using global instead closure-env
                                 (type-case CAns bind-es
                                   [AVal (v-es e-es s-es le-es) (interp-env clbody e-es s-es le-es)]
                                   [else bind-es]))]
                             [else (interp-error "Not a function" e-f s-f le-f)])]
                         [else funAns]))]

    [CFunc (args body) (AVal (VClosure args body env) env store lenv)]

    [CPrim1 (prim arg) (let ([argAns (interp-env arg env store lenv)])
                         (if (AVal? argAns)
                             (python-prim1 prim argAns)
                             argAns))]
    
    ;[CPrim2 (prim arg1 arg2) (python-prim2 prim (interp-env arg env) (interp-env arg env))]
    
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
    [CObject (type exp)
             (let ([rs (interp-env exp env store (resetLocalEnv lenv))])
               (type-case CAns rs 
                 [AVal (v-rs e-rs s-rs le-rs)
                       (AVal (VObject type 
                                      (let ([rst (make-hash empty)]) 
                                        (begin (map (lambda (x) (hash-set! rst (symbol->string x) (AVal-val (grabValue x e-rs s-rs le-rs))))
                                                    (getModifiedVars le-rs)) 
                                               rst))) 
                             env store lenv)]
                 
                 [else rs]))]
    [else (begin (display expr)
                 (error 'interp "no case"))]))

(define (resetLocalEnv (oldEnv : LocalEnv)) :  LocalEnv
  (foldl (lambda (x result) (hash-set result x false)) (hash empty) (hash-keys oldEnv)))

(define (getModifiedVars (oldEnv : LocalEnv)) : (listof symbol)
  (filter (lambda (x) (some-v (hash-ref oldEnv x))) (hash-keys oldEnv)))
      
#|
(define (make-ids (n : number)) : (listof symbol)
  (build-list n (lambda (n) (string->symbol (string-append "var" (to-string n))))))

;; cascade-lets will build up the nested lets, and use body as the
;; eventual body, preserving order of evaluation of the expressions
(define (cascade-lets (ids : (listof symbol))
                      (exprs : (listof ExprC))
                      (body : ExprC)) : ExprC
  (cond [(empty? ids) body]
        [(cons? ids)
         (LetC (first ids) (first exprs) (cascade-lets (rest ids) (rest exprs) body))]))
(local ([define ids (make-ids (length args))]
          [define id-exps (map IdC ids)])
    (cascade-lets ids (map desugar args)
|#

(define (bind-args (args : (listof symbol)) (locs : (listof Location)) (anss : (listof CAns)) (env : Env) (sto : Store) (lenv : LocalEnv)) : CAns
  (cond [(and (empty? args) (empty? anss)) (AVal (VStr "dummy") env sto lenv)]
        [(or (empty? args) (empty? anss))
         (interp-error "Arity mismatch" env sto lenv)]
        [(= (length args) (length anss))
         (let ([lastAns (first (reverse anss))])
           (if (AVal? lastAns)
               (AVal (VStr "dummy") 
                     (extendEnv args locs (AVal-env lastAns)) ;; use passin env instead of closure-env
                     (overrideStore locs anss (AVal-sto lastAns))
                     (AVal-lenv lastAns))
               lastAns))]))

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