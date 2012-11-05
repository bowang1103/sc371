#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt")

(define (interp-env [expr : CExp] [env : Env] [store : Store]) : CAns
  (type-case CExp expr
    [CNum (n) (AVal (VNum n) env store)]
    [CStr (s) (AVal (VStr s) env store)]
    [CTrue () (AVal (VTrue) env store)]
    [CFalse () (AVal (VFalse) env store)]

    [CError (e) (let ([ans (interp-env e env store)])
                  (AExc (AVal-val ans) (AVal-env ans) (AVal-sto ans)))]

    [CIf (i t e) (let ([ians (interp-env i env store)])
                   (type-case CAns ians
                     [AVal (v-i e-i s-i)
                           (if (VFalse? v-i) 
                               (interp-env e e-i s-i)
                               (interp-env t e-i s-i))]
                     [else ians]))]

    [CId (id) (grab id env store)]

    [CLet (id bind body) (let ([bindAns (interp-env bind env store)]
                               [where (newLoc)])
                           (type-case CAns bindAns
                             [AVal (v-bind e-bind s-bind)
                                   (interp-env body
                                     (hash-set e-bind id where)
                                     (hash-set s-bind where v-bind))]
                             [else bindAns]))]

    [CSeq (e1 e2) (let ([e1Ans (interp-env e1 env store)])
                    (type-case CAns e1Ans
                      [AVal (v-e1 e-e1 s-e1) (interp-env e2 e-e1 s-e1)]
                      [else e1Ans]))]
    
    [CApp (fun args) (let ([funAns (interp-env fun env store)])
                       ;; function answer
                       (type-case CAns funAns
                         [AVal (v-f e-f s-f)
                           ;; function value
                           (type-case CVal v-f
                             [VClosure (clargs clbody clenv)
                               (let ([bind-es (bind-args clargs 
                                                         (allocLocList (length (VClosure-args v-f))) 
                                                         (interpArgs args e-f s-f) 
                                                         env store)]) ;; extend env using global instead closure-env
                                 (type-case CAns bind-es
                                   [AVal (v-es e-es s-es) (interp-env clbody e-es s-es)]
                                   [else bind-es]))]
                             [else (interp-error "Not a function" e-f s-f)])]
                         [else funAns]))]

    [CFunc (args body) (AVal (VClosure args body env) env store)]

    [CPrim1 (prim arg) (let ([argAns (interp-env arg env store)])
                         (if (AVal? argAns)
                             (python-prim1 prim argAns)
                             argAns))]
    
    [CPrim2 (prim arg1 arg2) (python-prim2 prim (interp-env arg1 env store) (interp-env arg2 env store))]
    
    [else (error 'interp "no case")]
    ))

(define (bind-args (args : (listof symbol)) (locs : (listof Location)) (anss : (listof CAns)) (env : Env) (sto : Store)) : CAns
  (cond [(and (empty? args) (empty? anss)) (AVal (VStr "dummy") env sto)]
        [(or (empty? args) (empty? anss))
         (interp-error "Arity mismatch" env sto)]
        [(= (length args) (length anss))
         (let ([lastAns (first (reverse anss))])
           (if (AVal? lastAns)
               (AVal (VStr "dummy") 
                     (extendEnv args locs (AVal-env lastAns)) ;; use passin env instead of closure-env
                     (overrideStore locs anss (AVal-sto lastAns)))
               lastAns))]))

(define (interp expr)
  (interp-env expr (hash (list)) (hash (list))))

(define (grab (for : symbol) (env : Env) (sto : Store)) : CAns
  (type-case (optionof Location) (hash-ref env for)
    [some (loc) 
          (type-case (optionof CVal) (hash-ref sto loc)
            [some (v) (AVal v env sto)]
            [none () (interp-error "Unbound identifier" env sto)])]
    [none () (interp-error "location not found in store" env sto)]))

;; get a new memory addr
(define newLoc
  (local ([define n (box 0)])
    (lambda () 
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

; take in a list of CExp (func args) and interp them to a list of answer
(define (interpArgs [args : (listof CExp)] [env : Env] [store : Store]) : (listof CAns)
  (cond
    [(empty? args) empty]
    [else (let ([argAns (interp-env (first args) env store)])
            (type-case CAns argAns
              [AVal (v-first e-first s-first)
                      (cons argAns (interpArgs (rest args) e-first s-first))]
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

