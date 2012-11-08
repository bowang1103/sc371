#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(define-type CExp
  [CNum (n : number)]
  [CStr (s : string)]
  [CTrue]
  [CFalse]
  [CSeq (e1 : CExp) (e2 : CExp)]
  [CError (e1 : CExp)]
  [CIf (test : CExp) (then : CExp) (else : CExp)]
  [CId (id : symbol)]
  [CLet (id : symbol) (bind : CExp) (body : CExp)]
  [CSet (id : symbol) (value : CExp)]
  [CApp (fun : CExp) (args : (listof CExp))]
  [CFunc (args : (listof symbol)) (body : CExp)]
  [CPrim1 (prim : symbol) (arg : CExp)]
  [CPrim2 (prim : symbol) (arg1 : CExp) (arg2 : CExp)] ;; arg1 and arg2 should be IdC
  [CPrim2Seq (left : CExp) (prims : (listof symbol)) (args : (listof CExp))]
;  [CPrim2 (prim : symbol) (args : (listof CExp))])
  
  ;;Exception ( combine Try Except else finally)
  ;[CTryFinally (b : CExp) (fb : CExp)]
  ;[CTryExn (b : CExp) (hdlers : CExp) (else : CExp)]
  ;[CRaise (exc : CExp) (cause : CExp)]
    
  ;;Define Object ; CObject here is different from PT's ObjectC (lisfofField)
  [CObject (type : string) (body : CExp)]  
  [CSetfield (obj : CExp) (field : string) (value : CExp)]
  [CGetfield (obj : CExp) (field : string)]
  [CEmpty])

(define-type CVal
  [VNum (n : number)]
  [VStr (s : string)]
  [VTrue]
  [VFalse]
  [VEmpty]
  [VObject (type : string) (field : ObjfieldV)]
  [VClosure (args : (listof symbol)) (body : CExp) (env : Env)])
   



(define-type CAns 
  [AVal (val : CVal) (env : Env) (sto : Store) (lenv : LocalEnv)]
  [AExc (exc : CVal) (env : Env) (sto : Store) (lenv : LocalEnv)])

(define-type-alias Location number)
(define-type-alias Env (hashof symbol Location))
(define-type-alias Store (hashof Location CVal))

;; LocalEnv only works when interping the class, when Assigning the 
;; variable in Class definition, we'll set boolean to True; (defualt is False)
(define-type-alias LocalEnv (hashof symbol boolean))
(define-type-alias ObjfieldV (hashof string CVal))

(define (interp-error str env store lenv)
  (AExc (VStr str) env store lenv))

