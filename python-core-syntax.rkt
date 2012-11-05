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
  [CPrim2Seq (left : CExp) (prims : (listof symbol)) (args : (listof CExp))])
;  [CPrim2 (prim : symbol) (args : (listof CExp))])

(define-type CVal
  [VNum (n : number)]
  [VStr (s : string)]
  [VTrue]
  [VFalse]
  [VClosure (args : (listof symbol)) (body : CExp) (env : Env)])

(define-type CAns 
  [AVal (val : CVal) (env : Env) (sto : Store)]
  [AExc (exc : CVal) (env : Env) (sto : Store)])

(define-type-alias Location number)
(define-type-alias Env (hashof symbol Location))
(define-type-alias Store (hashof Location CVal))
(define-type-alias CValue (hashof VName ))
(define-type-alias CMethod (hashof ))

(define (interp-error str env store)
  (AExc (VStr str) env store))

