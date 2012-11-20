#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(define-type CExp
  [CNum (n : number)]
  [CStr (s : string)]
  [CList (es : (listof CExp))]
  [CTuple (es : (listof CExp))]
  [CSetV (es : (listof CExp))]
  [CDict (keys : (listof CExp)) (values : (listof CExp))]
  [CTrue]
  [CFalse]
  [CSeq (e1 : CExp) (e2 : CExp)]
  [CError (e1 : CExp)]
  [CException (type : string) (message : CExp)]
  [CIf (test : CExp) (then : CExp) (else : CExp)]
  [CId (id : symbol)]
  [CLet (id : symbol) (bind : CExp) (body : CExp)]
  [CSet (id : symbol) (value : CExp)]
  [CDel (tg : CExp)]
  [CApp (fun : CExp) (args : (listof CExp))]
  [CFunc (args : (listof symbol)) (defaults : (listof CExp)) (body : CExp)]
  [CPrim1 (prim : symbol) (arg : CExp)]
  [CPrim2 (prim : symbol) (arg1 : CExp) (arg2 : CExp)] ;; arg1 and arg2 should be IdC
  [CPrim2Seq (left : CExp) (prims : (listof symbol)) (args : (listof CExp))]
  [CWrap (type : string) (obj : CVal)]
;  [CPrim2 (prim : symbol) (args : (listof CExp))])
  
  ;;Exception ( combine Try Except else finally)
  [CTryFinally (b : CExp) (fb : CExp)]
  [CTryExn (b : CExp) (hdlers : CExp) (else : CExp)]
  [CExceptHandler (name : CExp) (body : CExp) (type : CExp)]
  [CRaise (cause : CExp) (exc : CExp)]
    
  ;;Define Object ; CObject here is different from PT's ObjectC (lisfofField)
  [CObject (type : string) (value : CExp) (body : CExp)]  
  [CSetelement (obj : CExp) (index : CExp) (value : CExp)]
  [CGetelement (obj : CExp) (indexs : (listof CExp))]
  [CSetfield (obj : CExp) (field : string) (value : CExp)]
  [CGetfield (obj : CExp) (field : string)]
  [COperation (obj : CExp) (type : string) (op : string) (args : (listof CExp))]
  [CEmpty])

(define-type CVal
  [VNum (n : number)]
  [VStr (s : string)]
  [VList (es : (listof CVal))]
  [VTuple (es : (listof CVal))]
  [VSet (es : (hashof CVal boolean))]
  [VDict (dict : (hashof CVal CVal))]
  [VTrue]
  [VFalse]
  [VEmpty]
  [VObject (type : string) (value : CVal) (loc : Location) (field : ObjfieldV)]
  [VPoint (obj : CExp) (field : string)]
  [VException (type : string) (message : CVal)]
  [VMPoint (loc : Location)]
  [VClosure (args : (listof symbol)) (defaults : (listof CVal)) (body : CExp) (env : Env) (sto : Store)])
   
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

(define (core-error str)
  (CError (CStr str)))

(define (interp-error str env store lenv)
  (AExc (VStr str) env store lenv))

(define isImmutableTable 
  (make-hash (list (values "Int" true) (values "Float" true) (values "Bool" true) (values "Str" true) (values "Tuple" true) 
                   (values "Func" true) (values "MPoint" true) (values "Empty" true))))

(define (isImmutable (type : string)) : boolean
  (if (equal? (none) (hash-ref isImmutableTable type))
      false
      true))

