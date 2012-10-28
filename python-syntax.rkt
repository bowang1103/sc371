#lang plai-typed

;(define-type PyField
;  [pyField (name : string) (value : PyExpr)])

;(define-type LHS
;  [BracketLHS (obj : PyExpr) (field : ExprP)]
;  [DotLHS (class : PyExpr) (field :ExprP)]
;  [IdLHS (id : symbol)])

(define-type PyExpr
;  (PyFunDef (name : symbol) (ids : listof symbol) (body : PyExpr))
;  [PyRet (ret : PyExpr)]
;  [PyIf (test : PyExpr) (then : PyExpr) (els : PyExpr)]  
  [PySeq (es : (listof PyExpr))]
;  [PyPrim (op : symbol) (args : (listof PyExpr))]
  [PyNum (n : number)]
;  [PyEmp]
  [PyId (x : symbol)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))])

