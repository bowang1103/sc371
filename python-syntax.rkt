#lang plai-typed

(define-type PyExpr
  [PySubscript (obj : PyExpr) (indexs : (listof PyExpr))]
  [PySeq (es : (listof PyExpr))]
  [PyId (x : symbol)]
  
  ;; added by hichuang
  
  ;;;;;; stmt ;;;;;;
  [PyFuncDef (name : symbol) (args : PyExpr) (body : PyExpr) (return : PyExpr)]
  [PyDel (targets : (listof PyExpr))]
  [PyAssign (targets : (listof PyExpr)) (value : PyExpr)]
  
  ; for num in range(2, 10): 
  ;     body.... 
  ;         break; 
  ; else: ... 
  ; ### if not terminate by break in body go to else
  [PyFor (target : PyExpr) (iter : PyExpr) (body : PyExpr) (orelse : PyExpr)]
  [PyWhile (test : PyExpr) (body : PyExpr) (orelse : PyExpr)]
  [PyIf (test : PyExpr) (body : PyExpr) (orelse : PyExpr)]
  
  ;;;;;; exceptions ;;;;;;
  [PyRaise (cause : PyExpr) (exc : PyExpr)]
  [PyTryExcept (body : PyExpr) (handlers : PyExpr) (orelse : PyExpr)] ;;???
  [PyTryFinally (body : PyExpr) (finalbody : PyExpr)]
  [PyExceptHandler (name : PyExpr) (body : PyExpr) (type : PyExpr)]
  
  [PyGlobal (names : (listof symbol))]
  [PyNonlocal (names : (listof symbol))]
  
  ;;;;;; expr ;;;;;;
  ; boolop = {and, or}
  [PyBoolOp (boolop : symbol) (values : (listof PyExpr))]
  ; op = {+, -, *, /, %, **, <<, >>, bor, ^, band, //}
  [PyBinOp (left : PyExpr) (op : symbol) (right : PyExpr)]
  ; unaryop = {~, not, pos, neg} ~must be int, pos and neg should be numeric, not ?
  [PyUnaryOp (unaryop : symbol) (operand : PyExpr)]
  ; op = {==, !=, <, <=, >, >=, is, !is, in, !in} a < b < c => a < b and b < c
  [PyCompare (left : PyExpr) (ops : (listof symbol)) (comparators : (listof PyExpr))]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))]
  
  ;;;;;; ds ;;;;;;
  [PyDict (keys : (listof PyExpr)) (values : (listof PyExpr))]
  [PySet (elts : (listof PyExpr))]
  ; mutable
  [PyList (elts : (listof PyExpr))]
  ; immutable
  [PyTuple (elts : (listof PyExpr))]
  [PyNum (n : number)]
  [PyStr (s : string)]
  
  [PyArgs (args : (listof symbol)) (defaults : (listof PyExpr))]
  
  
  
;  (PyFunDef (name : symbol) (ids : listof symbol) (body : PyExpr))
  [PyReturn (ret : PyExpr)]
  [PyEmp]
  [PyBreak]

  ;;;; false value ;;;;
  ; None False (zero of any number type) (empty sequence () [] "") (empty mapping {}) (obj ___bool___ or ___len___ return false or 0)
  )
