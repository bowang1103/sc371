#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

;; desugar/args2: listof PyExpr * symbol -> CExp
;; For the operator with two arguments I just put them into
;; CPrim2 to address
;(define (desugar/args2 (op : symbol) (args : (listof PyExpr))) : CExp
;  (CLet 'var1 (desugar (first args))
;        (CLet 'var2 (desugar (second args))
;              (CPrim2 op (CId 'var1) (CId 'var2)))))

;; desugar/arg: PyExpr * symbol -> CExp
;; For the operator with one arguments I just put them into
;; CPrim1 to address
;(define (desugar/arg (op : symbol) (arg : PyExpr)) : CExp
;  (CLet 'var (desugar arg)
;        (CPrim1 op (CId 'var))))

(define (desugar (expr : PyExpr)) : CExp
  (type-case PyExpr expr
    [PySeq (es) (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))]
;    [PyIf (test then els) (CIf (desugar test) (desugar then) (desugar els))]
;    [PyPrim (op args)
;            (cond 
;              [(equal? 1 (length args)) (desugar/arg op args)]
;              [(equal? 2 (length args)) (desugar/args2 op args)])]
    [PyNum (n) (CLet 'newObj (CObject "Int" (CEmpty))
                 (let ([newList (map2 (lambda (name value) (CSetfield (CId 'newObj) name value)) 
                                      (list "value")
                                      (list (CNum n)))])
                   (foldl (lambda (e1 e2) (CSeq e2 e1)) (first newList) (rest newList))))]
    [PyStr (s) (CLet 'newObj (CObject "Str" (CEmpty))
                 (let ([newList (map2 (lambda (name value) (CSetfield (CId 'newObj) name value)) 
                                      (list "value")
                                      (list (CStr s)))])
                   (foldl (lambda (e1 e2) (CSeq e2 e1)) (first newList) (rest newList))))]
    [PyApp (f args) (CApp (desugar f) (map desugar args))]
    [PyId (x) (CId x)]
    
    ;; Handling Try Exception
    ;[PyTryExcept (b hdlers els) (CTryExn (desugar b) (desugar hdlers) (desugar els) )]
    ;[PyTryFinally (b fb) (CTryFinally (desugar b) (desugar fb) )]
    ;r[PyRaise (exc) (cause)  (CRaise (desugar exc) (desugar cause))]
    [else (CNum 1)]))
