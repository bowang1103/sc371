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
    [PyIf (test then els) (CIf (desugar test) (desugar then) (desugar els))]
;    [PyPrim (op args)
;            (cond 
;              [(equal? 1 (length args)) (desugar/arg op args)]
;              [(equal? 2 (length args)) (desugar/args2 op args)])]
    [PyNum (n) (CNum n)]
    [PyApp (f args) (CApp (desugar f) (map desugar args))]
    [PyId (x) (CId x)]
    
    ; and or
    [PyBoolOp (boolop exprs) (desugar-boolop boolop exprs)]
    ; ==, !=, <, <=, >, >=, is, !is, in, !in : a < b < c => a < b and b < c
    [PyCompare (left ops comparators) (desugar-compare left ops comparators)]
    ; +, -, *, /, %, **, <<, >>, bor, ^, band, //
    [PyBinOp (left binop right) (CPrim2 binop (desugar left) (desugar right))]
    ; unaryop = {~, not, pos, neg} ~must be int, pos and neg should be numeric, not ?
    [PyUnaryOp (unaryop operand) (CPrim1 unaryop (desugar operand))]
    
    [else (CError (CStr "no case yet"))]))

(define (make-ids (n : number)) : (listof symbol)
  (build-list n (lambda (n) (string->symbol (string-append "_tmpvar" (to-string n))))))

(define (last l)
  (first (reverse l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; desugar for compare ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (desugar-compare (left : PyExpr) (ops : (listof symbol)) (comparators : (listof PyExpr))) : CExp
  (CStr "dummy"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; desugar for boolop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (desugar-boolop (boolop : symbol) (exprs : (listof PyExpr))) : CExp
  (letrec ([ids (make-ids (length exprs))]
           [cexps (map desugar exprs)])
    (case boolop
      [(and) (desugar-and ids cexps (last ids))]
      [(or) (desugar-or ids cexps (last ids))])))

(define (desugar-and (ids : (listof symbol)) (cexprs : (listof CExp)) (lastId : symbol)) : CExp
  (cond [(empty? ids) (CId lastId)]
        [else (CLet (first ids) (first cexprs)
                    (CIf (CId (first ids))
                         (desugar-and (rest ids) (rest cexprs) lastId)
                         (CId (first ids))))]))

(define (desugar-or (ids : (listof symbol)) (cexprs : (listof CExp)) (lastId : symbol)) : CExp
  (cond [(empty? ids) (CId lastId)] 
        [else (CLet (first ids) (first cexprs)
                    (CIf (CId (first ids))
                         (CId (first ids))
                         (desugar-and (rest ids) (rest cexprs) lastId)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;