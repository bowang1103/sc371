#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "python-objects.rkt")

(define (desugar (expr : PyExpr)) : CExp
  (type-case PyExpr expr
    [PyClassDef (obj name) (CEmpty)]
    [PyAttr (obj attr) (CLet 'self (desugar obj)
                             (CGetfield (CId 'self) attr))]
    [PyAssign (tgs val)
              (CLet 'value (desugar val)
                    (let ([rst 
                           (map2 (lambda (tar newval) 
                                   (type-case PyExpr tar
                                    [PySubscript (obj indexs) 
                                                 (if (equal? (length indexs) 1)
                                                     (CSetelement (desugar obj)
                                                                  (desugar (first indexs)) 
                                                                  (if (equal? newval (PyEmp)) 
                                                                      (CId 'value)
                                                                      (desugar newval)))
                                                     (CEmpty))]
                                     [PyId (id) (CSet id (if (equal? newval (PyEmp)) (CId 'value) (desugar newval)))]
                                     [else (CEmpty)])) (reverse tgs) (cons (PyEmp) (reverse (rest tgs))))])
                      (foldl (lambda (e1 e2) (CSeq e2 e1)) (first rst) (rest rst))))]
    [PyDel (targets)
           (if (equal? 1 (length targets))
               (CDel (desugar (first targets)))
               (let ([tgs (map desugar targets)]) 
                 (foldl (lambda (e1 e2) (CSeq e2 e1)) (first tgs) (rest tgs))))]
    [PyAugAssign (op target value) (desugar (PyAssign (list target) (PyBinOp target op value)))]
    [PySubscript (obj indexs) (cond 
                                [(or (equal? (length indexs) 1) (equal? (length indexs) 3))
                                 (CGetelement (desugar obj) (map desugar indexs))]
                                [else (CEmpty)])]
    [PySeq (es) (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))]
    [PyIf (test then els) (CIf (desugar test) (desugar then) (desugar els))]
    [PyEmp () ($to-object (CEmpty))]
    [PyNum (n) ($to-object (CNum n))]
    [PyStr (s) ($to-object (CStr s))]
    [PyList (es) ($to-object (CList (map desugar es)))]
    [PyTuple (es) ($to-object (CTuple (map desugar es)))]
    [PySet (es) ($to-object (CSetV (map desugar es)))]
    [PyDict (keys values) ($to-object (CDict (map desugar keys) (map desugar values)))]
    [PyApp (f args) (CApp (desugar f) (map desugar args))]
    [PyFunc (args body) 
            (type-case PyExpr args
              [PyArgs (args defaults) ($to-object (CFunc args (map desugar defaults) (desugar body)))]
              [else (core-error "shouldn't came here")])]
    ;[PyArgs (args defaults) (args (map desugar defaults))]
    [PyId (x) (CId x)]
    
    ; and or
    [PyBoolOp (boolop exprs) (desugar-boolop boolop exprs)]
    ; ==, !=, <, <=, >, >=, is, !is, in, !in : a < b < c => a < b and b < c
    [PyCompare (left ops comparators) (desugar-compare left ops comparators)]
    ; +, -, *, /, %, **, <<, >>, bor, bxor, band, //
    [PyBinOp (left binop right) (CPrim2 binop (desugar left) (desugar right))]
    ; unaryop = {~, not, pos, neg} ~must be int, pos and neg should be numeric, not ?
    [PyUnaryOp (unaryop operand) (CPrim1 unaryop (desugar operand))]
    
	;; Handling Try Exception
    [PyTryExcept (b hdlers els) (CTryExn (desugar b) (desugar hdlers) (desugar els) )]
    [PyExceptHandler (name body type) (CExceptHandler (desugar name) (desugar body) (desugar type))]   
    [PyTryFinally (b fb) (CTryFinally (desugar b) (desugar fb) )]
    [PyRaise (cause exc) (CRaise (desugar cause) (desugar exc))]
    [else (CNum 10)]))

(define (make-ids (n : number)) : (listof symbol)
  (build-list n (lambda (n) (string->symbol (string-append "_tmpvar" (to-string n))))))

(define (last l)
  (first (reverse l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; desugar for compare ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (desugar-compare (left : PyExpr) (ops : (listof symbol)) (comparators : (listof PyExpr))) : CExp
  (let ([ids (make-ids (add1 (length comparators)))]
        [cexps (map desugar comparators)])
    (CLet (first ids) (desugar left)
          (desugar-compare-recur ops ids cexps))))

(define (desugar-compare-recur (ops : (listof symbol)) (ids : (listof symbol)) (cexprs : (listof CExp))) : CExp
  (cond [(empty? cexprs) (CId 'True)]
        [else (CLet (second ids) (first cexprs)
                    (CIf (CPrim2 (first ops) (CId (first ids)) (CId (second ids)))
                         (desugar-compare-recur (rest ops) (rest ids) (rest cexprs))
                         (CId 'False)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; desugar for boolop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (desugar-boolop (boolop : symbol) (exprs : (listof PyExpr))) : CExp
  (let ([ids (make-ids (length exprs))]
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
