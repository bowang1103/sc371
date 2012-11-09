#lang plai-typed

(require "python-core-syntax.rkt")

(define (to-str-obj [prim : CExp]) : CExp
  (CLet 'newObj (CObject "Str" prim (CEmpty))
        (let ([builtin-lst (map (lambda (key) (CSetfield (CId 'newObj) key (some-v (hash-ref str-hash key))))
                                (hash-keys str-hash))])
          (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId 'newObj)))))

(define (to-int-obj [prim : CExp]) : CExp
  (CLet 'newObj (CObject "Int" prim (CEmpty))
        (let ([builtin-lst (map (lambda (key) (CSetfield (CId 'newObj) key (some-v (hash-ref int-hash key))))
                                (hash-keys int-hash))])
          (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId 'newObj)))))

(define (to-bool-obj [prim : CExp]) : CExp
  (CLet 'newObj (CObject "Bool" prim (CEmpty))
        (let ([builtin-lst (map (lambda (key) (CSetfield (CId 'newObj) key (some-v (hash-ref bool-hash key))))
                                (hash-keys bool-hash))])
          (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId 'newObj)))))

;; built-in methods for str
(define str-hash 
         (hash 
          (list (values "%add"
                        (CFunc (list 'self 'right)
                               (CPrim2 'str+
                                       (CId 'self)
                                       (CId 'right))))
                )))

;; bulit-in methods for int
(define int-hash 
         (hash 
          (list (values "%add"
                        (CFunc (list 'self 'right)
                               (CPrim2 'num+
                                       (CId 'self)
                                       (CId 'right))))
                )))

;; bulit-in methods for bool
(define bool-hash 
         (hash 
          (list (values "%dummy"
                        (CFunc (list 'self 'right)
                               (CPrim2 'num+
                                       (CId 'self)
                                       (CId 'right))))
                )))


(define ($to-object (prim : CExp)) : CExp
  (type-case CExp prim
    [CNum (n) (to-int-obj prim)]
    [CTrue () (to-bool-obj (CNum 1))]
    [CFalse () (to-bool-obj (CNum 0))]
    [CStr (s) (to-str-obj prim)]
    ;;[VError (exn) (exn-type exn)]
    [else (error 'to-object "not implemented object yet")]))