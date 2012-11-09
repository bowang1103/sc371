#lang plai-typed

(require "python-core-syntax.rkt")

(define (to-str-obj [val : CVal]) : CExp
  (CLet 'newObj (CObject "Str" val (CEmpty))
        (let ([builtin-lst (map (lambda (key) (CSetfield (CId 'newObj) key (some-v (hash-ref str-hash key))))
                                (hash-keys str-hash))])
          (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId 'newObj)))))

(define (to-int-obj [val : CVal]) : CExp
  (CLet 'newObj (CObject "Int" val (CEmpty))
        (let ([builtin-lst (map (lambda (key) (CSetfield (CId 'newObj) key (some-v (hash-ref int-hash key))))
                                (hash-keys int-hash))])
          (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId 'newObj)))))

(define str-hash 
         (hash 
          (list (values "%add"
                        (CFunc (list 'self 'right)
                               (CPrim2 'str+
                                       (CId 'self)
                                       (CId 'right))))
                )))

(define int-hash 
         (hash 
          (list (values "%add"
                        (CFunc (list 'self 'right)
                               (CPrim2 'num+
                                       (CId 'self)
                                       (CId 'right))))
                )))


(define ($to-object (val : CVal)) : CExp
  (type-case CVal val
    [VNum (n) (to-int-obj val)]
    ;[VTrue () (to-bool-obj (VNum 1))]
    ;[VFalse () (to-bool-obj (VNum 0))]
    [VStr (s) (to-str-obj val)]
    ;;[VError (exn) (exn-type exn)]
    [else (error 'to-object "not implemented object yet")]))