#lang plai-typed

(require "python-core-syntax.rkt")

(define (to-str-obj [prim : CExp]) : CExp
  (CLet 'newObj (CObject "Str" prim (CEmpty))
        (let ([builtin-lst (map (lambda (key) (CSetfield (CId 'newObj) key (some-v (hash-ref str-hash key))))
                                (hash-keys str-hash))])
          (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId 'newObj)))))

(define (to-int-obj [prim : CExp]) : CExp
  (CLet 'newObj (CObject "Int" prim (CEmpty))
        (let ([builtin-lst (map (lambda (key) (CSetfield (CId 'newObj) key (some-v (hash-ref num-hash key))))
                                (hash-keys num-hash))])
          (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId 'newObj)))))

(define (to-float-obj [prim : CExp]) : CExp
  (CLet 'newObj (CObject "Float" prim (CEmpty))
        (let ([builtin-lst (map (lambda (key) (CSetfield (CId 'newObj) key (some-v (hash-ref num-hash key))))
                                (hash-keys num-hash))])
          (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId 'newObj)))))

(define (to-bool-obj [prim : CExp]) : CExp
  (CLet 'newObj (CObject "Bool" prim (CEmpty))
        (let ([builtin-lst (map (lambda (key) (CSetfield (CId 'newObj) key (some-v (hash-ref bool-hash key))))
                                (hash-keys bool-hash))])
          (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId 'newObj)))))

(define (to-list-obj [prim : CExp]) : CExp
  (CLet 'newObj (CObject "List" prim (CEmpty))
        (CId 'newObj)))

(define (to-tuple-obj [prim : CExp]) : CExp
  (CLet 'newObj (CObject "Tuple" prim (CEmpty))
        (CId 'newObj)))

(define (to-dict-obj [prim : CExp]) : CExp
  (CLet 'newObj (CObject "Dict" prim (CEmpty))
        (CId 'newObj)))

(define (to-func-obj [prim : CExp]) : CExp
  (CLet 'newObj (CObject "Func" prim (CEmpty))
        (CId 'newObj)))

(define (to-exc-obj [prim : CExp]) : CExp
  (CLet 'newObj (CObject "Exception" prim (CEmpty))
	    (CId 'newObj)))


;; built-in methods for str
(define str-hash 
         (hash 
          (list (values "%add"
                        (CFunc (list 'self 'right)
                               (CPrim2 '+
                                       (CId 'self)
                                       (CId 'right))))
                )))

;; bulit-in methods for number
(define num-hash 
         (hash 
          (list (values "%add"
                        (CFunc (list 'self 'right)
                               (CPrim2 '+
                                       (CId 'self)
                                       (CId 'right))))
                )))

;; bulit-in methods for bool
(define bool-hash 
         (hash 
          (list (values "%add"
                        (CFunc (list 'self 'right)
                               (CPrim2 '+
                                       (CId 'self)
                                       (CId 'right))))
                )))

(define (isInteger (n : number)) : boolean
  (if (= 0 n)
      (equal? (to-string 0) (to-string n))
      (equal? (to-string 1) (to-string (/ n n)))))

(define ($to-object (prim : CExp)) : CExp
  (type-case CExp prim
    [CNum (n) (if (isInteger n) (to-int-obj prim) (to-float-obj prim))]
    [CTrue () (to-bool-obj (CNum 1))]
    [CFalse () (to-bool-obj (CNum 0))]
    [CStr (s) (to-str-obj prim)]
    [CList (es) (to-list-obj prim)]
    [CTuple (es) (to-tuple-obj prim)]
    [CDict (keys values) (to-dict-obj prim)]
    [CFunc (args body) (to-func-obj prim)]
    [CException (type message) (to-exc-obj prim)]
    ;;[VError (exn) (exn-type exn)]
    [else (error 'to-object "not implemented object yet")]))
