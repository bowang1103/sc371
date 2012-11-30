#lang plai-typed

(require "python-core-syntax.rkt")

(define getId
  (local ([define n (box 0)])
    (lambda () 
      (begin
        (set-box! n (add1 (unbox n)))
        (string->symbol (string-append "newObj" (to-string (unbox n))))))))

(define (to-str-obj [prim : CExp]) : CExp
  (let ([id (getId)]) 
    (CLet id (CObject "Str" prim (CEmpty))
       (CLet 'self (CId id)
          (let ([builtin-lst (map (lambda (key) (CSetfield (CId id) key (some-v (hash-ref str-hash key))))
                                  (hash-keys str-hash))])
            (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId id)))))))

(define (to-int-obj [prim : CExp]) : CExp
  (let ([id (getId)]) 
    (CLet id (CObject "Int" prim (CEmpty))
       (CLet 'self (CId id)
          (let ([builtin-lst (map (lambda (key) (CSetfield (CId id) key (some-v (hash-ref num-hash key))))
                                  (hash-keys num-hash))])
            (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId id)))))))

(define (to-float-obj [prim : CExp]) : CExp
  (let ([id (getId)]) 
    (CLet id (CObject "Float" prim (CEmpty))
       (CLet 'self (CId id)
          (let ([builtin-lst (map (lambda (key) (CSetfield (CId id) key (some-v (hash-ref num-hash key))))
                                  (hash-keys num-hash))])
            (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId id)))))))

(define (to-bool-obj [prim : CExp]) : CExp
  (let ([id (getId)]) 
    (CLet id (CObject "Bool" prim (CEmpty))
       (CLet 'self (CId id)
          (let ([builtin-lst (map (lambda (key) (CSetfield (CId id) key (some-v (hash-ref bool-hash key))))
                                  (hash-keys bool-hash))])
            (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId id)))))))

(define (to-list-obj [prim : CExp]) : CExp
  (let ([id (getId)]) 
    (CLet id (CObject "List" prim (CEmpty))
       (CLet 'self (CId id)
          (let ([builtin-lst (map (lambda (key) (CSetfield (CId id) key (some-v (hash-ref list-hash key))))
                                  (hash-keys list-hash))])
            (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId id)))))))

(define (to-tuple-obj [prim : CExp]) : CExp
  (let ([id (getId)]) 
    (CLet id (CObject "Tuple" prim (CEmpty))
       (CLet 'self (CId id)
          (let ([builtin-lst (map (lambda (key) (CSetfield (CId id) key (some-v (hash-ref tuple-hash key))))
                                  (hash-keys tuple-hash))])
            (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId id)))))))

(define (to-set-obj [prim : CExp]) : CExp
  (let ([id (getId)]) 
    (CLet id (CObject "Set" prim (CEmpty))
       (CId id))))

(define (to-dict-obj [prim : CExp]) : CExp
  (let ([id (getId)]) 
    (CLet id (CObject "Dict" prim (CEmpty))
       (CLet 'self (CId id)
          (let ([builtin-lst (map (lambda (key) (CSetfield (CId id) key (some-v (hash-ref dict-hash key))))
                                  (hash-keys dict-hash))])
            (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId id)))))))

(define (to-range-obj [prim : CExp]) : CExp
  (let ([id (getId)]) 
    (CLet id (CObject "Range" prim (CEmpty))
       (CLet 'self (CId id)
          (let ([builtin-lst (map (lambda (key) (CSetfield (CId id) key (some-v (hash-ref range-hash key))))
                                  (hash-keys range-hash))])
            (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId id)))))))

(define (to-iter-obj [prim : CExp]) : CExp
  (let ([id (getId)]) 
    (CLet id (CObject "Iter" prim (CEmpty))
       (CLet 'self (CId id)
          (let ([builtin-lst (map (lambda (key) (CSetfield (CId id) key (some-v (hash-ref iter-hash key))))
                                  (hash-keys iter-hash))])
            (CSeq (foldl (lambda (e1 e2) (CSeq e2 e1)) (first builtin-lst) (rest builtin-lst)) (CId id)))))))

(define (to-func-obj [prim : CExp]) : CExp
  (let ([id (getId)]) 
    (CLet id (CObject "Func" prim (CEmpty))
          (CId id))))

(define (to-exc-obj [prim : CExp]) : CExp
  (let ([id (getId)]) 
    (CLet id (CObject "Exception" prim (CEmpty))
          (CId id))))

(define (to-empty-obj [prim : CExp]) : CExp
  (let ([id (getId)]) 
    (CLet id (CObject "None" prim (CEmpty))
          (CId id))))

(define ($to-object (prim : CExp)) : CExp
  (type-case CExp prim
    [CNum (n) (if (isInteger n) (to-int-obj prim) (to-float-obj prim))]
    [CTrue () (to-bool-obj (CNum 1))]
    [CFalse () (to-bool-obj (CNum 0))]
    [CStr (s) (to-str-obj prim)]
    [CList (es) (to-list-obj prim)]
    [CTuple (es) (to-tuple-obj prim)]
    [CRange (range) (to-range-obj prim)]
    [CSetV (es) (to-set-obj prim)]
    [CDict (keys values) (to-dict-obj prim)]
    [CIter (lst) (to-iter-obj prim)]
    [CFunc (args varargs defaults body) (to-func-obj prim)]
    [CException (type message) (to-exc-obj prim)]
    [CEmpty () (to-empty-obj prim)]
    ;;[VError (exn) (exn-type exn)]
    [else (error 'to-object "not implemented object yet")]))

;; built-in methods for str
(define str-hash 
         (hash 
          (list (values "__add__"
                        ($to-object (CFunc (list 'self 'right)
                                           (list) (list)
                                           (CPrim2 '+
                                                   (CId 'self)
                                                   (CId 'right)))))
                (values "__iter__"
                        ($to-object (CFunc (list 'self)
                                           (list) (list)
                                           (COperation (CId 'self) "Str" "iter" (list)))))
                )))

;; bulit-in methods for number
(define num-hash 
         (hash 
          (list (values "__add__"
                        ($to-object (CFunc (list 'self 'right)
                                           (list) (list)
                                           (CPrim2 '+
                                                   (CId 'self)
                                                   (CId 'right)))))
                )))

;; bulit-in methods for bool
(define bool-hash 
         (hash 
          (list (values "__add__"
                        ($to-object (CFunc (list 'self 'right)
                                           (list) (list)
                                           (CPrim2 '+
                                                   (CId 'self)
                                                   (CId 'right)))))
                )))

;; bulit-in methods for tuple
(define tuple-hash 
         (hash 
          (list (values "__add__"
                        ($to-object (CFunc (list 'self 'right)
                                           (list) (list)
                                           (CPrim2 '+
                                                   (CId 'self)
                                                   (CId 'right)))))
                (values "__iter__"
                        ($to-object (CFunc (list 'self)
                                           (list) (list)
                                           (COperation (CId 'self) "Tuple" "iter" (list)))))
                )))

;; bulit-in methods for list
(define list-hash 
         (hash 
          (list (values "__add__"
                        ($to-object (CFunc (list 'self 'right)
                                           (list) (list)
                                           (CPrim2 '+
                                                   (CId 'self)
                                                   (CId 'right)))))
                (values "__iter__"
                        ($to-object (CFunc (list 'self)
                                           (list) (list)
                                           (COperation (CId 'self) "List" "iter" (list)))))
                )))

;; bulit-in methods for range
(define range-hash 
         (hash 
          (list (values "__add__"
                        ($to-object (CFunc (list 'self 'right)
                                           (list) (list)
                                           (CPrim2 '+
                                                   (CId 'self)
                                                   (CId 'right)))))
                (values "__iter__"
                        ($to-object (CFunc (list 'self)
                                           (list) (list)
                                           (COperation (CId 'self) "Range" "iter" (list)))))
                )))

;; bulit-in methods for iter
(define iter-hash 
         (hash 
          (list (values "__next__"
                        ($to-object (CFunc (list 'self)
                                           (list) (list)
                                           (COperation (CId 'self) "Iter" "next" (list)))))
                (values "__iter__"
                        ($to-object (CFunc (list 'self)
                                           (list) (list)
                                           (COperation (CId 'self) "Iter" "iter" (list)))))
                )))

;; built-in methods for dict
(define dict-hash
  (hash
   (list (values "clear"
                 ($to-object (CFunc (list 'self)
                                    (list) (list)
                                    (COperation (CId 'self) "Dict" "clear" (list)))))
         (values "keys"
                 ($to-object (CFunc (list 'self)
                                    (list) (list)
                                    (COperation (CId 'self) "Dict" "keys" (list)))))
         (values "values"
                 ($to-object (CFunc (list 'self)
                                    (list) (list)
                                    (COperation (CId 'self) "Dict" "values" (list)))))
         (values "get"
                 ($to-object (CFunc (list 'self 'index 'default) (list)
                                    (list ($to-object (CEmpty)))
                                    (COperation (CId 'self) "Dict" "get" (list (CId 'index) (CId 'default))))))
         (values "update"
                 ($to-object (CFunc (list 'self 'value) (list)
                                    (list ($to-object (CSetV (list))))
                                    (COperation (CId 'self) "Dict" "update" (list (CId 'value))))))
         (values "__iter__"
                        ($to-object (CFunc (list 'self)
                                           (list) (list)
                                           (COperation (CId 'self) "Dict" "iter" (list))))))))
                                                  
(define (isInteger (n : number)) : boolean
  (if (= 0 n)
      (equal? (to-string 0) (to-string n))
      (equal? (to-string 1) (to-string (/ n n)))))
