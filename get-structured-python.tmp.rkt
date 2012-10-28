#lang plai

(require "python-syntax.rkt")
(require racket/match
         racket/list)

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

|#

(define (get-structured-python pyjson)
  (match pyjson
    [_ (error 'parse "Haven't handled a case yet")]))


(define (get-structured-python pyjson) 
  (match pyjson
    [(hash-table ('nodetype "Module") 
                 ('body expr-list))
     (PySeq (map get-structured-python expr-list))]
    [(hash-table ('nodetype "FunctionDef") 
                 ('name name) 
                 ('args args) 
                 ('decorator_list dl) ;; ignoring decorator_list for now
                 ('body body)
                 ('returns returns))
     (PyFunDef name 
               (get-structured-python args) 
               (get-structured-python body) 
               (if (null? returns) PyEmp (get-structured-python returns)))]
;    [(hash-table ('nodetype "ClassDef"))]
    [(hash-table ('nodetype "Return") ('value value))
     (if (null? value)
         (PyReturn PyEmp)
         (PyReturn (get-structured-python value)))]
    [(hash-table ('nodetype "Delete")
                 ('targets targets))
     (PyDel (map get-structured-python targets))]
;    [(hash-table ('nodetype "Assign") ('targets expr-list) ('value expr))]
;    [(hash-table ('nodetype "AugAssign"))]
;    [(hash-table ('nodetype "Print"))]
    [(hash-table ('nodetype "For")
                 ('target target)
                 ('iter iter)
                 ('body body)
                 ('orelse orelse))
     (PyFor (get-structured-python target) 
            (get-structured-python iter) 
            (PySeq (map get-structured-python body))
            (PySeq (map get-structured-python orelse)))]
    [(hash-table ('nodetype "While")
                 ('test test)
                 ('body body)
                 ('orelse orelse))
     (PyWhile (get-structured-python test) 
              (PySeq (map get-structured-python body)) 
              (PySeq (map get-structured-python orelse)))]
    [(hash-table ('nodetype "If") ('test test) ('body body) ('orelse orelse))
     (PyIf (get-structured-python test) 
           (PySeq (map get-structured-python body)) 
           (PySeq (map get-structured-python orelse)))]
;    [(hash-table ('nodetype "With"))]
    [(hash-table ('nodetype "Raise") 
                 ('cause cause) ;; I don't know the meaning of it
                 ('exc exc))
     (PyRaise (if (null? exc)
                  PyEmp
                  (get-structured-python exc))
              (if (null? cause)
                  PyEmp
                  (get-structured-python cause)))]
    [(hash-table ('nodetype "TryExcept")
                 ('body body)
                 ('handlers handlers)
                 ('orelse orelse))
     (PyTryExcept (PySeq (map get-structured-python body)) 
                  (PySeq (map get-structured-python handlers))
                  (PySeq (map get-structured-python orelse)))]    
    [(hash-table ('nodetype "Finally")
                 ('body body)
                 ('finalbody finalbody))
     (PyFinally (PySeq (map get-structured-python body)) 
                (PySeq (map get-structured-python finalbody)))]
;    [(hash-table ('nodetype "Assert"))]
;    [(hash-table ('nodetype "Import"))]
;    [(hash-table ('nodetype "ImportFrom"))]
;    [(hash-table ('nodetype "Exec"))]
    [(hash-table ('nodetype "Global")
                 ('names names))
     (PyGlobal (map string->symbol names))]
    [(hash-table ('nodetype "Nonlocal")
                 ('names names))
     (PyNonlocal (map string->symbol names))]
    [(hash-table ('nodetype "Expr") ('value expr))
     (get-structured-python expr)]
    [(hash-table ('nodetype "Pass")) PyEmp]
    [(hash-table ('nodetype "Break")) PyBreak]
;    [(hash-table ('nodetype "Continue"))]
    [(hash-table ('nodetype "BoolOp") ('value value) ('op op))
     (PyBoolOp (get-structured-python op) 
               (map get-structured-python value))]
    [(hash-table ('nodetype "BinOp") ('op op) ('left left) ('right right))
     (PyBinOp (get-structured-python left) 
              (get-structured-python op) 
              (get-structured-python right))]
    [(hash-table ('nodetype "UnaryOp") ('op op) ('operand operand))
     (PyUnaryOp (get-structured-python op) 
                (get-structured-python operand))]
;    [(hash-table ('nodetype "Lam:qbda"))]
;    [(hash-table ('nodetype "IfExp"))]
    [(hash-table ('nodetype "Dict")
                 ('key keys)
                 ('value values))
     (PyDict (map get-structured-python keys) 
             (map get-structured-python values))]
    [(hash-table ('nodetype "Set")
                 ('elts elts))
     (PySet (map get-structured-python elts))]
;    [(hash-table ('nodetype "ListComp"))]
;    [(hash-table ('nodetype "SetComp"))]
;    [(hash-table ('nodetype "DictComp"))]
;    [(hash-table ('nodetype "GeneratorExp"))]
;    [(hash-table ('nodetype "Yield"))]
    [(hash-table ('nodetype "Compare") 
                 ('left left) 
                 ('ops ops)
                 ('comparators comp))
     (PyCompare (get-structured-python left) 
                (map get-structured-python ops) 
                (map get-structured-python comp))]
    [(hash-table ('nodetype "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list))]
;    [(hash-table ('nodetype "Repr"))]
    [(hash-table ('nodetype "Num") ('n n))
     (PyNum n)]
    [(hash-table ('nodetype "Str") ('s s))
     (PyStr s)]
;    [(hash-table ('nodetype "Attribute"))]
    [(hash-table ('nodetype "Subscript")
                 ('value value)
                 ('slice slice)
                 ('ctx ctx))
     (PySubscript (get-structured-python value)
                  (get-structured-python slice))]
    [(hash-table ('nodetype "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id id))
     (PyId (string->symbol id))]
    [(hash-table ('nodetype "List")
                 ('elts elts)
                 ('ctx ctx)) ;; ignoring ctx for now
     (PyList (map get-structured-python elts))]
    [(hash-table ('nodetype "Tuple") 
                 ('ctx ctx)
                 ('elts elts))
     (PyTuple (map get-structured-python elts))]
;    [(hash-table ('nodetype "Load"))]
;    [(hash-table ('nodetype "Store"))]
;    [(hash-table ('nodetype "Del"))]
;    [(hash-table ('nodetype "AugLoad"))]
;    [(hash-table ('nodetype "AugStore"))]
;    [(hash-table ('nodetype "Param"))]
;    [(hash-table ('nodetype "Ellipsis"))]
    [(hash-table ('nodetype "Slice")
                 ('lower lower)
                 ('upper upper)
                 ('step step))
     (list (if (null? lower) 
               PyEmp
               (get-structured-python lower)) 
           (if (null? upper)
               PyEmp
               (get-structured-python upper)) 
           (if (null? step)
               PyEmp
               (get-structured-python step)))]
;    [(hash-table ('nodetype "ExtSlice"))]
    [(hash-table ('nodetype "Index")
                 ('value value))
     (list (get-structured-python value)))]
    [(hash-table ('nodetype "And")) 'and]
    [(hash-table ('nodetype "Or")) 'or]
    [(hash-table ('nodetype "Add")) '+]
    [(hash-table ('nodetype "Sub")) '-]
    [(hash-table ('nodetype "Mult")) '*]
    [(hash-table ('nodetype "Div")) '/]
    [(hash-table ('nodetype "Mod")) '%]
    [(hash-table ('nodetype "Pow")) '**]
    [(hash-table ('nodetype "Lshift")) '<<]
    [(hash-table ('nodetype "Rshift")) '>>]
    [(hash-table ('nodetype "BitOr")) 'bor]
    [(hash-table ('nodetype "BitXor")) '^]
    [(hash-table ('nodetype "BitAnd")) 'band]
    [(hash-table ('nodetype "FloorDiv")) '//]
    [(hash-table ('nodetype "Invert")) '~]
    [(hash-table ('nodetype "Not")) 'not]
    [(hash-table ('nodetype "UAdd")) 'pos]
    [(hash-table ('nodetype "USub")) 'neg]
    [(hash-table ('nodetype "Eq")) '==]
    [(hash-table ('nodetype "NotEq")) '!=]
    [(hash-table ('nodetype "Lt")) '<]
    [(hash-table ('nodetype "LtE")) '<=]
    [(hash-table ('nodetype "Gt")) '>]
    [(hash-table ('nodetype "GtE")) '>=]
    [(hash-table ('nodetype "Is")) 'is]
    [(hash-table ('nodetype "IsNot")) '!is]
    [(hash-table ('nodetype "In")) 'in]
    [(hash-table ('nodetype "NotIn")) '!in]
    [(hash-table ('nodetype "ExceptHandler")
                 ('name name) ;; ignoring keywords for name 
                 ('body body)
                 ('type type)) ;; ignoring keywords for type
     (PySeq (map get-structured-python body))]
    [(hash-table ('nodetype "arguments")
                 ('defaults default-list) ;; ignoring keywords for default-list 
                 ('kwargannotation kwar) ;; ignoring keywords for kwar
                 ('vararg var) ;; ignoring keywords for var
                 ('args args-list))
     (map get-structured-python args-list)]
    [(hash-table ('nodetype "arg")
                 ('arg arg)
                 ('annotation annotation))
     (string->symbol arg)]
    [_ (error 'parse "Haven't handled a case yet")]))

