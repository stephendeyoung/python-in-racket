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
    [(hash-table ('nodetype "Module") ('body expr-list))
     (PySeq (map get-structured-python expr-list))]
    [(hash-table ('nodetype "Expr") ('value expr))
     (get-structured-python expr)]
    [(hash-table ('nodetype "If")
                 ('test test-expr)
                 ('body then)
                 ('orelse els))
     (PyCond (get-structured-python test-expr)
             (PySeq (map get-structured-python then))
             (if (empty? els)
                 (PyNone)
                 (PySeq (map get-structured-python els))))]
    [(hash-table ('nodetype "Compare")
                 ('ops ops-list)
                 ('comparators comparators-list)
                 ('left left-expr))
     (PyPrimP (get-structured-python left-expr)
              (first (map get-structured-python ops-list))
              (PySeq (map get-structured-python comparators-list)))]
    [(hash-table ('nodetype "BinOp")
                 ('op operation)
                 ('left left-expr)
                 ('right right-expr))
     (PyPrimP (get-structured-python left-expr)
              (get-structured-python operation)
              (get-structured-python right-expr))]
    [(hash-table ('nodetype "UnaryOp")
                 ('op operation)
                 ('operand val))
     (PyUnaryOp (get-structured-python operation)
               (get-structured-python val))]
    [(hash-table ('nodetype "BoolOp")
                 ('op ops-list)
                 ('values values-list))
     (PyBoolOp (get-structured-python ops-list)
               (map get-structured-python values-list))]
    [(hash-table ('nodetype "Add"))
     '+]
    [(hash-table ('nodetype "Mult"))
     '*]
    [(hash-table ('nodetype "Eq"))
     '==]
    [(hash-table ('nodetype "Lt"))
     '<]
    [(hash-table ('nodetype "LtE"))
     '<=]
    [(hash-table ('nodetype "Gt"))
     '>]
    [(hash-table ('nodetype "GtE"))
     '>=]
    [(hash-table ('nodetype "NotEq"))
     '!=]
    [(hash-table ('nodetype "Or"))
     'or]
    [(hash-table ('nodetype "And"))
     'and]
    [(hash-table ('nodetype "Not"))
     'not]
    [(hash-table ('nodetype "Is"))
     'is]
    [(hash-table ('nodetype "USub"))
     'neg]
    [(hash-table ('nodetype "In"))
     'in]
    [(hash-table ('nodetype "NotIn"))
     'not-in]
    [(hash-table ('nodetype "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list))]
    ;[(hash-table ('nodetype "Attribute")
     ;            ('ctx _)
      ;           ('attr attr)
       ;          ('value val))
     ;(PyApp (string->symbol attr)
      ;      (map get-structured-python val))]
    [(hash-table ('nodetype "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id id))
     (PyId (string->symbol id))]
    [(hash-table ('nodetype "Num")
                 ('n n))
     (PyNum n)]
    [(hash-table ('nodetype "Str")
                 ('s s))
     (PyStr s)]
    [(hash-table ('nodetype "Raise")
                 ('cause _)
                 ('exc exception))
     (PyRaise (get-structured-python exception))]
    [(hash-table ('nodetype "List")
                 ('ctx _)
                 ('elts elements))
     (PyList (map get-structured-python elements))]
    [(hash-table ('nodetype "Pass"))
     (PyNone)]
    [(hash-table ('nodetype "Assign")
                 ('value val)
                 ('targets id))
    (PySet! (first (map get-structured-python id))
            (get-structured-python val))]
    [(hash-table ('nodetype "Lambda")
                 ('args args)
                 ('body body))
     (PyFunc (get-structured-python args)
             (get-structured-python body))]
    [(hash-table ('nodetype "FunctionDef")
                 ('name name)
                 ('args args)
                 ('body body)
                 ('decorator_list _)
                 ('returns _))
     (PySet! (PyId (string->symbol name))
             (PyFunc (get-structured-python args)
                     (PySeq (map get-structured-python body))))]
    [(hash-table ('nodetype "arguments")
                 ('args args)
                 ('defaults _)
                 ('kwargannotation _)
                 ('vararg _)
                 ('kwarg _)
                 ('varargannotation _)
                 ('kw_defaults _)
                 ('kwonlyargs _))
     (map get-structured-python args)]
    [(hash-table ('nodetype "arg")
                 ('arg arg)
                 ('annotation _))
     (string->symbol arg)]
    [(hash-table ('nodetype "Return")
                 ('value val))
     (get-structured-python val)]
                 
    [a (error 'parse "Haven't handled a case yet: '~a'" a)]))

