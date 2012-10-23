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
                 (PyVoid)
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
    [(hash-table ('nodetype "BoolOp")
                 ('op ops-list)
                 ('values values-list))
     (PyBoolOp (get-structured-python ops-list)
               (map get-structured-python values-list))]
    [(hash-table ('nodetype "Add"))
     '+]
    [(hash-table ('nodetype "Eq"))
     '==]
    [(hash-table ('nodetype "Or"))
     'or]
    [(hash-table ('nodetype "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list))]
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
                 
    [_ (error 'parse (printf pyjson))]))

