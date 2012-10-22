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
    [(hash-table ('type "Module") ('body expr-list))
     (PySeq (map get-structured-python expr-list))]
    [(hash-table ('type "Expr") ('value expr))
     (get-structured-python expr)]
    ;[(hash-table ('type "If")
     ;            ('test test-expr)
      ;           ('body then)
       ;          ('orelse else))
     ;(PyCond (get-structured-python test-expr)
      ;       (get-structured-python then))]
             ;(get-structured-python else)
    ;[(hash-table ('type "Compare")
     ;            ('ops ops-list)
      ;           ('comparators comparators-list)
       ;          ('left left-expr)
        ;         ('right right-expr))
     ;(PyPrimP (get-structured-python left-expr)
    [(hash-table ('type "BinOp")
                 ('op operation)
                 ('left left-expr)
                 ('right right-expr))
     (PyPrimP (get-structured-python left-expr)
              (get-structured-python operation)
              (get-structured-python right-expr))]   
    [(hash-table ('type "Add"))
     '+]
                  
    [(hash-table ('type "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list))]
    [(hash-table ('type "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id id))
     (PyId (string->symbol id))]
    [(hash-table ('type "Num")
                 ('n n))
     (PyNum n)]
    [(hash-table ('type "Str")
                 ('s s))
     (PyStr s)]
    ;[(hash-table ('type "If")
                 
    [_ (error 'parse "Haven't handled a case yet")]))

