#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(define (desugar expr)
  (type-case PyExpr expr
    [PySeq (es) (foldr (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))]
    [PyNum (n) (CNum n)]
    [PyStr (s) (CStr s)]
    [PyVoid () (CVoid)]
    [PyApp (f args) (CApp (desugar f) (map desugar args))]
    [PyId (x) (CId x)]
    [PyPrimP (l o r) (CPrimP (desugar l) o (desugar r))]
    [PyBoolOp (o e)
              (case o
                [(or) (CLet 'or-exprs (map desugar e)
                            (CLet 'or-func
                                  (CFunc 
                                   (CLet 'var-or1 (first (CId )
                                         (CIf (CId 'var-or1)
                                              (CId 'var-or1)
                                              (CIf (empty? (rest e))
                                                   (CError (CStr "invalid syntax"))
                                                   (CApp (CId 'or-func) (rest e))))))
                                  (CApp (CId 'or-func) (e))])]
                                 
    [PyCond (c t e) (CIf (desugar c) (desugar t) (desugar e))]
    [PyRaise (e) (CError (desugar e))]
    [else (error 'desugar (string-append "no case yet" (to-string expr)))]))
