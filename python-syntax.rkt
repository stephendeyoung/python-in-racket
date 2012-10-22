#lang plai-typed

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  [PyNum (n : number)]
  [PyStr (s : string)]
  [PyBool (v : boolean)]
  [PyId (x : symbol)]
  ;[PyCond (test : PyExpr) (then : PyExpr) (else : PyExpr)]
  [PyPrimP (left : PyExpr) (op : symbol) (right : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))])

