#lang plai-typed

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  [PyNum (n : number)]
  [PyStr (s : string)]
  [PyId (x : symbol)]
  ;[PyCond (test : PyExpr) (then : PyExpr) (else : PyExpr)]
  [PyPrimP (left : (listof PyExpr)) (op : symbol) (right : (listof PyExpr))]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))])

