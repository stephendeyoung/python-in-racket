#lang plai-typed

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  [PyNum (n : number)]
  [PyStr (s : string)]
  [PyBool (v : boolean)]
  [PyVoid]
  [PyId (x : symbol)]
  [PyCond (cond : PyExpr) (then : PyExpr) (else : PyExpr)]
  [PyPrimP (left : PyExpr) (op : symbol) (right : PyExpr)]
  [PyBoolOp (op : symbol) (vals : (listof PyExpr))]
  [PyRaise (e : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))])

