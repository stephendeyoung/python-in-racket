#lang plai-typed

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  [PyNum (n : number)]
  [PyStr (s : string)]
  [PyBool (v : boolean)]
  [PyNone]
  [PyId (x : symbol)]
  [PyCond (cond : PyExpr) (then : PyExpr) (else : PyExpr)]
  [PyPrimP (left : PyExpr) (op : symbol) (right : PyExpr)]
  [PyBoolOp (op : symbol) (vals : (listof PyExpr))]
  [PyUnaryOp (op : symbol) (val : PyExpr)]
  [PyRaise (e : PyExpr)]
  [PyList (lst : (listof PyExpr))]
  ;[PyLet (x : symbol) (bind : PyExpr) (body : CExp)]
  [PySet! (id : PyExpr) (val : PyExpr)]
  [PyFunc (args : (listof symbol)) (body : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))])

