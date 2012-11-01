#lang plai-typed

(require "python-core-syntax.rkt")

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#

(require (typed-in racket/base [display : (string -> void)]))

(define (pretty arg)
  (type-case CAnswer arg
    [ValueA (arg-v arg-s) 
            (type-case CVal arg-v
              [VNum (n) (to-string n)]
              [VStr (s) s]
              [VTrue () "true"]
              [VFalse () "false"]
              [VNone () ""]
              [VList (l) (to-string l)]
              [VTuple (l) (to-string l)]
              [VRange (l) (to-string l)]
              [VDict (d) (to-string d)]
              [VException (e) (error 'type (to-string (map (lambda (e)
                                                             (pretty (ValueA e (hash (list)))))
                                                           e)))]
              [VClosure (env args body) (error 'prim "Can't print closures yet")]
              [else "no pretty print for this type yet"])]))
  

(define (print arg)
  (display (pretty arg)))

(define (python-prim1 op args)
  (case op
    [(print) (begin (print (ValueA (first args) (hash (list)))) (first args))]
    [(type) (cond
              [(VNum? (first args)) (VStr "int")]
              [(VStr? (first args)) (VStr "string")]
              [(VTrue? (first args)) (VStr "true")]
              [(VList? (first args)) (VStr "list")]
              [(VClosure? (first args)) (VStr "function")]
              [(VException? (first args)) (VStr "type")])]
    [(len) (VNum (length (VList-lst (first args))))]
    [(pop) (list-ref (VList-lst (first args)) (VNum-n (second args)))]
    [(True) (cond
              [(and (VNum? (first args)) (= (VNum-n (first args)) 0)) (VFalse)]
              [(and (VStr? (first args)) (string=? (VStr-s (first args)) "")) (VFalse)]
              [(and (VList? (first args)) (empty? (VList-lst (first args)))) (VFalse)]
              [(VNone? (first args)) (VFalse)]
              [(VFalse? (first args)) (VFalse)]
              [else (VTrue)])]))

