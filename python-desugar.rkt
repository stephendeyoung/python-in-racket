#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(define (desugar expr)
  (type-case PyExpr expr
    [PySeq (es) (get-vars es (list) (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es)))]
    [PyNum (n) (CNum n)]
    [PyStr (s) (CStr s)]
    [PyNone () (CNone)]
    [PyApp (f args) (CApp (desugar f) (map desugar args))]
    [PyId (x) (CId x)]
    [PyPrimP (l o r) (CPrimP (desugar l) o (desugar r))]
    [PyBoolOp (o e)
              (case o ; TODO: Use cleaner methods such as first/rest
                [(or) (local ([define dummy-fun (CFunc (list) (CPrim1 'print (list (CError (list (CStr "Dummy function"))))))])
                        (CLet 'or-func dummy-fun
                              (CLet 'or-func2
                                    (CFunc (list 'el 'ln 'num)
                                           (CIf (CId 'el)
                                                (CId 'el)
                                                (CIf (CPrimP (CId 'ln) '== (CId 'num))
                                                     (CId 'el)
                                                     (CLet 'next-or (CPrimP (CId 'num) '+ (CNum 1))
                                                           (CApp (CId 'or-func) (list (CPrim1 'pop (list (CList (map desugar e)) (CId 'next-or))) (CId 'ln) (CId 'next-or)))))))
                                    (CSeq (CSet! 'or-func (CId 'or-func2))
                                          (CApp (CId 'or-func) (list (desugar (first e))
                                                                     (CPrimP (CPrim1 'len (list (CList (map desugar e)))) '- (CNum 1))
                                                                     (CNum 0)))))))]
                [(and) (local ([define dummy-fun (CFunc (list) (CPrim1 'print (list (CError (list (CStr "Dummy function"))))))])
                         (CLet 'and-var dummy-fun
                               (CLet 'and-func
                                     (CFunc (list 'el 'ln 'num)
                                            (CIf (CId 'el)
                                                 (CIf (CPrimP (CId 'ln) '== (CId 'num))
                                                      (CId 'el)
                                                      (CLet 'next-and (CPrimP (CId 'num) '+ (CNum 1))
                                                           (CApp (CId 'and-var) (list (CPrim1 'pop (list (CList (map desugar e)) (CId 'next-and))) (CId 'ln) (CId 'next-and)))))
                                                 (CId 'el)))
                                     (CSeq (CSet! 'and-var (CId 'and-func))
                                           (CApp (CId 'and-var) (list (desugar (first e))
                                                                     (CPrimP (CPrim1 'len (list (CList (map desugar e)))) '- (CNum 1))
                                                                     (CNum 0)))))))])]
    
    [PyUnaryOp (o v)
               (case o
                 [(not) (CIf (CPrimP (desugar v) '== (CFalse))
                             (CTrue)
                             (CFalse))]
                 [(neg) (CPrimP (desugar v) '* (CNum -1))])]                                              
                                 
    [PyCond (c t e) (CIf (CPrim1 'True (list (desugar c))) (desugar t) (desugar e))]
    [PyRaise (e) (CError (list (desugar e)))]
    [PyList (l) (CList (map desugar l))]
    [PySet! (i v) (CSet! (PyId-x i) (desugar v))]
    [PyFunc (a b) (CFunc a (desugar b))]
    [else (error 'desugar (string-append "no case yet " (to-string expr)))]))

(define (get-vars (exprs : (listof PyExpr)) (sym : (listof symbol)) seq) : CExp
  (if (empty? exprs)
      seq 
      (type-case PyExpr (first exprs)
        [PySet! (i v) (CIf (lookup (PyId-x i) sym)
                           (CLet (PyId-x i) (CError (list (CPrimP (CStr "Unbound identifier: ")
                                                                  '+
                                                                  (CStr (to-string (PyId-x i))))))
                                 (get-vars (rest exprs) (cons (PyId-x i) sym) seq))
                           (get-vars (rest exprs) sym seq))]
        [else (if (empty? (rest exprs))
                  seq
                  (get-vars (rest exprs) sym seq))])))

(define (lookup (sym : symbol) (syms : (listof symbol))) : CExp
  (if (empty? syms)
      (CTrue)
      (if (symbol=? sym (first syms))
          (CFalse)
          (lookup sym (rest syms)))))


