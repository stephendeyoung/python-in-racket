#lang plai-typed

(require "python-core-syntax.rkt")

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

(define-type-alias Lib (CExp -> CExp))

(define print-lambda
  (CFunc (list 'to-print)
    (CPrim1 'print (list (CId 'to-print)))))

(define assert-true-lambda
  (CFunc (list 'check-true)
    (CIf (CId 'check-true) (CTrue) (CPrim1 'print (list (CError (list (CStr "Assert failed"))))))))

(define assert-equal-lambda
  (CFunc (list 'check-equal1 'check-equal2)
         (CIf (CPrimP (CId 'check-equal1) '== (CId 'check-equal2))
              (CTrue)
              (CPrim1 'print (list (CError (list (CStr "Assert failed"))))))))

;(define assert-raises-lambda
 ; (CFunc (list 'check-raises 'error-func)
  ;       (CPrimP (

(define true-val
  (CFunc (list 'check-truthy)
         (CPrim1 'True (list (CId 'check-truthy)))))

(define type-lambda
  (CFunc (list 'check-type)
         (CPrim1 'type (list (CId 'check-type)))))

(define get-length
  (CFunc (list 'check-length)
         (CIf (CPrimP (CPrim1 'type (list (CId 'check-length))) '== (CStr "list"))
              (CPrim1 'len (list (CId 'check-length)))
              (CPrim1 'print
                      (list (CError (list (CPrimP (CStr "object of type ") '+ (CPrimP (CPrim1 'type (list (CId 'check-length))) '+ (CStr " has no len()"))))))))))

(define pop-lambda
  (CFunc (list 'lst 'index)
         (CPrim1 'pop (list (CId 'lst) (CId 'index)))))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define lib-functions
  (list (bind 'print print-lambda)
        (bind 'True true-val)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertEqual assert-equal-lambda)
        ;(bind '___assertRaises assert-raises-lambda)
        (bind 'type type-lambda)
        (bind 'len get-length)
        (bind 'pop pop-lambda)

))

(define python-lib
  (local [(define (python-lib/recur libs bindings)
            (cond [(empty? libs) bindings]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (python-lib/recur (rest libs) (hash-set bindings name value))))]))]
    (python-lib/recur lib-functions (hash empty))))


