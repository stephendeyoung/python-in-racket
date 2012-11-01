#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "python-lib.rkt")

(require (typed-in racket/base (string<? : (string string -> boolean)))
         (typed-in racket/base (string<=? : (string string -> boolean)))
         (typed-in racket/base (string>=? : (string string -> boolean)))
         (typed-in racket/base (string>? : (string string -> boolean))))

(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

(define (interp-env expr env store)
  (type-case CExp expr
    [CNum (n) (ValueA (VNum n) store)]
    [CStr (s) (ValueA (VStr s) store)]
    [CTrue () (ValueA (VTrue) store)]
    [CFalse () (ValueA (VFalse) store)]

    [CError (e) (ValueA (VException (map (lambda (e-v)
                                           (type-case CAnswer (interp-env e-v env store)
                                             [ValueA (e-v s-v) e-v])) e)) store)]
    
    [CNone () (ValueA (VNone) store)]

    [CIf (i t e) (type-case CAnswer (interp-env i env store)
      [ValueA (i-v i-s)
              (if (VTrue? i-v)
                  (type-case CAnswer (interp-env t env i-s)
                    [ValueA (t-v t-s)
                            (ValueA t-v t-s)])
                  (type-case CAnswer (interp-env e env i-s)
                    [ValueA (e-v e-s)
                            (ValueA e-v e-s)]))])]

    [CId (x) (type-case (optionof Loc) (hash-ref env x)
      [some (v) (type-case (optionof CVal) (hash-ref store v)
                  [some (v) (ValueA v store)]
                  [none () (error 'interp (string-append "No value in store for this location: " (to-string v)))])]
      [none () (type-case (optionof CExp) (hash-ref python-lib x)
                 [some (v) (type-case CAnswer (interp-env v env store)
                             [ValueA (v-v s-v)
                                     (ValueA v-v s-v)])]
                 [none () (ValueA (VException (list (VStr (string-append "Unbound identifier: " (to-string x))))) store)])])]

    [CLet (x bind body)
          (type-case CAnswer (interp-env bind env store)
            [ValueA (b-v b-s)
                    (let ([loc (new-loc)])
                      (type-case CAnswer (interp-env body (hash-set env x loc) (hash-set store loc b-v))
                        [ValueA (bod-v bod-s)
                                (ValueA bod-v bod-s)]))])]

    [CSeq (e1 e2)
      (type-case CAnswer (interp-env e1 env store)
        [ValueA (e1-v e1-s)
                (type-case CAnswer (interp-env e2 env e1-s)
                  [ValueA (e2-v e2-s)
                          (ValueA e2-v e2-s)])])]
    
    [CSet! (x v)
           (type-case (optionof Loc) (hash-ref env x)
             [some (val) (type-case CAnswer (interp-env v env store)
                           [ValueA (v-v s-v)
                                   (ValueA v-v (hash-set s-v val v-v))])]
             [none () (let ([loc (new-loc)])
                        (type-case CAnswer (interp-env v (hash-set env x loc) store)
                           [ValueA (v-v s-v)
                                   (ValueA v-v (hash-set s-v loc v-v))]))])]
    
    [CList (l)
           (ValueA (VList (map (lambda (e) 
                                 (type-case CAnswer (interp-env e env store)
                                   [ValueA (e-v e-s)
                                           e-v])) l)) store)]

    [CApp (fun arges)
     (type-case CAnswer (interp-env fun env store)
       [ValueA (fun-v fun-s)
               (if (VClosure? fun-v)
                   (local [(define argvs (map (lambda (e) 
                                                (type-case CAnswer (interp-env e env store)
                                                  [ValueA (e-v e-s)
                                                          e-v])) arges))]
                     (bind-args (VClosure-args fun-v) argvs (VClosure-env fun-v) fun-s (VClosure-body fun-v)))
                   (error 'interp (string-append "Not a closure: " (to-string fun-v))))])]

    [CFunc (args body) (ValueA (VClosure env args body) store)] 

    [CPrim1 (prim args) (ValueA (python-prim1 prim (map (lambda (e) 
                                                  (type-case CAnswer (interp-env e env store)
                                                    [ValueA (e-v e-s)
                                                            e-v])) args)) store)]
    
    [CPrimP (l o r)
            (type-case CAnswer (interp-env l env store)
              [ValueA (l-v l-s)
                      (type-case CVal l-v
                        [VNum (l-n) (type-case CAnswer (interp-env r env l-s)
                                      [ValueA (r-v r-s)
                                              (type-case CVal r-v
                                                [VNum (r-n) (case o
                                                              [(+) (ValueA (VNum (+ l-n r-n)) r-s)]
                                                              [(-) (ValueA (VNum (- l-n r-n)) r-s)]
                                                              [(*) (ValueA (VNum (* l-n r-n)) r-s)]
                                                              [(==) (if (= l-n r-n)
                                                                        (ValueA (VTrue) r-s)
                                                                        (ValueA (VFalse) r-s))]
                                                              [(is) (if (= l-n r-n)
                                                                        (ValueA (VTrue) r-s)
                                                                        (ValueA (VFalse) r-s))]
                                                              [(<) (if (< l-n r-n)
                                                                       (ValueA (VTrue) r-s)
                                                                       (ValueA (VFalse) r-s))]
                                                              [(<=) (if (<= l-n r-n)
                                                                        (ValueA (VTrue) r-s)
                                                                        (ValueA (VFalse) r-s))]
                                                              [(>) (if (> l-n r-n)
                                                                        (ValueA (VTrue) r-s)
                                                                        (ValueA (VFalse) r-s))]
                                                              [(>=) (if (>= l-n r-n)
                                                                        (ValueA (VTrue) r-s)
                                                                        (ValueA (VFalse) r-s))]
                                                              [(!=) (if (= l-n r-n)
                                                                        (ValueA (VFalse) r-s)
                                                                        (ValueA (VTrue) r-s))])]                                                
                                                [else (case o
                                                        [(==) (ValueA (VFalse) r-s)]
                                                        [(is) (ValueA (VFalse) r-s)]
                                                        [(!=) (ValueA (VTrue) r-s)]
                                                        [else (error 'interp "Unsupported operand")])])])]
                        [VStr (l-str) (type-case CAnswer (interp-env r env l-s)
                                        [ValueA (r-v r-s)
                                                (type-case CVal r-v
                                                  [VStr (r-str) (case o
                                                                  [(+) (ValueA (VStr (string-append l-str r-str)) r-s)]
                                                                  [(==) (if (string=? l-str r-str)
                                                                            (ValueA (VTrue) r-s)
                                                                            (ValueA (VFalse) r-s))]
                                                                  [(is) (if (string=? l-str r-str)
                                                                            (ValueA (VTrue) r-s)
                                                                            (ValueA (VFalse) r-s))]
                                                                  [(<) (if (string<? l-str r-str)
                                                                       (ValueA (VTrue) r-s)
                                                                       (ValueA (VFalse) r-s))]
                                                                  [(<=) (if (string<=? l-str r-str)
                                                                            (ValueA (VTrue) r-s)
                                                                            (ValueA (VFalse) r-s))]
                                                                  [(>) (if (string>? l-str r-str)
                                                                           (ValueA (VTrue) r-s)
                                                                           (ValueA (VFalse) r-s))]
                                                                  [(>=) (if (string>=? l-str r-str)
                                                                            (ValueA (VTrue) r-s)
                                                                            (ValueA (VFalse) r-s))]
                                                                  [(!=) (if (string=? l-str r-str)
                                                                            (ValueA (VFalse) r-s)
                                                                            (ValueA (VTrue) r-s))])]    
                                                  [else (case o
                                                          [(==) (ValueA (VFalse) r-s)]
                                                          [(is) (ValueA (VFalse) r-s)]
                                                          [else (error 'interp "TypeError: unsupported operand type(s)")])])])]
                        [VTrue () (type-case CAnswer (interp-env r env l-s)
                                      [ValueA (r-v r-s)
                                              (type-case CVal r-v
                                                [VTrue () (case o
                                                               [(==) (ValueA (VTrue) r-s)]
                                                               [(is) (ValueA (VTrue) r-s)]
                                                               [else (error 'interp "Unsupported operand")])]
                                                [else (case o
                                                          [(==) (ValueA (VFalse) r-s)]
                                                          [(is) (ValueA (VFalse) r-s)]
                                                          [else (error 'interp "TypeError: unsupported operand type(s)")])])])]
                        [VFalse () (type-case CAnswer (interp-env r env l-s)
                                      [ValueA (r-v r-s)
                                              (type-case CVal r-v
                                                [VFalse () (case o
                                                               [(==) (ValueA (VTrue) r-s)]
                                                               [(is) (ValueA (VTrue) r-s)]
                                                               [else (error 'interp "Unsupported operand")])]
                                                [else (case o
                                                          [(==) (ValueA (VFalse) r-s)]
                                                          [(is) (ValueA (VFalse) r-s)]
                                                          [else (error 'interp "TypeError: unsupported operand type(s)")])])])]
                        [VNone () (type-case CAnswer (interp-env r env l-s)
                                      [ValueA (r-v r-s)
                                              (type-case CVal r-v
                                                [VNone () (case o
                                                               [(==) (ValueA (VTrue) r-s)]
                                                               [(is) (ValueA (VTrue) r-s)]
                                                               [else (error 'interp "Unsupported operand")])]
                                                [else (case o
                                                          [(==) (ValueA (VFalse) r-s)]
                                                          [(is) (ValueA (VFalse) r-s)]
                                                          [else (error 'interp "TypeError: unsupported operand type(s)")])])])]
                        ;[VException (e) (type-case CAnswer (interp-env r env l-s)
                         ;                 [ValueA (r-v r-s)
                          ;                        (type-case CVal r-v
                           ;                         [VException (r-e)
                            ;                                    (if (not (equal? e r-e))
                        [else (error 'interp "TypeError: unsupported operand type(s)")])])]
              
    [else (error 'interp (string-append "no case yet: " (to-string expr)))]))

(define (bind-args args vals env store expr)
  (cond [(and (empty? args) (empty? vals)) (type-case CAnswer (interp-env expr env store)
                                             [ValueA (expr-v expr-s)
                                                     (ValueA expr-v expr-s)])]
        [(or (empty? args) (empty? vals))
         (error 'interp "Arity mismatch")]
        [(and (cons? args) (cons? vals))
         (let ([loc (new-loc)])
           (bind-args (rest args) 
                      (rest vals) 
                      (hash-set env (first args) loc) 
                      (hash-set store loc (first vals))
                      expr))]))

(define (interp expr)
  (interp-env expr (hash (list)) (hash (list))))


