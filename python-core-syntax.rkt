#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(define-type CExp
  [CNum (n : number)]
  [CStr (s : string)]
  [CByte (b : string)]
  [CByteArray (b : string)]
  [CTrue]
  [CFalse]
  [CNone]
  [CSeq (e1 : CExp) (e2 : CExp)]
  [CSet! (x : symbol) (val : CExp)]
  [CError (err : (listof CExp))]
  [CList (lst : (listof CExp))] ; change to vectors
  [CTuple (lst : (listof CExp))]
  [CRange (seq : (listof CExp))]
  [CDict (dict : (hashof symbol CExp))]
  [CSet (l : (vectorof CExp))]
  [CFrozSet (l : (listof CExp))]
  [CIf (test : CExp) (then : CExp) (else : CExp)]
  [CId (x : symbol)]
  [CLet (x : symbol) (bind : CExp) (body : CExp)]
  [CApp (fun : CExp) (args : (listof CExp))]
  [CFunc (args : (listof symbol)) (body : CExp)]
  [CPrim1 (prim : symbol) (args : (listof CExp))]
  [CPrimP (l : CExp) (op : symbol) (r : CExp)]
  [CClass (f : CExp) (bases : (listof symbol))])

(define-type CVal
  [VNum (n : number)]
  [VStr (s : string)]
  [VByte (b : string)]
  [VByteArray (b : string)]
  [VTrue]
  [VFalse]
  [VNone]
  [VException (exn : (listof CVal))]
  [VList (lst : (listof CVal))]
  [VTuple (lst : (listof CVal))]
  [VRange (lst : (listof CVal))]
  [VDict (dict : (hashof symbol CVal))]
  [VSet (l : (vectorof CVal))]
  [VFrozSet (l : (listof CVal))]
  [VClass (env : Env) (bases : (listof symbol)) (body : CExp)]
  [VClosure (env : Env) (args : (listof symbol)) (body : CExp)])

(define-type CAnswer
  [ValueA (v : CVal) (s : Store)])

(define-type-alias Loc number)
(define-type-alias Env (hashof symbol Loc))

(define-type-alias Store (hashof Loc CVal))

