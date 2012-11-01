* DESIGN CHOICES *

I've chosen to stick with many of the core ParselTongue features excluding Objects and its GetField/SetField types.

I have added the following core types:

- CByte: maps to Racket strings (this will mean the interpreter will need to ensure the string passed to CByte represent the ASCII value of single bytes)
- CByteArray: same as CByte except it is mutable
- CList: maps to Racket vectors since Python lists are mutable
- CTuple: maps to Racket lists since Python tuples are immutable
- CRange: maps to Racket lists (the desugarer will need to generate the appropriate numbers based on the arguments passed to the method)
- CDict: maps to Racket hash tables
- CSet: maps to Racket vectors since Python sets are mutable
- CFrozSet: maps to Racket lists since Python frozen sets are immutable
- CClass: this will desugar to a function and therefore it's similar to a closure

So, the values that can be returned are as follows:

- VNum
- VStr
- VByte
- VByteArray
- VTrue
- VFalse
- VNone
- VException
- VList
- VTuple
- VRange
- VDict
- VSet
- VFrozSet
- VClass
- VClosure

* CODE STRUCTURE *

The code structure hasn't altered from what was originally given to us on GitHub. The desugarer and interpreter follow the same style as what we did for ParselTongue.

* TESTS PASSED *

Types:

test_floats.py
test_comparisons.py
test_booleans.py

Scope:

lambda1.py
lambda2.py
lambda3.py
lambda4.py
nearest-enclosing-scope.py
nesting-global-no-free.py
extra-nesting.py
simple-nesting.py
