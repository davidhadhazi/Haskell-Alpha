module Integrate (integrate) where

import SyntaxTree
import Tokens
import Derivate
import Simplification

integrate :: Expression -> Expression
integrate (SIMPLE n) = BINIX (SIMPLE n, MUL, VAR 'x')
integrate (VAR x) = BINIX (BINIX (SIMPLE 1, DIV, SIMPLE 2), MUL, BINIX (VAR x, RAI, SIMPLE 2))
integrate (UNIX (NEG, e)) = UNIX (NEG, integrate e)
integrate (UNIX (SIN, VAR 'x')) = UNIX (NEG, UNIX (COS, VAR 'x'))
integrate (UNIX (COS, VAR 'x')) = UNIX (SIN, VAR 'x')
integrate (UNIX (TAN, VAR 'x')) = UNIX (NEG, UNIX (LN, UNIX (COS, VAR 'x')))
integrate (UNIX (CTG, VAR 'x')) = UNIX (LN, UNIX (COS, VAR 'x'))
integrate (BINIX (VAR 'x', RAI, SIMPLE n)) = BINIX (SIMPLE (1 /(n + 1)), MUL, BINIX (VAR 'x', RAI, SIMPLE (n + 1)))
integrate (BINIX (e1, ADD, e2)) = BINIX (integrate e1, ADD, integrate e2)
integrate (BINIX (e1, MIN, e2)) = BINIX (integrate e1, MIN, integrate e2)
integrate (BINIX (SIMPLE n, RAI, VAR 'x')) = BINIX (BINIX (SIMPLE n, RAI, VAR 'x'), DIV, UNIX (LN, SIMPLE n))
-------------------------------
integrate (BINIX (e1, MUL, UNIX (SIN, e2)))
 | simplifying (derivate e2) == e1 = UNIX (NEG, UNIX (COS, e2)) 
-------- Partial integration
integrate (BINIX (e1, MUL, e2)) = e where
 f = e1
 g' = e2
 g = integrate g'
 f' = simplifying $ derivate f
 e
  | (e1 == f' && e2 == g) || (e2 == f' && e1 == g) = BINIX (simplifying (BINIX (f, MUL, g)), DIV, SIMPLE 2)
  |otherwise = BINIX (simplifying (BINIX (f, MUL, g)), MIN, (integrate (simplifying (BINIX (f', MUL, g)))))