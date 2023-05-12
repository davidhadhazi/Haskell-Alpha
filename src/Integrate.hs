module Integrate (integrate) where

import SyntaxTree
import Tokens
import Derivate
import Simplification
import Number

isSimple :: Expression -> Bool
isSimple (SIMPLE _) = True
isSimple _ = False

difference :: Expression -> Expression -> Number'
difference (BINIX (a1, ADD, b1)) (BINIX (a2, ADD, b2))
 | (isMultipleOf a1 a2) && (isMultipleOf b1 b2) && (difference a1 a2) == (difference b1 b2) = difference a1 a2
difference (BINIX (SIMPLE n, MUL, e1)) (BINIX (SIMPLE m, MUL, e2))
 | e1 == e2 = n / m
difference (BINIX (SIMPLE n, MUL, e1)) e2
 | e1 == e2 = n
difference e1 (BINIX (SIMPLE m, MUL, e2))
 | e1 == e2 = 1 / m
difference (UNIX (NEG, e1)) (UNIX (NEG, e2))
 | isMultipleOf e1 e2 = difference e1 e2
difference (UNIX (NEG, e1)) e2
 | isMultipleOf e1 e2 = -1 * difference e1 e2
difference e1 (UNIX (NEG, e2))
 | isMultipleOf e1 e2 = -1 * difference e1 e2
difference (SIMPLE n) (SIMPLE m) = n / m
difference e1 e2
 | e1 == e2 = Integer 1

isMultipleOf :: Expression -> Expression -> Bool
isMultipleOf (SIMPLE _) (SIMPLE _) = True
isMultipleOf (UNIX (NEG, e1)) (UNIX (NEG, e2)) = isMultipleOf e1 e2
isMultipleOf (UNIX (NEG, e1)) e2 = isMultipleOf e1 e2
isMultipleOf e1 (UNIX (NEG, e2)) = isMultipleOf e1 e2
isMultipleOf (BINIX (SIMPLE _, MUL, e1)) (BINIX (SIMPLE _, MUL, e2)) = isMultipleOf e1 e2
isMultipleOf (BINIX (SIMPLE _, MUL, e1)) e2 = isMultipleOf e1 e2
isMultipleOf e1 (BINIX (SIMPLE _, MUL, e2)) = isMultipleOf e1 e2
isMultipleOf (BINIX (a1, ADD, b1)) (BINIX (a2, ADD, b2)) = (isMultipleOf a1 a2) && (isMultipleOf b1 b2) && (difference a1 a2) == (difference b1 b2)
isMultipleOf e1 e2
 | e1 == e2 = True
 |otherwise = False

integrate :: Expression -> Expression
integrate (SIMPLE n) = BINIX (SIMPLE n, MUL, VAR 'x')
integrate (BINIX (e1, ADD, e2)) = BINIX (integrate e1, ADD, integrate e2)
integrate (BINIX (e1, MIN, e2)) = BINIX (integrate e1, MIN, integrate e2)
integrate (UNIX (NEG, e)) = UNIX (NEG, integrate e)
integrate (VAR 'x') = BINIX (BINIX (SIMPLE 1, DIV, SIMPLE 2), MUL, BINIX (VAR 'x', RAI, SIMPLE 2))
integrate (UNIX (SIN, e)) 
 | isSimple (simplifying (derivate e)) = BINIX (BINIX (SIMPLE 1, DIV, simplifying (derivate e)), MUL, UNIX (NEG, UNIX (COS, e)))
integrate (UNIX (COS, e)) 
 | isSimple (simplifying (derivate e)) = BINIX (BINIX (SIMPLE 1, DIV, simplifying (derivate e)), MUL, UNIX (SIN, e))
integrate (UNIX (TAN, e)) 
 | isSimple (simplifying (derivate e)) = BINIX (BINIX (SIMPLE 1, DIV, simplifying (derivate e)), MUL, UNIX (NEG, UNIX (LN, UNIX (COS, e))))
integrate (UNIX (CTG, e))
 | isSimple (simplifying (derivate e)) = BINIX (BINIX (SIMPLE 1, DIV, simplifying (derivate e)), MUL, UNIX (LN, UNIX (COS, e)))
integrate (BINIX (e, RAI, SIMPLE n)) 
 | isSimple (simplifying (derivate e)) = BINIX (SIMPLE (1 /(n + 1)), MUL, BINIX (e, RAI, SIMPLE (n + 1)))
integrate (BINIX (SIMPLE n, RAI, e)) 
 | isSimple (simplifying (derivate e)) = BINIX (BINIX (simplifying (derivate e), DIV, simplifying (derivate e)), MUL, BINIX (BINIX (SIMPLE n, RAI, e), DIV, UNIX (LN, SIMPLE n)))
-------------------------------
integrate (BINIX (SIMPLE n, MUL, e)) = BINIX (SIMPLE n, MUL, integrate e)
integrate (BINIX (e1, MUL, UNIX (SIN, e2)))
 | isMultipleOf (simplifying (derivate e2)) e1 = UNIX (NEG, UNIX (COS, e2))
-------- Integration by substitution
integrate (BINIX (e1, MUL, e2))
 | isMultipleOf (simplifying (derivate e2)) e1 = BINIX (BINIX (SIMPLE 1, DIV, SIMPLE (difference (simplifying (derivate e2)) e1)), MUL, BINIX (BINIX (e2, RAI, SIMPLE 2), DIV, SIMPLE 2))
-------- Partial integration
 |otherwise = e where
  f = e1
  g' = e2
  g = integrate g'
  f' = simplifying $ derivate f
  e
   | e2 == f' && e1 == g = BINIX (simplifying (BINIX (f, MUL, g)), DIV, SIMPLE 2)
   |otherwise = BINIX (simplifying (BINIX (f, MUL, g)), MIN, (integrate (simplifying (BINIX (f', MUL, g)))))
-- integrate (BINIX (e1, MUL, e2)) = e where