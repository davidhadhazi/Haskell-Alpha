module Lib.Simplification where

import Lib.SyntaxTree
import Lib.Tokens

calculateable :: Expression -> Bool
calculateable (VAR _) = False
calculateable (SIMPLE _) = True
calculateable (UNIX (_, x)) = calculateable x
calculateable (BINIX (exp1, _, exp2)) = (calculateable exp1) && (calculateable exp2)

reduce :: Expression -> Expression
reduce (SIMPLE n) = SIMPLE n
reduce (VAR    x) = VAR    x
reduce (BINIX (e, MUL, (SIMPLE 1))) = e
reduce (BINIX ((SIMPLE 1), MUL, e)) = e
reduce (BINIX (e, MUL, (SIMPLE 0))) = SIMPLE 0
reduce (BINIX ((SIMPLE 0), MUL, e)) = SIMPLE 0
reduce (BINIX ((BINIX (e1, t1, e2)), t2, e3))
 | t1 == t2 && not (isInfixR t1) && calculateable (BINIX (e2, t2, e3)) = BINIX (e1, t1, (SIMPLE (calculate (BINIX (e2, t2, e3)))))
reduce (BINIX (e1, t, e2))
 | calculateable e1 && calculateable e2 = SIMPLE $ calculate $ BINIX (e1, t, e2)
 | calculateable e1 = BINIX ((SIMPLE (calculate e1)), t, (reduce e2))
 | calculateable e2 = BINIX ((reduce e1), t, (SIMPLE (calculate e2)))
 | otherwise = BINIX ((reduce e1), t, (reduce e2))

simplifying :: Expression -> Expression
simplifying e = if e == (reduce e) then e else simplifying $ reduce e