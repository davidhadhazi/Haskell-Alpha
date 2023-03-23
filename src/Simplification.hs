module Simplification (simplifying) where

import SyntaxTree
import Tokens

calculateable :: Expression -> Bool
calculateable (VAR _) = False
calculateable (SIMPLE _) = True
calculateable (UNIX (_, x)) = calculateable x
calculateable (BINIX (exp1, _, exp2)) = (calculateable exp1) && (calculateable exp2)

reduce :: Expression -> Expression
reduce (SIMPLE n) = SIMPLE n
reduce (VAR    x) = VAR    x
reduce (UNIX (t, e)) = if calculateable e then SIMPLE (calculate (UNIX (t, e))) else UNIX (t, reduce e)
reduce (BINIX (e, ADD, (SIMPLE 0))) = e
reduce (BINIX ((SIMPLE 0), ADD, e)) = e
reduce (BINIX (e, MUL, (SIMPLE 1))) = e
reduce (BINIX ((SIMPLE 1), MUL, e)) = e
reduce (BINIX (_, MUL, (SIMPLE 0))) = SIMPLE 0
reduce (BINIX ((SIMPLE 0), MUL, _)) = SIMPLE 0
reduce (BINIX ((SIMPLE n), MUL, BINIX ((SIMPLE m), DIV, e))) = BINIX ((SIMPLE (n * m)), DIV, e)
reduce (BINIX ((BINIX ((SIMPLE m), DIV, e)), MUL, (SIMPLE n))) = BINIX ((SIMPLE (n * m)), DIV, e)
reduce (BINIX (e1, t, e2))
 | calculateable e1 && calculateable e2 = SIMPLE $ calculate $ BINIX (e1, t, e2)
 | calculateable e1 = BINIX ((SIMPLE (calculate e1)), t, (reduce e2))
 | calculateable e2 = BINIX ((reduce e1), t, (SIMPLE (calculate e2)))
 | otherwise = BINIX ((reduce e1), t, (reduce e2))

unbracketing :: Expression -> Expression
unbracketing (UNIX (NEG, UNIX (NEG, e))) = unbracketing e       --      - - a = a
unbracketing (UNIX (NEG, (BINIX (e1, MIN, e2)))) = BINIX (unbracketing e2, MIN, unbracketing e1)        --      -(a - b) = b - a
unbracketing (BINIX (e1, ADD, (UNIX (NEG, e2)))) = BINIX (unbracketing e1, MIN, unbracketing e2)        --      (a + -b) = a - b
unbracketing (BINIX ((UNIX (NEG, e1)), ADD, e2)) = BINIX (unbracketing e2, MIN, unbracketing e1)        --      (-a + b) = b - a
unbracketing (BINIX (e1, MIN, UNIX (NEG, e2)))   = BINIX (e1, ADD, e2)                                  --      (a - -b) = a + b
unbracketing (UNIX (NEG, BINIX (e1, ADD, e2))) = unbracketing $ BINIX                                   --      -(a + b) = -a - b 
 (unbracketing (UNIX (NEG, e1)), ADD, unbracketing (UNIX (NEG, e2)))
-----------------------------------------------------
unbracketing (UNIX (NEG, BINIX (e1, MUL, UNIX (NEG, e2)))) = BINIX (unbracketing e1, MUL, unbracketing e2)      --      -(a * -b) = a * b
unbracketing (UNIX (NEG, BINIX ((UNIX (NEG, e1)), MUL, e2))) = BINIX (unbracketing e1, MUL, unbracketing e2)    --      -(-a * b) = a * b
unbracketing (UNIX (NEG, BINIX (e1, DIV, UNIX (NEG, e2)))) = BINIX (unbracketing e1, DIV, unbracketing e2)      --      -(a / -b) = a / b
unbracketing (UNIX (NEG, BINIX ((UNIX (NEG, e1)), DIV, e2))) = BINIX (unbracketing e1, DIV, unbracketing e2)    --      -(-a / b) = a / b
-----------------------------------------------------
unbracketing (UNIX (LN, BINIX (exp1, RAI, exp2))) = BINIX (unbracketing exp2, MUL, UNIX (LN, unbracketing exp1))        --      ln (a^b) = b * ln a
unbracketing (UNIX (LN, BINIX (UNIX (NEG, exp1), MUL, UNIX (NEG, exp2)))) = 
 BINIX (UNIX (LN, unbracketing exp1), ADD, UNIX (LN, unbracketing exp2))                                                        -- ln (-a*-b) = ln a + ln b
unbracketing (UNIX (LN, BINIX (exp1, MUL, exp2))) = BINIX (UNIX (LN, unbracketing exp1), ADD, UNIX (LN, unbracketing exp2))     -- ln (a*b) = ln a + ln b
unbracketing (UNIX (LN, BINIX (UNIX (NEG, exp1), DIV, UNIX (NEG, exp2)))) =
 BINIX (UNIX (LN, unbracketing exp1), MIN, UNIX (LN, unbracketing exp2))                                                        -- ln (-a/-b) = ln a - ln b  
unbracketing (UNIX (LN, BINIX (exp1, DIV, exp2))) = BINIX (UNIX (LN, unbracketing exp1), MIN, UNIX (LN, unbracketing exp2))     -- ln (a/b) = ln a - ln b   
-----------------------------------------------------
unbracketing (UNIX (LOG10, BINIX (exp1, RAI, exp2))) = BINIX (unbracketing exp2, MUL, UNIX (LOG10, exp1))
unbracketing (UNIX (LOG10, BINIX (UNIX (NEG, exp1), MUL, UNIX (NEG, exp2)))) = 
 BINIX (UNIX (LOG10, unbracketing exp1), ADD, UNIX (LOG10, unbracketing exp2))
unbracketing (UNIX (LOG10, BINIX (exp1, MUL, exp2))) = BINIX (UNIX (LOG10, unbracketing exp1), ADD, UNIX (LOG10, unbracketing exp2))
unbracketing (UNIX (LOG10, BINIX (UNIX (NEG, exp1), DIV, UNIX (NEG, exp2)))) =
 BINIX (UNIX (LOG10, unbracketing exp1), MIN, UNIX (LOG10, unbracketing exp2))
unbracketing (UNIX (LOG10, BINIX (exp1, DIV, exp2))) = BINIX (UNIX (LOG10, unbracketing exp1), MIN, UNIX (LOG10, unbracketing exp2))
-----------------------------------------------------
unbracketing (BINIX (n, LOG, BINIX (exp1, RAI, exp2))) = BINIX (unbracketing exp2, MUL, BINIX (n, LOG, exp1))
unbracketing (BINIX (n, LOG, BINIX (UNIX (NEG, exp1), MUL, UNIX (NEG, exp2)))) = 
 BINIX (BINIX (n, LOG, unbracketing exp1), ADD, BINIX (n, LOG, unbracketing exp2))
unbracketing (BINIX (n, LOG, BINIX (exp1, MUL, exp2))) = BINIX (BINIX (n, LOG, unbracketing exp1), ADD, BINIX (n, LOG, unbracketing exp2))
unbracketing (BINIX (n, LOG, BINIX (UNIX (NEG, exp1), DIV, UNIX (NEG, exp2)))) =
 BINIX (BINIX (n, LOG, unbracketing exp1), MIN, BINIX (n, LOG, unbracketing exp2))
unbracketing (BINIX (n, LOG, BINIX (exp1, DIV, exp2))) = BINIX (BINIX (n, LOG, unbracketing exp1), MIN, BINIX (n, LOG, unbracketing exp2))
-----------------------------------------------------
unbracketing (BINIX (e1, t, e2)) = BINIX (unbracketing e1, t, unbracketing e2)
unbracketing (UNIX (t, e)) = UNIX (t, unbracketing e)
unbracketing (SIMPLE n) = SIMPLE n
unbracketing (VAR    x) = VAR    x

rebracketing :: Expression -> Expression
rebracketing (BINIX (BINIX (e1, ADD, e2), ADD, BINIX (e3, ADD, e4))) = rebracketing (BINIX (e1, ADD, (BINIX (e2, ADD, BINIX (e3, ADD, e4)))))
rebracketing (BINIX (BINIX (e1, ADD, e2), ADD, e3)) = rebracketing (BINIX (e1, ADD, (BINIX (e2, ADD, e3))))
rebracketing (BINIX (e1, ADD, BINIX (e2, ADD, e3))) = BINIX (e1, ADD, rebracketing (BINIX (e2, ADD, e3)))
rebracketing e = e

ordering :: Expression -> Expression
ordering (BINIX ((SIMPLE n), ADD, e)) = BINIX ((SIMPLE n), ADD, ordering e)
ordering (BINIX (e1, ADD, BINIX (SIMPLE n, ADD, e2))) = BINIX (SIMPLE n, ADD, ordering (BINIX (e1, ADD, e2)))
ordering (BINIX (e, ADD, (SIMPLE n))) = BINIX ((SIMPLE n), ADD, ordering e)
ordering (BINIX ((VAR x), ADD, e)) = BINIX ((VAR x), ADD, ordering e)
ordering (BINIX (e1, ADD, BINIX (VAR x, ADD, e2))) = BINIX (VAR x, ADD, ordering (BINIX (e1, ADD, e2)))
ordering (BINIX (e, ADD, (VAR x))) = BINIX ((VAR x), ADD, ordering e)
ordering (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, BINIX (e2, RAI, SIMPLE m)))
 | n > m = BINIX (BINIX (ordering e2, RAI, SIMPLE m), ADD, BINIX (ordering e1, RAI, SIMPLE n))
 |otherwise = BINIX (BINIX (e1, RAI, SIMPLE n), ADD, BINIX (e2, RAI, SIMPLE m))
ordering (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, BINIX (BINIX (e2, RAI, SIMPLE m), ADD, e3)))
 | n > m = BINIX (BINIX (ordering e2, RAI, SIMPLE m), ADD, ordering (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, e3)))
 |otherwise = BINIX (BINIX (ordering e1, RAI, SIMPLE n), ADD, ordering (BINIX (BINIX (e2, RAI, SIMPLE m), ADD, e3)))
ordering (BINIX (e1, ADD, BINIX (BINIX (e2, RAI, SIMPLE n), ADD, e3))) = BINIX (BINIX (ordering e2, RAI, SIMPLE n), ADD, BINIX (e1, ADD, e3))
ordering (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, e2)) = BINIX (BINIX (ordering e1, RAI, SIMPLE n), ADD, ordering e2)
ordering (BINIX (e1, ADD, BINIX (e2, RAI, SIMPLE n))) = BINIX (BINIX (ordering e2, RAI, SIMPLE n), ADD, ordering e1)
ordering (BINIX (UNIX (t1, e1), ADD, UNIX (t2, e2)))
 | t1 > t2 = BINIX (UNIX (t2, ordering e2), ADD, UNIX (t1, ordering e1))
 |otherwise = BINIX (UNIX (t1, ordering e1), ADD, UNIX (t2, ordering e2))
ordering (BINIX (UNIX (t1, e1), ADD, BINIX (UNIX (t2, e2), ADD, e3)))
 | t1 > t2 = BINIX (UNIX (t2, ordering e2), ADD, ordering (BINIX (UNIX (t1, e1), ADD, e3)))
 |otherwise = BINIX (UNIX (t1, ordering e1), ADD, ordering (BINIX (UNIX (t2, e2), ADD, e3)))
ordering (BINIX ((UNIX (t, e1)), ADD, e2)) = BINIX (ordering e2, ADD, UNIX (t, ordering e1))
-----------------------------------------------------
ordering (BINIX ((SIMPLE n), MUL, e)) = BINIX ((SIMPLE n), MUL, ordering e)
ordering (BINIX (e, MUL, (SIMPLE n))) = BINIX ((SIMPLE n), MUL, ordering e)
-----------------------------------------------------
ordering (BINIX (e1, t, e2)) = BINIX (ordering e1, t, ordering e2)
ordering (UNIX (t, e)) = UNIX (t, ordering e)
ordering (SIMPLE n) = SIMPLE n
ordering (VAR x) = VAR x

simplifying :: Expression -> Expression
simplifying e = if e == reduce (ordering (rebracketing (unbracketing e))) then e else simplifying $ reduce $ ordering $ rebracketing $ unbracketing e