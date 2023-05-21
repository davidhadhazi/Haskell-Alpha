module Simplification (simplifying) where

import SyntaxTree
import Tokens
import Ordering
import Summation

unbracketing :: Expression -> Expression
unbracketing (BINIX (SIMPLE n, MUL, BINIX (e1, ADD, e2))) = unbracketing $ BINIX (BINIX (SIMPLE n, MUL, e1), ADD, BINIX (SIMPLE n, MUL, e2))
unbracketing (UNIX (NEG, UNIX (NEG, e))) = unbracketing e
unbracketing (UNIX (NEG, SIMPLE n)) = SIMPLE (-n)
unbracketing (UNIX (NEG, e)) = BINIX (SIMPLE (-1), MUL, unbracketing e)
unbracketing (BINIX (e1, MIN, e2)) = BINIX (unbracketing e1, ADD, BINIX (SIMPLE (-1), MUL, unbracketing e2))
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
unbracketing (BINIX (e, RAI, (SIMPLE n)))
 | n < 0 = BINIX (SIMPLE 1, DIV, BINIX (e, RAI, SIMPLE (negate n)))
-----------------------------------------------------
unbracketing (BINIX (e1, t, e2)) = BINIX (unbracketing e1, t, unbracketing e2)
unbracketing (UNIX (t, e)) = UNIX (t, unbracketing e)
unbracketing (SIMPLE n) = SIMPLE n
unbracketing (VAR    x) = VAR    x

rebracketing :: Expression -> Expression
rebracketing (BINIX (BINIX (e1, ADD, e2), ADD, BINIX (e3, ADD, e4))) = rebracketing (BINIX (e1, ADD, (BINIX (e2, ADD, BINIX (e3, ADD, e4)))))
rebracketing (BINIX (BINIX (e1, ADD, e2), ADD, e3)) = rebracketing (BINIX (e1, ADD, (BINIX (e2, ADD, e3))))
rebracketing (BINIX (e1, ADD, BINIX (e2, ADD, e3))) = BINIX (e1, ADD, rebracketing (BINIX (e2, ADD, e3)))
----------------------------------------------------------------
rebracketing (BINIX (BINIX (e1, MUL, e2), MUL, BINIX (e3, MUL, e4))) = rebracketing (BINIX (e1, MUL, (BINIX (e2, MUL, BINIX (e3, MUL, e4)))))
rebracketing (BINIX (BINIX (e1, MUL, e2), MUL, e3)) = rebracketing (BINIX (e1, MUL, (BINIX (e2, MUL, e3))))
rebracketing (BINIX (e1, MUL, BINIX (e2, MUL, e3))) = BINIX (e1, MUL, rebracketing (BINIX (e2, MUL, e3)))
rebracketing e = e

negation :: Expression -> Expression
negation (BINIX (e1, ADD, UNIX (NEG, e2))) = negation $ BINIX (e1, MIN, e2)
negation (BINIX (e1, t, e2)) = BINIX (negation e1, t, negation e2)
negation (UNIX (t, e)) = UNIX (t, negation e)
negation e = e

simplifying :: Expression -> Expression
simplifying e = if e == negation (summation (ordering (rebracketing (unbracketing e))))
     then e else simplifying $ negation $ summation $ ordering $ rebracketing $ unbracketing e