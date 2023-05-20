module Summation (summation) where

import Tokens
import SyntaxTree

summation, summationSum, summationProd, summationSimples :: Expression -> Expression
summation = summationSimples . summationProd . summationSum

summationSimples (BINIX (SIMPLE 0, MUL, _)) = SIMPLE 0
summationSimples (BINIX (SIMPLE 1, MUL, e)) = summation e
summationSimples (BINIX (e, DIV, SIMPLE 1)) = summation e
summationSimples (UNIX (t, SIMPLE n)) = SIMPLE (calculate (UNIX (t, SIMPLE n)))
summationSimples (BINIX (SIMPLE 0, RAI, SIMPLE 0)) = undefined
summationSimples (BINIX (SIMPLE n, t, SIMPLE m)) = SIMPLE (calculate (BINIX (SIMPLE n, t, SIMPLE m)))
summationSimples (BINIX (SIMPLE 0, ADD, e)) = summation e
summationSimples (BINIX (e1, ADD, BINIX (SIMPLE (-1), MUL, e2)))
 | e1 == e2 = SIMPLE 0
 |otherwise = summation $ BINIX (e1, MIN, e2)
summationSimples (BINIX (SIMPLE n, ADD, BINIX (SIMPLE m, ADD, e))) = summation $ BINIX (SIMPLE (n + m), ADD, e)
summationSimples (BINIX (e1, ADD, BINIX (BINIX (SIMPLE (-1), MUL, e2), ADD, e3))) = summation $ BINIX (BINIX (e1, MIN, e2), ADD, e3)
summationSimples (BINIX (SIMPLE (-1), MUL, BINIX (SIMPLE n, MUL, e))) = BINIX (SIMPLE (-n), MUL, summation e)
summationSimples (BINIX (SIMPLE (-1), MUL, UNIX (t, e))) = UNIX (NEG, summation (UNIX (t, e)))
summationSimples (BINIX (e1, MUL, BINIX (e2, RAI, SIMPLE (-1)))) = summation $ BINIX (e1, DIV, e2)
summationSimples (BINIX (_, RAI, SIMPLE 0)) = SIMPLE 1
summationSimples (BINIX (e, RAI, SIMPLE 1)) = summation e
summationSimples (BINIX (e1, t, e2)) = BINIX (summation e1, t, summation e2)
summationSimples (UNIX (t, e)) = UNIX (t, summation e)
summationSimples e = e

summationSum (BINIX (e1, ADD, e2))
 | e1 == e2 = summation $ BINIX (SIMPLE 2, MUL, e1)         -- e + e = 2e
summationSum (BINIX (e1, ADD, BINIX (e2, ADD, f)))
 | e1 == e2 = summation $ BINIX (BINIX (SIMPLE 2, MUL, e1), ADD, f)     -- e + (e + f) = 2e + f
summationSum (BINIX (e1, ADD, BINIX (SIMPLE n, MUL, e2))) 
 | e1 == e2 = summation $ BINIX (SIMPLE (n + 1), MUL, e1)        -- e + ne = (n + 1)e
summationSum (BINIX (e1, ADD, BINIX (BINIX (SIMPLE n, MUL, e2), ADD, f)))
 | e1 == e2 = summation $ BINIX (BINIX (SIMPLE (n + 1), MUL, e1), ADD, f)      -- e + (ne + f) = (n + 1)e + f
summationSum (BINIX (BINIX (SIMPLE n, MUL, e1), ADD, BINIX (SIMPLE m, MUL, e2)))
 | e1 == e2 = BINIX (SIMPLE (n + m), MUL, e1)     -- ne + me = (n + m)e
summationSum (BINIX (BINIX (SIMPLE n, MUL, e1), ADD, BINIX (BINIX (SIMPLE m, MUL, e2), ADD, f)))        -- ne + (me + f) = (n + m)e + f
 | e1 == e2 = summation $ BINIX (BINIX (SIMPLE (n + m), MUL, e1), ADD, f)
summationSum (BINIX (e1, ADD, UNIX (NEG, e2))) = summation $ BINIX (e1, MIN, e2)
--------------------------------------------------------------------------------------------------
summationSum (BINIX (BINIX (UNIX (SIN, e1), RAI, SIMPLE 2), ADD, BINIX (UNIX (COS, e2), RAI, SIMPLE 2)))
 | e1 == e2 = SIMPLE 1
summationSum (BINIX (BINIX (SIMPLE n, MUL, BINIX (UNIX (SIN, e1), RAI, SIMPLE 2)), ADD, BINIX (SIMPLE m, MUL, BINIX (UNIX (COS, e2), RAI, SIMPLE 2))))
 | e1 == e2 && n < 0 = BINIX (SIMPLE 1, ADD, BINIX (BINIX (SIMPLE (n + 1), MUL, BINIX (UNIX (SIN, e1), RAI, SIMPLE 2)), ADD, BINIX (SIMPLE (m + 1), MUL, BINIX (UNIX (COS, e2), RAI, SIMPLE 2))))
 | e1 == e2 && n > 0 = BINIX (SIMPLE 1, ADD, BINIX (BINIX (SIMPLE (n - 1), MUL, BINIX (UNIX (SIN, e1), RAI, SIMPLE 2)), ADD, BINIX (SIMPLE (m - 1), MUL, BINIX (UNIX (COS, e2), RAI, SIMPLE 2))))
--------------------------------------------------------------------------------------------------
summationSum (BINIX (e1, ADD, BINIX (e2, ADD, e3)))
 | e1 == e2 = summation (BINIX (BINIX (SIMPLE 2, MUL, e1), ADD, e3))
summationSum (BINIX (e1, ADD, BINIX (BINIX (SIMPLE (-1), MUL, e2), ADD, e3))) = summation $ BINIX (BINIX (e1, MIN, e2), ADD, e3)
summationSum (BINIX (e1, t, e2)) = BINIX (summation e1, t, summation e2)
summationSum (UNIX (t, e)) = UNIX (t, summation e)
summationSum (VAR x) = VAR x
summationSum (SIMPLE n) = SIMPLE n

summationProd (BINIX (e1, MUL, e2))
 | e1 == e2 = summation $ BINIX (e1, RAI, SIMPLE 2)     -- e * e = e^2
summationProd (BINIX (SIMPLE n, MUL, BINIX (SIMPLE m, MUL, e))) = summation $ BINIX (SIMPLE (n * m), MUL, e)        -- n * (m * e) = (n * m) * e
summationProd (BINIX (e1, MUL, e2))
 | e1 == e2 = BINIX (e1, RAI, SIMPLE 2)        -- e * e = e ^ 2
summationProd (BINIX (e1, MUL, BINIX (e2, MUL, f))) 
 | e1 == e2 = summation $ BINIX (BINIX (e1, RAI, SIMPLE 2), MUL, f)        -- e * (e * f) = e ^ 2 * f
summationProd (BINIX (e1, MUL, BINIX (e2, RAI, SIMPLE n)))
 | e1 == e2 = BINIX (e1, RAI, SIMPLE (n + 1))       -- e * e ^ n = e ^ (n + 1)
summationProd (BINIX (BINIX (e1, RAI, SIMPLE n), MUL, BINIX (e2, RAI, SIMPLE m)))
 | e1 == e2 = BINIX (summation e1, RAI, SIMPLE (n + m))        -- e ^ n * e ^ m = e ^ (n + m)
summationProd (BINIX (BINIX (e1, RAI, e2), MUL, BINIX (e3, DIV, e4))) 
 | e1 == e4 = summation $ BINIX (e3, MUL, BINIX (e1, RAI, BINIX (e2, MIN, SIMPLE 1)))
summationProd (BINIX (BINIX (SIMPLE n, MUL, e1), DIV, e2))
 | e1 == e2 = SIMPLE n
summationProd (BINIX (e1, DIV, e2))
 | e1 == e2 = SIMPLE 1
summationProd (BINIX (SIMPLE (-1), MUL, e)) = UNIX (NEG, e)
summationProd (BINIX (e1, t, e2)) = BINIX (summation e1, t, summation e2)
summationProd (UNIX (t, e)) = UNIX (t, summation e)
summationProd (VAR x) = VAR x
summationProd (SIMPLE n) = SIMPLE n