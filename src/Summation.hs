module Summation (summation) where

import Tokens
import SyntaxTree

summation, summationSum, summationProd :: Expression -> Expression
summation = summationProd . summationSum

summationSum (BINIX (e1, ADD, e2))
 | e1 == e2 = summation $ BINIX (SIMPLE 2, MUL, e1)
summationSum (BINIX (e1, ADD, BINIX (e2, ADD, e3)))
 | e1 == e2 = summation (BINIX (BINIX (SIMPLE 2, MUL, e1), ADD, e3))
summationSum (BINIX (SIMPLE n, ADD, BINIX (SIMPLE m, ADD, e))) = summation $ BINIX (SIMPLE (n + m), ADD, e)     -- n + (m + e) = (n + m) + e
summationSum (BINIX (VAR x, ADD, BINIX (SIMPLE n, MUL, VAR y))) 
 | x == y = BINIX (SIMPLE (n + 1), MUL, VAR x)        -- x + nx = (n + 1)x
summationSum (BINIX (BINIX (SIMPLE n, MUL, VAR x), ADD, BINIX (SIMPLE m, MUL, VAR y)))
 | x == y = BINIX (SIMPLE (n + m), MUL, VAR x)     -- nx + mx = (n + m)x
summationSum (BINIX (VAR x, ADD, BINIX (VAR y, ADD, e)))
 | x == y = summation $ BINIX (BINIX (SIMPLE 2, MUL, VAR x), ADD, e)     -- x + (x + e) = 2x + e
summationSum (BINIX (VAR x, ADD, BINIX (BINIX (SIMPLE n, MUL, VAR y), ADD, e)))
 | x == y = summation $ BINIX (BINIX (SIMPLE (n + 1), MUL, VAR x), ADD, e)      -- x + (nx + e) = (n + 1)x + e
summationSum (BINIX (BINIX (SIMPLE n, MUL, VAR x), ADD, BINIX (BINIX (SIMPLE m, MUL, VAR y), ADD, e)))        -- nx + (mx + e) = (n + m)x + e
 | x == y = summation $ BINIX (BINIX (SIMPLE (n + m), MUL, VAR x), ADD, e)
summationSum (BINIX (BINIX (UNIX (SIN, e1), RAI, SIMPLE 2), ADD, BINIX (UNIX (COS, e2), RAI, SIMPLE 2)))
 | e1 == e2 = SIMPLE 1
summationSum (BINIX (BINIX (UNIX (COS, e1), RAI, SIMPLE 2), ADD, BINIX (UNIX (SIN, e2), RAI, SIMPLE 2)))
 | e1 == e2 = SIMPLE 1
summationSum (BINIX (UNIX (NEG, BINIX (UNIX (SIN, e1), RAI, SIMPLE 2)), MIN, BINIX (UNIX (COS, e2), RAI, SIMPLE 2)))
 | e1 == e2 = SIMPLE 1
summationSum (BINIX (UNIX (NEG, BINIX (UNIX (COS, e1), RAI, SIMPLE 2)), MIN, BINIX (UNIX (SIN, e2), RAI, SIMPLE 2)))
 | e1 == e2 = SIMPLE 1
summationSum (BINIX (e1, t, e2)) = BINIX (summation e1, t, summation e2)
summationSum (UNIX (t, e)) = UNIX (t, summation e)
summationSum (VAR x) = VAR x
summationSum (SIMPLE n) = SIMPLE n

summationProd (BINIX (e1, MUL, e2))
 | e1 == e2 = summation $ BINIX (e1, RAI, SIMPLE 2)
summationProd (BINIX (e1, MUL, UNIX (NEG,e2)))
 | e1 == e2 = summation $ UNIX (NEG, BINIX (e1, RAI, SIMPLE 2))
summationProd (BINIX (SIMPLE n, MUL, BINIX (SIMPLE m, MUL, e))) = summation $ BINIX (SIMPLE (n * m), MUL, e)        -- n * (m * e) = (n * m) * e
summationProd (BINIX (VAR x, MUL, VAR y))
 | x == y = BINIX (VAR x, RAI, SIMPLE 2)        -- x * x = x ^ 2
summationProd (BINIX (VAR x, MUL, BINIX (VAR y, RAI, SIMPLE n)))
 | x == y = BINIX (VAR x, RAI, SIMPLE (n + 1))       -- x * x ^ n = x ^ (n + 1)
summationProd (BINIX (BINIX (VAR x, RAI, SIMPLE n), MUL, BINIX (VAR y, RAI, SIMPLE m)))
 | x == y = BINIX (VAR x, RAI, SIMPLE (n + m))        -- x ^ n * x ^ x ^ m = x ^ (n + m)
summationProd (BINIX (VAR x, MUL, BINIX (VAR y, MUL, e))) 
 | x == y = summation $ BINIX (BINIX (VAR x, RAI, SIMPLE 2), MUL, e)        -- x * (x * e) = x ^ 2 * e
summationProd (BINIX (BINIX (e1, RAI, e2), MUL, BINIX (e3, DIV, e4))) 
 | e1 == e4 = summation $ BINIX (e3, MUL, BINIX (e1, RAI, BINIX (e2, MIN, SIMPLE 1)))
summationProd (BINIX (e1, t, e2)) = BINIX (summation e1, t, summation e2)
summationProd (UNIX (t, e)) = UNIX (t, summation e)
summationProd (VAR x) = VAR x
summationProd (SIMPLE n) = SIMPLE n