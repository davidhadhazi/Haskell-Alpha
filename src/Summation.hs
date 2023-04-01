module Summation (summation) where

import Tokens
import SyntaxTree

summation, summationSum, summationProd :: Expression -> Expression
summation = summationProd . summationSum

summationSum (BINIX (SIMPLE n, ADD, BINIX (SIMPLE m, ADD, e))) = summation $ BINIX (SIMPLE (n + m), ADD, e)     -- n + (m + e) = (n + m) + e
summationSum (BINIX (VAR x, ADD, VAR y)) = BINIX (SIMPLE 2, MUL, VAR x)         -- x + x = 2x
summationSum (BINIX (VAR x, ADD, BINIX (SIMPLE n, MUL, VAR y))) = BINIX (SIMPLE (n + 1), MUL, VAR x)        -- x + nx = (n + 1)x
summationSum (BINIX (BINIX (SIMPLE n, MUL, VAR x), ADD, BINIX (SIMPLE m, MUL, VAR y))) = BINIX (SIMPLE (n + m), MUL, VAR x)     -- nx + mx = (n + m)x
summationSum (BINIX (VAR x, ADD, BINIX (VAR y, ADD, e))) = summation $ BINIX (BINIX (SIMPLE 2, MUL, VAR x), ADD, e)     -- x + (x + e) = 2x + e
summationSum (BINIX (VAR x, ADD, BINIX (BINIX (SIMPLE n, MUL, VAR y), ADD, e))) = summation $ BINIX (BINIX (SIMPLE (n + 1), MUL, VAR x), ADD, e)      -- x + (nx + e) = (n + 1)x + e
summationSum (BINIX (BINIX (SIMPLE n, MUL, VAR x), ADD, BINIX (BINIX (SIMPLE m, MUL, VAR y), ADD, e))) =        -- nx + (mx + e) = (n + m)x + e
    summation $ BINIX (BINIX (SIMPLE (n + m), MUL, VAR x), ADD, e)
summationSum (BINIX (e1, t, e2)) = BINIX (summation e1, t, summation e2)
summationSum (UNIX (t, e)) = UNIX (t, summation e)
summationSum (VAR x) = VAR x
summationSum (SIMPLE n) = SIMPLE n

summationProd (BINIX (SIMPLE n, MUL, BINIX (SIMPLE m, MUL, e))) = summation $ BINIX (SIMPLE (n * m), MUL, e)        -- n * (m * e) = (n * m) * e
summationProd (BINIX (VAR x, MUL, VAR y)) = BINIX (VAR x, RAI, SIMPLE 2)        -- x * x = x ^ 2
summationProd (BINIX (VAR x, MUL, BINIX (VAR y, RAI, SIMPLE n))) = BINIX (VAR x, RAI, SIMPLE (n + 1))       -- x * x ^ n = x ^ (n + 1)
summationProd (BINIX (BINIX (VAR x, RAI, SIMPLE n), MUL, BINIX (VAR y, RAI, SIMPLE m))) = BINIX (VAR x, RAI, SIMPLE (n + m))        -- x ^ n * x ^ x ^ m = x ^ (n + m)
summationProd (BINIX (VAR x, MUL, BINIX (VAR y, MUL, e))) = summation $ BINIX (BINIX (VAR x, RAI, SIMPLE 2), MUL, e)        -- x * (x * e) = x ^ 2 * e
summationProd (BINIX (e1, t, e2)) = BINIX (summation e1, t, summation e2)
summationProd (UNIX (t, e)) = UNIX (t, summation e)
summationProd (VAR x) = VAR x
summationProd (SIMPLE n) = SIMPLE n