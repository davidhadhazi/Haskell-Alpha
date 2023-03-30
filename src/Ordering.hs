module Ordering (ordering) where

import Tokens
import SyntaxTree

orderingSum, orderingMul, ordering :: Expression -> Expression

ordering = orderingMul . orderingSum

----- Placing simple numbers -----
orderingSum (BINIX (SIMPLE n, ADD, e)) = BINIX (SIMPLE n, ADD, ordering e)          -- n + e = n + e
orderingSum (BINIX (e, ADD, SIMPLE n)) = BINIX (SIMPLE n, ADD, ordering e)          -- e + n = n + e
orderingSum (BINIX (e1, ADD, BINIX (SIMPLE n, ADD, e2))) = BINIX (SIMPLE n, ADD, ordering (BINIX (e1, ADD, e2)))        -- e + (n + f) = n + (e + f)
----- Placing variables -----
orderingSum (BINIX (VAR x, ADD, e)) = BINIX (VAR x, ADD, ordering e)        -- x + e = x + e
orderingSum (BINIX (e, ADD, VAR x)) = BINIX (VAR x, ADD, ordering e)        -- e + x = x + e
orderingSum (BINIX (BINIX (SIMPLE n, MUL, VAR x), ADD, e)) = BINIX (BINIX (SIMPLE n, MUL, VAR x), ADD, ordering e)      -- n*x + e = n*x + e
orderingSum (BINIX (e, ADD, BINIX (SIMPLE n, MUL, VAR x))) = BINIX (BINIX (SIMPLE n, MUL, VAR x), ADD, ordering e)      -- e + n*x = n*x + e
orderingSum (BINIX (e1, ADD, BINIX (VAR x, ADD, e2))) = BINIX (VAR x, ADD, ordering (BINIX (e1, ADD, e2)))      -- e + (x + f) = x + (e + f)
orderingSum (BINIX (e1, ADD, BINIX (BINIX (SIMPLE n, MUL, VAR x), ADD, e2))) = BINIX (BINIX (SIMPLE n, MUL, VAR x), ADD, ordering (BINIX (e1, ADD, e2)))        -- e + (n*x + f) = n*x + (e + f)
----- Placing expression on the n-th power -----
orderingSum (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, BINIX (e2, RAI, SIMPLE m)))                     -- e^n + f^m
 | n > m = BINIX (BINIX (ordering e2, RAI, SIMPLE m), ADD, BINIX (ordering e1, RAI, SIMPLE n))      -- |n > m = f^m + e^n
 |otherwise = BINIX (BINIX (ordering e1, RAI, SIMPLE n), ADD, BINIX (ordering e2, RAI, SIMPLE m))   -- | othw = e^n + f^m
orderingSum (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, BINIX (SIMPLE b, MUL, BINIX (e2, RAI, SIMPLE m))))                          -- e^n + bf^m
 | n > m = BINIX (BINIX (SIMPLE b, MUL, BINIX (ordering e2, RAI, SIMPLE m)), ADD, BINIX (ordering e1, RAI, SIMPLE n))           -- |n > m = bf^m + e^n
 |otherwise = BINIX (BINIX (ordering e1, RAI, SIMPLE n), ADD, BINIX (SIMPLE b, MUL, BINIX (ordering e2, RAI, SIMPLE m)))        -- | othw = e^n + bf^m
orderingSum (BINIX (BINIX (SIMPLE a, MUL, BINIX (e1, RAI, SIMPLE n)), ADD, BINIX (e2, RAI, SIMPLE m)))                      -- ae^n + f^m
 | n > m = BINIX (BINIX (ordering e2, RAI, SIMPLE m), ADD, BINIX (SIMPLE a, MUL, BINIX (ordering e1, RAI, SIMPLE n)))       -- |n > m = f^m + ae^n
 |otherwise = BINIX (BINIX (SIMPLE a, MUL, BINIX (ordering e1, RAI, SIMPLE n)), ADD, BINIX (ordering e2, RAI, SIMPLE m))    -- | othw = ae^n + f^m
orderingSum (BINIX (BINIX (SIMPLE a, MUL, BINIX (e1, RAI, SIMPLE n)), ADD, BINIX (SIMPLE b, MUL, BINIX (e2, RAI, SIMPLE m))))                       -- ae^n + bf^m
 | n > m = BINIX (BINIX (SIMPLE b, MUL, BINIX (ordering e2, RAI, SIMPLE m)), ADD, BINIX (SIMPLE a, MUL, BINIX (ordering e1, RAI, SIMPLE n)))        -- |n > m = bf^m + ae^n
 |otherwise = BINIX (BINIX (SIMPLE a, MUL, BINIX (ordering e1, RAI, SIMPLE n)), ADD, BINIX (SIMPLE b, MUL, BINIX (ordering e2, RAI, SIMPLE m)))     -- | othw = ae^n + bf^m
--------------------------------------------------------
orderingSum (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, BINIX (BINIX (e2, RAI, SIMPLE m), ADD, e3)))                        -- e^n + (f^m + g)
 | n > m = BINIX (BINIX (ordering e2, RAI, SIMPLE m), ADD, ordering (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, e3)))       -- |n > m = f^m + (e^n + g)
 |otherwise = BINIX (BINIX (ordering e1, RAI, SIMPLE n), ADD, ordering (BINIX (BINIX (e2, RAI, SIMPLE m), ADD, e3)))    -- | othw = e^n + (f^m + g)
orderingSum (BINIX (BINIX (SIMPLE a, MUL, BINIX (e1, RAI, SIMPLE n)), ADD, BINIX (BINIX (e2, RAI, SIMPLE m), ADD, e3)))                         -- ae^n + (f^m + g)
 | n > m = BINIX (BINIX (ordering e2, RAI, SIMPLE m), ADD, ordering (BINIX (BINIX (SIMPLE a, MUL, BINIX (e1, RAI, SIMPLE n)), ADD, e3)))        -- |n > m = f^m + (ae^n + g)
 |otherwise = BINIX (BINIX (SIMPLE a, MUL, BINIX (ordering e1, RAI, SIMPLE n)), ADD, ordering (BINIX (BINIX (e2, RAI, SIMPLE m), ADD, e3)))     -- | othw = ae^n + (f^m + g)
 -------------------------------------------------------
orderingSum (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, BINIX (BINIX (SIMPLE b, MUL, BINIX (e2, RAI, SIMPLE m)), ADD, e3)))                         -- e^n + (bf^m + g)
 | n > m = BINIX (BINIX (SIMPLE b, MUL, BINIX (ordering e2, RAI, SIMPLE m)), ADD, ordering (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, e3)))        -- |n > m = f^m + (ae^n + g)
 |otherwise = BINIX (BINIX (ordering e1, RAI, SIMPLE n), ADD, ordering (BINIX (BINIX (SIMPLE b, MUL, BINIX (e2, RAI, SIMPLE m)), ADD, e3)))     -- | othw = ae^n + (f^m + g)
orderingSum (BINIX (BINIX (SIMPLE a, MUL, BINIX (e1, RAI, SIMPLE n)), ADD, BINIX (BINIX (SIMPLE b, MUL, BINIX (e2, RAI, SIMPLE m)), ADD, e3)))                          -- ae^n + (bf^m + g)
 | n > m = BINIX (BINIX (SIMPLE b, MUL, BINIX (ordering e2, RAI, SIMPLE m)), ADD, ordering (BINIX (BINIX (SIMPLE a, MUL, BINIX (e1, RAI, SIMPLE n)), ADD, e3)))         -- |n > m = bf^m + (ae^n + g)
 |otherwise = BINIX (BINIX (SIMPLE a, MUL, BINIX (ordering e1, RAI, SIMPLE n)), ADD, ordering (BINIX (BINIX (SIMPLE b, MUL, BINIX (e2, RAI, SIMPLE m)), ADD, e3)))      -- | othw = ae^n + (bf^m + g)
----------------------------------------------------------
orderingSum (BINIX (e1, ADD, BINIX (BINIX (e2, RAI, SIMPLE n), ADD, e3))) = 
    BINIX (BINIX (ordering e2, RAI, SIMPLE n), ADD, ordering (BINIX (e1, ADD, e3)))     -- e + (f^n + g) = f^n + (e + g)
orderingSum (BINIX (e1, ADD, BINIX (BINIX (SIMPLE a, MUL, BINIX (e2, RAI, SIMPLE n)), ADD, e3))) = 
    BINIX (BINIX (SIMPLE a, MUL, BINIX (ordering e2, RAI, SIMPLE n)), ADD, ordering (BINIX (e1, ADD, e3)))     -- e + (af^n + g) = af^n + (e + g)
orderingSum (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, e2)) = BINIX (BINIX (ordering e1, RAI, SIMPLE n), ADD, ordering e2)     -- e^n + f = f + e^n
orderingSum (BINIX (e1, ADD, BINIX (e2, RAI, SIMPLE n))) = BINIX (BINIX (ordering e2, RAI, SIMPLE n), ADD, ordering e1)     -- f + e^n = f + e^n
orderingSum (BINIX (BINIX (SIMPLE a, MUL, BINIX (e1, RAI, SIMPLE n)), ADD, e2)) = BINIX (BINIX (SIMPLE a, MUL, BINIX (ordering e1, RAI, SIMPLE n)), ADD, ordering e2)     -- ae^n + f = f + ae^n
orderingSum (BINIX (e1, ADD, BINIX (SIMPLE a, MUL, BINIX (e2, RAI, SIMPLE n)))) = BINIX (BINIX (SIMPLE a, MUL, BINIX (ordering e2, RAI, SIMPLE n)), ADD, ordering e1)     -- f + ae^n = f + ae^n
----- Placing funtcions -----
orderingSum (BINIX (UNIX (t1, e1), ADD, UNIX (t2, e2)))                     -- h(e) + i(f)
 | t1 > t2 = BINIX (UNIX (t2, ordering e2), ADD, UNIX (t1, ordering e1))    -- |h > i = i(f) + h(e)
 |otherwise = BINIX (UNIX (t1, ordering e1), ADD, UNIX (t2, ordering e2))   -- | othw = h(e) + i(f)
orderingSum (BINIX (UNIX (t1, e1), ADD, BINIX (UNIX (t2, e2), ADD, e3)))                        -- h(e) + (i(f) + g)
 | t1 > t2 = BINIX (UNIX (t2, ordering e2), ADD, ordering (BINIX (UNIX (t1, e1), ADD, e3)))     -- |h > i = i(f) + (h(e) + g)
 |otherwise = BINIX (UNIX (t1, ordering e1), ADD, ordering (BINIX (UNIX (t2, e2), ADD, e3)))    -- | othw = h(e) + (i(f) + g)
orderingSum (BINIX (UNIX (t, e1), ADD, e2)) = BINIX (UNIX (t, ordering e1), ADD, ordering e2)     -- h(e) + f = h(e) + f
orderingSum (BINIX (e2, ADD, UNIX (t, e1))) = BINIX (UNIX (t, ordering e1), ADD, ordering e2)     -- f + h(e) = h(e) + f
----------------------------------------------------
orderingSum (BINIX (e1, t, e2)) = BINIX (ordering e1, t, ordering e2)
orderingSum (UNIX (t, e)) = UNIX (t, ordering e)
orderingSum (VAR x) = VAR x
orderingSum (SIMPLE n) = SIMPLE n


orderingMul (BINIX (SIMPLE n, MUL, e)) = BINIX ((SIMPLE n), MUL, ordering e)      -- n * e = n * e
orderingMul (BINIX (e, MUL, SIMPLE n)) = BINIX ((SIMPLE n), MUL, ordering e)      -- e * n = n * e
orderingMul (BINIX (e1, MUL, BINIX (SIMPLE n, MUL, e2))) = BINIX (SIMPLE n, MUL, ordering (BINIX (e1, MUL, e2)))        -- e * (n * f) = n * (e * f)
orderingMul (BINIX (VAR x, MUL, e)) = BINIX (VAR x, MUL, ordering e)        -- x * e = x * e
orderingMul (BINIX (e, MUL, VAR x)) = BINIX (VAR x, MUL, ordering e)        -- e * x = x * e
orderingMul (BINIX (e1, MUL, BINIX (VAR x, MUL, e2))) = BINIX (VAR x, MUL, ordering (BINIX (e1, MUL, e2)))      -- e * (x * f) = x * (e * f)
orderingMul (BINIX (BINIX (e1, RAI, SIMPLE n), MUL, BINIX (e2, RAI, SIMPLE m)))                     -- e^n * f^m
 | n > m = BINIX (BINIX (ordering e2, RAI, SIMPLE m), MUL, BINIX (ordering e1, RAI, SIMPLE n))      -- |n > m = f^m * e^n
 |otherwise = BINIX (BINIX (ordering e1, RAI, SIMPLE n), MUL, BINIX (ordering e2, RAI, SIMPLE m))   -- | othw = e^n * f^m
orderingMul (BINIX (BINIX (e1, RAI, SIMPLE n), MUL, BINIX (BINIX (e2, RAI, SIMPLE m), MUL, e3)))                        -- e^n * (f^m * g)
 | n > m = BINIX (BINIX (ordering e2, RAI, SIMPLE m), MUL, ordering (BINIX (BINIX (e1, RAI, SIMPLE n), MUL, e3)))       -- |n > m = f^m * (e^n * g)
 |otherwise = BINIX (BINIX (ordering e1, RAI, SIMPLE n), MUL, ordering (BINIX (BINIX (e2, RAI, SIMPLE m), MUL, e3)))    -- | othw = e^n * (f^m * g)
orderingMul (BINIX (e1, MUL, BINIX (BINIX (e2, RAI, SIMPLE n), MUL, e3))) = 
    BINIX (BINIX (ordering e2, RAI, SIMPLE n), MUL, ordering (BINIX (e1, MUL, e3)))         -- e * (f^n * g) = f^n * (e * g)
orderingMul (BINIX (BINIX (e1, RAI, SIMPLE n), MUL, e2)) = BINIX (BINIX (ordering e1, RAI, SIMPLE n), MUL, ordering e2)     -- e^n * f = e^n * f
orderingMul (BINIX (e1, MUL, BINIX (e2, RAI, SIMPLE n))) = BINIX (BINIX (ordering e2, RAI, SIMPLE n), MUL, ordering e1)     -- f * e^n = e^n * f
orderingMul (BINIX (UNIX (t1, e1), MUL, UNIX (t2, e2)))                     
 | t1 > t2 = BINIX (UNIX (t2, ordering e2), MUL, UNIX (t1, ordering e1))
 |otherwise = BINIX (UNIX (t1, ordering e1), MUL, UNIX (t2, ordering e2))
orderingMul (BINIX (UNIX (t1, e1), MUL, BINIX (UNIX (t2, e2), MUL, e3)))
 | t1 > t2 = BINIX (UNIX (t2, ordering e2), MUL, ordering (BINIX (UNIX (t1, e1), MUL, e3)))
 |otherwise = BINIX (UNIX (t1, ordering e1), MUL, ordering (BINIX (UNIX (t2, e2), MUL, e3)))
orderingMul (BINIX ((UNIX (t, e1)), MUL, e2)) = BINIX (ordering e2, MUL, UNIX (t, ordering e1))
orderingMul e = e