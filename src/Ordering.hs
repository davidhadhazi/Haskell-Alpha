module Ordering (ordering) where

import Tokens
import SyntaxTree

orderingSum, orderingMul, ordering :: Expression -> Expression

ordering = orderingMul . orderingSum

----- Placing simple numbers -----
orderingSum (BINIX ((SIMPLE n), ADD, e)) = BINIX ((SIMPLE n), ADD, ordering e)
orderingSum (BINIX (e1, ADD, BINIX (SIMPLE n, ADD, e2))) = BINIX (SIMPLE n, ADD, ordering (BINIX (e1, ADD, e2)))
orderingSum (BINIX (e, ADD, (SIMPLE n))) = BINIX ((SIMPLE n), ADD, ordering e)
----- Placing variables -----
orderingSum (BINIX ((VAR x), ADD, e)) = BINIX ((VAR x), ADD, ordering e)
orderingSum (BINIX (BINIX (SIMPLE n, MUL, VAR x), ADD, e)) = BINIX (BINIX (SIMPLE n, MUL, VAR x), ADD, ordering e)
orderingSum (BINIX (e1, ADD, BINIX (VAR x, ADD, e2))) = BINIX (VAR x, ADD, ordering (BINIX (e1, ADD, e2)))
orderingSum (BINIX (e1, ADD, BINIX (BINIX (SIMPLE n, MUL, VAR x), ADD, e2))) = BINIX (BINIX (SIMPLE n, MUL, VAR x), ADD, ordering (BINIX (e1, ADD, e2)))
orderingSum (BINIX (e, ADD, (VAR x))) = BINIX ((VAR x), ADD, ordering e)
orderingSum (BINIX (e, ADD, BINIX (SIMPLE n, MUL, VAR x))) = BINIX (BINIX (SIMPLE n, MUL, VAR x), ADD, ordering e)
----- Placing expression on the n-th power -----
orderingSum (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, BINIX (e2, RAI, SIMPLE m)))
 | n > m = BINIX (BINIX (ordering e2, RAI, SIMPLE m), ADD, BINIX (ordering e1, RAI, SIMPLE n))
 |otherwise = BINIX (BINIX (e1, RAI, SIMPLE n), ADD, BINIX (e2, RAI, SIMPLE m))
orderingSum (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, BINIX (SIMPLE b, MUL, BINIX (e2, RAI, SIMPLE m))))
 | n > m = BINIX (BINIX (SIMPLE b, MUL, BINIX (ordering e2, RAI, SIMPLE m)), ADD, BINIX (ordering e1, RAI, SIMPLE n))
 |otherwise = BINIX (BINIX (e1, RAI, SIMPLE n), ADD, BINIX (SIMPLE b, MUL, BINIX (e2, RAI, SIMPLE m)))
orderingSum (BINIX (BINIX (SIMPLE a, MUL, BINIX (e1, RAI, SIMPLE n)), ADD, BINIX (e2, RAI, SIMPLE m)))
 | n > m = BINIX (BINIX (ordering e2, RAI, SIMPLE m), ADD, BINIX (SIMPLE a, MUL, BINIX (ordering e1, RAI, SIMPLE n)))
 |otherwise = BINIX (BINIX (SIMPLE a, MUL, BINIX (e1, RAI, SIMPLE n)), ADD, BINIX (e2, RAI, SIMPLE m))
orderingSum (BINIX (BINIX (SIMPLE a, MUL, BINIX (e1, RAI, SIMPLE n)), ADD, BINIX (SIMPLE b, MUL, BINIX (e2, RAI, SIMPLE m))))
 | n > m = BINIX (BINIX (SIMPLE b, MUL, BINIX (ordering e2, RAI, SIMPLE m)), ADD, BINIX (SIMPLE a, MUL, BINIX (ordering e1, RAI, SIMPLE n)))
 |otherwise = BINIX (BINIX (SIMPLE a, MUL, BINIX (e1, RAI, SIMPLE n)), ADD, BINIX (SIMPLE b, MUL, BINIX (e2, RAI, SIMPLE m)))
--------------------------------------------------------
orderingSum (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, BINIX (BINIX (e2, RAI, SIMPLE m), ADD, e3)))
 | n > m = BINIX (BINIX (ordering e2, RAI, SIMPLE m), ADD, ordering (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, e3)))
 |otherwise = BINIX (BINIX (ordering e1, RAI, SIMPLE n), ADD, ordering (BINIX (BINIX (e2, RAI, SIMPLE m), ADD, e3)))
orderingSum (BINIX (BINIX (SIMPLE a, MUL, BINIX (e1, RAI, SIMPLE n)), ADD, BINIX (BINIX (e2, RAI, SIMPLE m), ADD, e3)))
 | n > m = BINIX (BINIX (ordering e2, RAI, SIMPLE m), ADD, ordering (BINIX (BINIX (SIMPLE a, MUL, BINIX (e1, RAI, SIMPLE n)), ADD, e3)))
 |otherwise = BINIX (BINIX (SIMPLE a, MUL, BINIX (ordering e1, RAI, SIMPLE n)), ADD, ordering (BINIX (BINIX (e2, RAI, SIMPLE m), ADD, e3)))
orderingSum (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, BINIX (BINIX (SIMPLE b, MUL, BINIX (e2, RAI, SIMPLE m)), ADD, e3)))
 | n > m = BINIX (BINIX (SIMPLE b, MUL, BINIX (ordering e2, RAI, SIMPLE m)), ADD, ordering (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, e3)))
 |otherwise = BINIX (BINIX (ordering e1, RAI, SIMPLE n), ADD, ordering (BINIX (BINIX (SIMPLE b, MUL, BINIX (e2, RAI, SIMPLE m)), ADD, e3)))
orderingSum (BINIX (BINIX (SIMPLE a, MUL, BINIX (e1, RAI, SIMPLE n)), ADD, BINIX (BINIX (SIMPLE b, MUL, BINIX (e2, RAI, SIMPLE m)), ADD, e3)))
 | n > m = BINIX (BINIX (SIMPLE b, MUL, BINIX (ordering e2, RAI, SIMPLE m)), ADD, ordering (BINIX (BINIX (SIMPLE a, MUL, BINIX (e1, RAI, SIMPLE n)), ADD, e3)))
 |otherwise = BINIX (BINIX (SIMPLE a, MUL, BINIX (ordering e1, RAI, SIMPLE n)), ADD, ordering (BINIX (BINIX (SIMPLE b, MUL, BINIX (e2, RAI, SIMPLE m)), ADD, e3)))
----------------------------------------------------------
orderingSum (BINIX (e1, ADD, BINIX (BINIX (e2, RAI, SIMPLE n), ADD, e3))) = BINIX (BINIX (ordering e2, RAI, SIMPLE n), ADD, BINIX (e1, ADD, e3))
orderingSum (BINIX (BINIX (e1, RAI, SIMPLE n), ADD, e2)) = BINIX (BINIX (ordering e1, RAI, SIMPLE n), ADD, ordering e2)
orderingSum (BINIX (e1, ADD, BINIX (e2, RAI, SIMPLE n))) = BINIX (BINIX (ordering e2, RAI, SIMPLE n), ADD, ordering e1)
----- Placing funtcions -----
orderingSum (BINIX (UNIX (t1, e1), ADD, UNIX (t2, e2)))
 | t1 > t2 = BINIX (UNIX (t2, ordering e2), ADD, UNIX (t1, ordering e1))
 |otherwise = BINIX (UNIX (t1, ordering e1), ADD, UNIX (t2, ordering e2))
orderingSum (BINIX (UNIX (t1, e1), ADD, BINIX (UNIX (t2, e2), ADD, e3)))
 | t1 > t2 = BINIX (UNIX (t2, ordering e2), ADD, ordering (BINIX (UNIX (t1, e1), ADD, e3)))
 |otherwise = BINIX (UNIX (t1, ordering e1), ADD, ordering (BINIX (UNIX (t2, e2), ADD, e3)))
orderingSum (BINIX ((UNIX (t, e1)), ADD, e2)) = BINIX (ordering e2, ADD, UNIX (t, ordering e1))
orderingSum id = id

orderingMul (BINIX ((SIMPLE n), MUL, e)) = BINIX ((SIMPLE n), MUL, ordering e)
orderingMul (BINIX (e1, MUL, BINIX (SIMPLE n, MUL, e2))) = BINIX (SIMPLE n, MUL, ordering (BINIX (e1, MUL, e2)))
orderingMul (BINIX (e, MUL, (SIMPLE n))) = BINIX ((SIMPLE n), MUL, ordering e)
orderingMul (BINIX ((VAR x), MUL, e)) = BINIX ((VAR x), MUL, ordering e)
orderingMul (BINIX (e1, MUL, BINIX (VAR x, MUL, e2))) = BINIX (VAR x, MUL, ordering (BINIX (e1, MUL, e2)))
orderingMul (BINIX (e, MUL, (VAR x))) = BINIX ((VAR x), MUL, ordering e)
orderingMul (BINIX (BINIX (e1, RAI, SIMPLE n), MUL, BINIX (e2, RAI, SIMPLE m)))
 | n > m = BINIX (BINIX (ordering e2, RAI, SIMPLE m), MUL, BINIX (ordering e1, RAI, SIMPLE n))
 |otherwise = BINIX (BINIX (e1, RAI, SIMPLE n), MUL, BINIX (e2, RAI, SIMPLE m))
orderingMul (BINIX (BINIX (e1, RAI, SIMPLE n), MUL, BINIX (BINIX (e2, RAI, SIMPLE m), MUL, e3)))
 | n > m = BINIX (BINIX (ordering e2, RAI, SIMPLE m), MUL, ordering (BINIX (BINIX (e1, RAI, SIMPLE n), MUL, e3)))
 |otherwise = BINIX (BINIX (ordering e1, RAI, SIMPLE n), MUL, ordering (BINIX (BINIX (e2, RAI, SIMPLE m), MUL, e3)))
orderingMul (BINIX (e1, MUL, BINIX (BINIX (e2, RAI, SIMPLE n), MUL, e3))) = BINIX (BINIX (ordering e2, RAI, SIMPLE n), MUL, BINIX (e1, MUL, e3))
orderingMul (BINIX (BINIX (e1, RAI, SIMPLE n), MUL, e2)) = BINIX (BINIX (ordering e1, RAI, SIMPLE n), MUL, ordering e2)
orderingMul (BINIX (e1, MUL, BINIX (e2, RAI, SIMPLE n))) = BINIX (BINIX (ordering e2, RAI, SIMPLE n), MUL, ordering e1)
orderingMul (BINIX (UNIX (t1, e1), MUL, UNIX (t2, e2)))
 | t1 > t2 = BINIX (UNIX (t2, ordering e2), MUL, UNIX (t1, ordering e1))
 |otherwise = BINIX (UNIX (t1, ordering e1), MUL, UNIX (t2, ordering e2))
orderingMul (BINIX (UNIX (t1, e1), MUL, BINIX (UNIX (t2, e2), MUL, e3)))
 | t1 > t2 = BINIX (UNIX (t2, ordering e2), MUL, ordering (BINIX (UNIX (t1, e1), MUL, e3)))
 |otherwise = BINIX (UNIX (t1, ordering e1), MUL, ordering (BINIX (UNIX (t2, e2), MUL, e3)))
orderingMul (BINIX ((UNIX (t, e1)), MUL, e2)) = BINIX (ordering e2, MUL, UNIX (t, ordering e1))