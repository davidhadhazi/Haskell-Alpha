module Ordering (ordering) where

import Tokens
import SyntaxTree
import Data.List (sort)

orderingSum, orderingMul, ordering :: Expression -> Expression

ordering = orderingMul . orderingSum

orderingSum (BINIX (e1, ADD, BINIX (e2, ADD, e3))) = list where
    l = sort [e1, e2, e3]
    list = BINIX (head l, ADD, ordering (BINIX (head (tail l), ADD, last l)))
--------------------------------------------------------------------------------
orderingSum (BINIX (BINIX (SIMPLE a, MUL, e1), ADD, BINIX (SIMPLE b, MUL , e2)))
 | e2 <= e1 = BINIX (ordering (BINIX (SIMPLE b, MUL, e2)), ADD, ordering (BINIX (SIMPLE a, MUL, e1)))
 |otherwise = BINIX (ordering (BINIX (SIMPLE a, MUL, e1)), ADD, ordering (BINIX (SIMPLE b, MUL, e2)))
orderingSum (BINIX (BINIX (SIMPLE a, MUL, e1), ADD, e2))
 | e2 <= e1 = BINIX (ordering e2, ADD, ordering (BINIX (SIMPLE a, MUL, e1)))
 |otherwise = BINIX (ordering (BINIX (SIMPLE a, MUL, e1)), ADD, ordering e2)
orderingSum (BINIX (e1, ADD, BINIX (SIMPLE a, MUL, e2)))
 | e2 <= e1 = BINIX (ordering (BINIX (SIMPLE a, MUL, e2)), ADD, ordering e1)
 |otherwise = BINIX (ordering e1, ADD, ordering (BINIX (SIMPLE a, MUL, e2)))
orderingSum (BINIX (e1, ADD, e2))
 | e2 < e1  = BINIX (ordering e2, ADD, ordering e1)
 |otherwise = BINIX (ordering e1, ADD, ordering e2)
--------------------------------------------------------------------------------
orderingSum (BINIX (e1, t, e2)) = BINIX (ordering e1, t, ordering e2)
orderingSum (UNIX (t, e)) = UNIX (t, ordering e)
orderingSum e = e

orderingMul (BINIX (e1, MUL, BINIX (e2, MUL, e3))) = list where
    l = sort [e1, e2, e3]
    list = BINIX (head l, MUL, ordering (BINIX (head (tail l), MUL, last l)))
orderingMul (BINIX (e1, MUL, e2))
 | e1 > e2 = BINIX (ordering e2, MUL, ordering e1)
 |otherwise = BINIX (e1, MUL, e2)
orderingMul (BINIX (e1, t, e2)) = BINIX (ordering e1, t, ordering e2)
orderingMul e = e