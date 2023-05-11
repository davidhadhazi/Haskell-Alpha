module Ordering (ordering) where

import Tokens
import SyntaxTree
import Data.List (sort)

orderingSum, orderingMul, ordering :: Expression -> Expression

ordering = orderingMul . orderingSum

instance Ord Expression where
    compare (SIMPLE n) (SIMPLE m) = compare n m
    compare (SIMPLE _) _ = LT
    compare _ (SIMPLE _) = GT
    compare (VAR x) (VAR y) = compare x y
    compare (VAR _) _ = LT
    compare _ (VAR _) = GT
    compare (UNIX (t1, e1)) (UNIX (t2, e2))
     | t1 == t2 = compare e1 e2
     |otherwise = compare t1 t2
    compare (UNIX _) _ = LT
    compare _ (UNIX _) = GT
    compare (BINIX (SIMPLE _, MUL, e1)) e2 = compare e1 e2
    compare e1 (BINIX (SIMPLE _, MUL, e2)) = compare e1 e2
    compare (BINIX (e1, RAI, SIMPLE n)) (BINIX (e2, RAI, SIMPLE m))
     | e1 == e2 = compare n m 
     |otherwise = compare e1 e2
    compare (BINIX (_, t1, _)) (BINIX (_, t2, _)) = compare t2 t1

orderingSum (BINIX (e1, ADD, BINIX (e2, ADD, e3))) = list where
    l = sort [e1, e2, e3]
    list = BINIX (head l, ADD, ordering (BINIX (head (tail l), ADD, last l)))
orderingSum (BINIX (e1, ADD, e2))
 | e1 > e2 = BINIX (ordering e2, ADD, ordering e1)
 |otherwise = BINIX (e1, ADD, e2)
orderingSum (BINIX (e1, t, e2)) = BINIX (ordering e1, t, ordering e2)
orderingSum e = e

orderingMul (BINIX (e1, MUL, BINIX (e2, MUL, e3))) = list where
    l = sort [e1, e2, e3]
    list = BINIX (head l, MUL, ordering (BINIX (head (tail l), MUL, last l)))
orderingMul (BINIX (e1, MUL, e2))
 | e1 > e2 = BINIX (ordering e2, MUL, ordering e1)
 |otherwise = BINIX (e1, MUL, e2)
orderingMul (BINIX (e1, t, e2)) = BINIX (ordering e1, t, ordering e2)
orderingMul e = e