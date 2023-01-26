module Lib.Derivate where

import Lib.SyntaxTree
import Lib.Tokens

derivate :: Expression -> Expression
derivate (SIMPLE _) = SIMPLE 0
derivate (VAR x) = SIMPLE 1
derivate (BINIX (exp1, ADD, exp2)) = BINIX ((derivate exp1), ADD, (derivate exp2))
derivate (BINIX (exp1, MIN, exp2)) = BINIX ((derivate exp1), MIN, (derivate exp2))
derivate (BINIX (exp1, MUL, exp2)) = BINIX ((BINIX (derivate exp1, MUL, exp2)), ADD, (BINIX (exp1, MUL, derivate exp2)))
derivate (BINIX (exp1, DIV, exp2)) = BINIX ((BINIX ((BINIX (derivate exp1, MUL, exp2)), MIN, (BINIX (exp1, MUL, derivate exp2)))), DIV, (BINIX (exp2, RAI, SIMPLE 2)))
derivate (BINIX (exp1, RAI, exp2)) = BINIX ((BINIX ((exp2), MUL, (BINIX (exp1, RAI, (BINIX (exp2, MUL, SIMPLE 1)))))), MUL, (derivate exp1))  -- (x^y)' = y*x^(y-1)*x'