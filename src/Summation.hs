module Summation (summation) where

import Tokens
import SyntaxTree

summation :: Expression -> Expression
summation (BINIX (e1, t, e2)) = BINIX (summation e1, t, summation e2)
summation (UNIX (t, e)) = UNIX (t, summation e)
summation (VAR x) = VAR x
summation (SIMPLE n) = SIMPLE n