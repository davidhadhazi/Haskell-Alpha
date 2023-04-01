module Derivate (derivate) where

import SyntaxTree
import Tokens

derivate :: Expression -> Expression
derivate (SIMPLE _) = SIMPLE 0
derivate (VAR _) = SIMPLE 1
derivate (UNIX (NEG, e)) = UNIX (NEG, derivate e)
derivate (BINIX (e1, ADD, e2)) = makeSyntax $ show (derivate e1) ++ "+" ++ show (derivate e2)
derivate (BINIX (e1, MIN, e2)) = makeSyntax $ show (derivate e1) ++ "-" ++ show (derivate e2)
derivate (BINIX (e1, MUL, e2)) = makeSyntax $ show (derivate e1) ++ "*" ++ show e2 ++ "+" ++ show e1 ++ "*" ++ show (derivate e2)
derivate (BINIX (e1, DIV, e2)) = makeSyntax $ "(" ++ show (derivate e1) ++ "*" ++ show e2 ++ "-" ++ show e1 ++ "*" ++ show (derivate e2) ++ ")/" ++
                                               show e2 ++ "^2"
derivate (BINIX (e1, RAI, e2)) = makeSyntax $ show e1 ++ "^" ++ show e2 ++ "*(" ++
                                              show e2 ++ "*" ++ show (derivate e1) ++ "/" ++ show e1 ++ "+" ++ show (derivate e2) ++ "* ln" ++ show e1 ++ ")"
derivate (BINIX (e1, LOG, e2)) = derivate $ BINIX (UNIX (LN, e2), DIV, UNIX (LN, e1))
derivate (UNIX (LOG10, e)) = derivate $ BINIX (UNIX (LN, e), DIV, UNIX (LN, SIMPLE 10))
derivate (UNIX (LN, e)) = BINIX (derivate e, DIV, e)              
derivate (UNIX (SIN, e)) = BINIX (UNIX (COS, e), MUL, derivate e)
derivate (UNIX (COS, e)) = BINIX (UNIX (NEG, UNIX (SIN, e)), MUL, derivate e)
derivate (UNIX (TAN, e)) = derivate $ BINIX (UNIX (SIN, e), DIV, UNIX (COS, e))                                
derivate (UNIX (CTG, e)) = derivate $ BINIX (UNIX (COS, e), DIV, UNIX (SIN, e))
derivate _ = undefined