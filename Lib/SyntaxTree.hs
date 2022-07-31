module Lib.SyntaxTree where

import Lib.Tokens
import Lib.Number

newtype LeveledToken = LeveledToken (Token, Int)
 deriving (Show, Eq)

levelUp :: Int -> [Token] -> [LeveledToken]
levelUp 0 [] = []
levelUp a (OP:xs) = levelUp (a + 1) xs
levelUp a (CL:xs) = levelUp (a - 1) xs
levelUp a (x :xs) = LeveledToken (x, a) : levelUp a xs
levelUp _ _ = undefined

instance Ord LeveledToken where
 compare (LeveledToken (t1, n1)) (LeveledToken (t2, n2))
  | n1 == n2 = compare t1 t2
  |otherwise = compare n1 n2

fst' :: LeveledToken -> Token
fst' (LeveledToken (x, _)) = x

data Expression
    = SIMPLE Number'
    | VAR    Char
    | UNIX   (Token, Expression)
    | BINIX  (Expression, Token, Expression)
 deriving Show

buildTree :: [LeveledToken] -> Expression
buildTree [] = undefined
buildTree [LeveledToken (NUM   x, _)] = SIMPLE x
buildTree [LeveledToken (PARAM x, _)] = VAR x
buildTree [LeveledToken (PI,      _)] = SIMPLE pi
buildTree tokens = l where
 lowest = minimum tokens
 list = zip [1..] tokens
 lowestPos = fst $ head $ dropWhile (\(_, t) -> t /= lowest) list
 left  = map snd $ init $ take lowestPos list
 right = map snd $ drop lowestPos list
 l = if null left
    then UNIX (fst' lowest, buildTree right)
    else BINIX (buildTree left, fst' lowest, buildTree right)

makeSyntax :: String -> Expression
makeSyntax str = buildTree $ levelUp 0 $ fillingUp $ stringToTokens str

calculate :: Expression -> Number'
calculate (SIMPLE x)       = x
calculate (UNIX (MIN, exp)) = negate $ calculate exp
calculate (UNIX (SIN, exp)) = sin $ calculate exp
calculate (UNIX (COS, exp)) = cos $ calculate exp
calculate (BINIX (exp1, ADD, exp2)) = (+) (calculate exp1) (calculate exp2)
calculate (BINIX (exp1, MIN, exp2)) = (-) (calculate exp1) (calculate exp2)
calculate (BINIX (exp1, MUL, exp2)) = (*) (calculate exp1) (calculate exp2)
calculate (BINIX (exp1, DIV, exp2)) = e where
  result = (/) (calculate exp1) (calculate exp2)
  rnd = round' result
  e = if result == rnd then rnd else result 
calculate (BINIX (exp1, RAI, exp2)) = c where
  re = (**) (calculate exp1) (calculate exp2)
  rre = round' re
  c = if re == rre then rre else re
calculate _ = undefined
