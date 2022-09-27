--
module Lib.SyntaxTree where

import Lib.Tokens
import Lib.Number
import BooleanFormula (simplify)
import Control.Exception (evaluate, NestedAtomically)

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
-- One attributed tokens
buildTree [LeveledToken (NUM   x, _)] = SIMPLE x
buildTree [LeveledToken (PARAM x, _)] = VAR x
buildTree [LeveledToken (PI,      _)] = SIMPLE pi
-- Multiple attributes
buildTree tokens = l where
  -- The token what we searched for
  lowest = minimum tokens
  list = zip [1..] tokens
  -- In order to find the corrrect operand we need to decide whether it is bound to left or right
  li = if isInfixR (fst' lowest) then reverse list else list
  lowestPos = fst $ head $ dropWhile (\(_, t) -> t /= lowest) $ reverse li
  -- Separating the left and right side of the operand
  left  = map snd $ init $ take lowestPos list
  right = map snd $ drop lowestPos list
  -- Recognizing unix operands
  l = if null left
  then UNIX (fst' lowest, buildTree right)
  else BINIX (buildTree left, fst' lowest, buildTree right)

reducing :: Number' -> Number' -> Number'
reducing x period = x - period * floor (x / period)

makeSyntax :: String -> Expression
makeSyntax str = buildTree $ levelUp 0 $ fillingUp $ stringToTokens str

calculate :: Expression -> Number'
calculate (SIMPLE x)       = x
calculate (BINIX (SIMPLE n, DIV, SIMPLE d)) = n / d
calculate (UNIX (MIN, exp)) = negate $ calculate exp
calculate (UNIX (SIN, exp)) = e where
  si = reducing (calculate exp) $ Creal $ 2 * pi
  e | si == 0 = Integer 0
    | si == (Creal     pi / 6) = Frac ( 1, 2)
    | si == (Creal 5 * pi / 6) = Frac ( 1, 2)
    | si == (Creal 7 * pi / 6) = Frac (-1, 2)
    | otherwise = sin si
calculate (UNIX (COS, exp)) = e where
  co = reducing (calculate exp) $ 2 * Creal pi
  e | co == 0 = Integer 1
    | co == (Creal pi / 3) = Frac (1, 2)
    | co == (Creal pi / 2) = Integer 0
    | co == (Creal 4 * pi / 3) = Frac (-1, 2)
    | otherwise = cos co
calculate (UNIX (TAN, exp)) = tan $ reducing (calculate exp) $ Creal pi
calculate (UNIX (CTG, exp)) = e where
  ctg = (/) 1 $ tan $ calculate exp
  e = if ctg == round ctg then round ctg else ctg
calculate (BINIX (exp1, ADD, exp2)) = (+) (calculate exp1) (calculate exp2)
calculate (BINIX (exp1, MIN, exp2)) = (-) (calculate exp1) (calculate exp2)
calculate (BINIX (exp1, MUL, exp2)) = (*) (calculate exp1) (calculate exp2)
calculate (BINIX (exp1, DIV, exp2)) = e where
  result = (/) (calculate exp1) (calculate exp2)
  rnd = floor result
  e = if result == rnd then rnd else result
calculate (BINIX (exp1, RAI, exp2)) = c where
  re = (**) (calculate exp1) (calculate exp2)
  rre = round re
  c = if re == rre then rre else re
calculate _ = undefined

evaluate :: String -> Number'
evaluate = calculate . makeSyntax
