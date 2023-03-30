module SyntaxTree (makeSyntax, calculate, Expression (..)) where

import Tokens
import Number

newtype LeveledToken = LeveledToken (Token, Int)
 deriving (Eq)

instance Show LeveledToken where
  show (LeveledToken (t, l)) = show (t, l)

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

instance Eq Expression where
  (==) (SIMPLE n) (SIMPLE m) = n == m
  (==) (SIMPLE _) _ = False
  (==) (VAR x) (VAR y) = x == y
  (==) (VAR _) _ = False
  (==) (UNIX (t1, e1)) (UNIX (t2, e2)) = t1 == t2 && e1 == e2
  (==) (UNIX _) _ = False
  (==) (BINIX (a1, t1, b1)) (BINIX (a2, t2, b2)) = t1 == t2 && a1 == a2 && b1 == b2
  (==) (BINIX _) _ = False

instance Show Expression where
  show (SIMPLE x) = show x
  show (VAR x)    = x : []
  show (UNIX (t, expr)) = show t ++ "(" ++ show expr ++ ")"
  show (BINIX (e1, t, e2)) = "(" ++ show e1 ++ " " ++ show t ++ " " ++ show e2 ++ ")"

buildTree :: [LeveledToken] -> Expression
-- One attributed tokens
buildTree [LeveledToken (NUM   x, _)] = SIMPLE x
buildTree [LeveledToken (PARAM x, _)] = VAR x
buildTree [LeveledToken (PI,      _)] = SIMPLE pi
buildTree [LeveledToken (E,       _)] = SIMPLE $ exp 1
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
  l
   | fst' lowest == MIN && null left = UNIX (NEG, buildTree right)
   | null left = UNIX (fst' lowest, buildTree right)
   | otherwise = BINIX (buildTree left, fst' lowest, buildTree right)

reducing :: Number' -> Number' -> Number'
reducing x period = x - period * floor (x / period)

makeSyntax :: String -> Expression
makeSyntax str = buildTree $ levelUp 0 $ fillingUp $ stringToTokens str

calculate :: Expression -> Number'
calculate (SIMPLE x)       = x
calculate (UNIX (NEG, expr)) = negate $ calculate expr
calculate (UNIX (SIN, expr)) = e where
  si = reducing (calculate expr) $ Creal $ 2 * pi
  e | si == Creal 0 = Integer 0
    | si == (Creal     pi / 6) = Frac ( 1, 2)
    | si == (Creal 5 * pi / 6) = Frac ( 1, 2)
    | si == Creal      pi      = Integer 0
    | si == (Creal 7 * pi / 6) = Frac (-1, 2)
    | si == (Creal 11* pi / 6) = Frac (-1, 2)
    | otherwise = sin si
calculate (UNIX (COS, expr)) = e where
  co = reducing (calculate expr) $ 2 * Creal pi
  e | co == Creal 0 = Integer 1
    | co == (Creal pi / 3) = Frac (1, 2)
    | co == (Creal pi / 2) = Integer 0
    | co == (Creal 2 * pi / 3) = Frac (-1, 2)
    | co == (Creal 4 * pi / 3) = Frac (-1, 2)
    | co == (Creal 3 * pi / 2) = Integer 0
    | co == (Creal 5 * pi / 3) = Frac (1, 2)
    | otherwise = cos co
calculate (UNIX (TAN, expr)) = e where
  ta = reducing (calculate expr) $ Creal pi
  e | ta == (Creal pi / 4) = Integer 1
    | ta == (Creal 3 * pi / 4) = Integer $ -1
    | otherwise = tan ta
calculate (UNIX (CTG, expr)) = e where
  ct = reducing (calculate expr) $ Creal pi
  e | ct == 0 = Integer 0
      |otherwise = 1 / tan ct
calculate (BINIX (exp1, LOG, exp2)) = round' $ (/) (log (calculate exp2)) $ log $ calculate exp1
calculate (UNIX (LOG10, expr)) = round' $ (/) (log (calculate expr)) $ log 10
calculate (UNIX (LN, expr)) = round' $ log $ calculate expr
calculate (BINIX (exp1, ADD, exp2)) = round' $  (+) (calculate exp1) (calculate exp2)
calculate (BINIX (exp1, MIN, exp2)) = round' $  (-) (calculate exp1) (calculate exp2)
calculate (BINIX (exp1, MUL, exp2)) = round' $  (*) (calculate exp1) (calculate exp2)
calculate (BINIX (exp1, DIV, exp2)) = round' $  (/) (calculate exp1) (calculate exp2)
calculate (BINIX (exp1, RAI, exp2)) = round' $ (**) (calculate exp1) (calculate exp2)
calculate _ = undefined

