module SyntaxTree (makeSyntax, calculate, replace, replace', reducing, isCalculateable, Expression (..)) where

import Tokens
import Number

-- For the brackets we need this type in order to determine how deep is the operation in the syntaxtree
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

-- Class for the expressions, which is a syntaxtree
data Expression
  = SIMPLE Number'
  | VAR    Char
  | UNIX   (Token, Expression)
  | BINIX  (Expression, Token, Expression)

instance Show Expression where
  show (SIMPLE x) = show x
  show (VAR x)    = x : []
  show (UNIX (t, expr)) = show t ++ "(" ++ show expr ++ ")"
  show (BINIX (e1, t, e2)) = "(" ++ show e1 ++ " " ++ show t ++ " " ++ show e2 ++ ")"

instance Eq Expression where
  (==) (SIMPLE n) (SIMPLE m) = n == m
  (==) (VAR x) (VAR y) = x == y
  (==) (UNIX (t1, e1)) (UNIX (t2, e2)) = t1 == t2 && e1 == e2
  (==) (BINIX (a1, t1, b1)) (BINIX (a2, t2, b2)) = t1 == t2 && a1 == a2 && b1 == b2
  (==) _ _ = False

instance Ord Expression where
  compare (SIMPLE n) (SIMPLE m) = compare n m
  compare (SIMPLE _) _ = LT
  compare _ (SIMPLE _) = GT
  compare (VAR x) (VAR y) = compare x y
  compare (VAR _) _ = LT
  compare _ (VAR _) = GT
  compare (BINIX (SIMPLE _, MUL, e1)) e2 = compare e1 e2
  compare e1 (BINIX (SIMPLE _, MUL, e2)) = compare e1 e2
  compare (BINIX (e1, RAI, SIMPLE n)) (BINIX (e2, RAI, SIMPLE m))
    | e1 == e2 = compare n m 
    |otherwise = compare e1 e2
  compare (BINIX (_, t1, _)) (BINIX (_, t2, _)) = compare t2 t1
  compare (BINIX _) _ = LT
  compare _ (BINIX _) = GT
  compare (UNIX (t1, e1)) (UNIX (t2, e2))
    | t1 == t2 = compare e1 e2
    |otherwise = compare t1 t2

-- For calculating the points for the chart
replace :: Double -> Expression -> Expression
replace n (VAR _) = SIMPLE (Creal (realToFrac n))
replace _ (SIMPLE n) = SIMPLE n
replace n (UNIX (t, e)) = UNIX (t, replace n e)
replace n (BINIX (e1, t, e2)) = BINIX (replace n e1, t, replace n e2)

-- For the definite integration
replace' :: Number' -> Expression -> Expression
replace' n (VAR _) = SIMPLE n
replace' _ (SIMPLE n) = SIMPLE n
replace' n (UNIX (t, e)) = UNIX (t, replace' n e)
replace' n (BINIX (e1, t, e2)) = BINIX (replace' n e1, t, replace' n e2)

-- It build is the syntaxtree
buildTree :: [LeveledToken] -> Expression
buildTree [] = error "Empty or wrong syntax"
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

-- Used for the syntaxbuilding from string to expression (does not simplify it)
makeSyntax :: String -> Expression
makeSyntax str = buildTree $ levelUp 0 $ fillingUp $ stringToTokens str

-- Some mathematical expression can't be calculated (such as 1/0)
isCalculateable :: Expression -> Bool
isCalculateable (BINIX (e1, DIV, e2)) = isCalculateable e1 && isCalculateable e2 && calculate e2 /= (Creal 0)
isCalculateable (BINIX (e1, LOG, e2)) = isCalculateable (BINIX (UNIX (LN, e2), DIV, UNIX (LN, e1)))
isCalculateable (UNIX (LOG10, e)) = isCalculateable e && calculate e > (Creal 0)
isCalculateable (UNIX (LN, e)) = isCalculateable e && calculate e > (Creal 0)
isCalculateable (BINIX (e1, RAI, e2)) = isCalculateable e1 && isCalculateable e2 && not (calculate e1 == (Creal 0) && calculate e2 == (Creal 0)) && not (calculate e1 < (Creal 0) && not (isInteger (calculate e2)))
isCalculateable (UNIX (TAN, e)) = isCalculateable e && (Creal 0) /= calculate (UNIX (COS, e))
isCalculateable (UNIX (CTG, e)) = isCalculateable e && (Creal 0) /= calculate (UNIX (SIN, e))
--------------------------------------------------------------
isCalculateable (BINIX (e1, _, e2)) = isCalculateable e1 && isCalculateable e2
isCalculateable (UNIX (_, e)) = isCalculateable e
isCalculateable (SIMPLE _) = True
isCalculateable (VAR _) = False

-- Calculates a number from the expression (does not checks whether it is calculateable or not)
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
calculate (UNIX (TAN, expr)) = calculate (BINIX (UNIX (SIN, expr), DIV, UNIX (COS, expr)))
calculate (UNIX (CTG, expr)) = calculate (BINIX (UNIX (COS, expr), DIV, UNIX (SIN, expr)))
calculate (BINIX (exp1, LOG, exp2)) = calculate (BINIX (UNIX (LN, exp2), DIV, UNIX (LN, exp1)))
calculate (UNIX (LOG10, expr)) = calculate (BINIX (UNIX (LN, expr), DIV, UNIX (LN, SIMPLE 10)))
calculate (UNIX (LN, expr)) 
 | round' (calculate expr) <= Integer 0 = error "Log of zero or negative number"
 |otherwise = round' $ log $ calculate expr
calculate (BINIX (exp1, ADD, exp2)) = round' $  (+) (calculate exp1) (calculate exp2)
calculate (BINIX (exp1, MIN, exp2)) = round' $  (-) (calculate exp1) (calculate exp2)
calculate (BINIX (exp1, MUL, exp2)) = round' $  (*) (calculate exp1) (calculate exp2)
calculate (BINIX (exp1, DIV, exp2)) = if (0 == calculate exp2) then error "Division by zero" else round' $  (/) (calculate exp1) (calculate exp2)
calculate (BINIX (exp1, RAI, exp2))
 | round' (calculate exp1) == Integer 0 && round' (calculate exp2) == Integer 0 = error "Zero on the power of zero"
 | round' (calculate exp1) == Integer 0 = Integer 0
 | round' (calculate exp2) == Integer 0 = Integer 1
 | round' (calculate exp1) == Integer 1 = Integer 1
 | round' (calculate exp1) == Integer 0 = Integer 0
 | (calculate exp1) < 0 = case (calculate exp2) of
    Integer n -> if n > 0 then calculate (BINIX (exp1, MUL, BINIX (exp1, RAI, SIMPLE ((calculate exp2) - 1)))) else 
                               calculate $ BINIX (SIMPLE 1, DIV, BINIX (SIMPLE (calculate exp1), RAI, SIMPLE (negate (calculate exp2))))
    _ -> error "Root of a negative number"
 |otherwise = round' $ (**) (calculate exp1) (calculate exp2)
calculate _ = error "Not a valid number"