module Lib.Tokens where

import Lib.Number

import Data.Char (isDigit, digitToInt, isLetter)
import GHC.Integer (floatFromInteger)
import GHC.Float

data Token
    = ADD
    | MIN
    | MUL
    | DIV
    | SIN
    | COS
    | RAI
    | OP
    | CL
    | DOT
    | PI
    | PURE  Int
    | NUM   Number'
    | PARAM Char
 deriving (Show, Eq, Ord)

isNum :: Token -> Bool
isNum (NUM _) = True
isNum _ = False

isParam :: Token -> Bool
isParam (PARAM _) = True
isParam _ = False

makeDouble :: Int -> Int -> Number'
makeDouble n1 n2 = Float (int2Float n1 + makeEnd (int2Float n2))

makeEnd :: Float -> Float
makeEnd dl
 | dl < 1   = dl
 |otherwise = makeEnd $ dl / 10

fillingUp :: [Token] -> [Token]
fillingUp [] = []
fillingUp ((PURE n):(PARAM p):ts) = NUM (Integer (toInteger n)) : MUL : fillingUp (PARAM p:ts)
fillingUp ((PARAM p1):(PARAM p2):ts) = PARAM p1 : MUL : fillingUp (PARAM p2:ts)
fillingUp ((PURE n1):DOT:(PURE n2):ts) = NUM (makeDouble n1 n2) : fillingUp ts
fillingUp ((PURE n):ts) = NUM (Integer (toInteger n)) : fillingUp ts
fillingUp (t:ts) = t : fillingUp ts

stringToTokens :: String -> [Token]
stringToTokens [] = []
stringToTokens ('+':        s) = ADD : stringToTokens s
stringToTokens ('-':        s) = MIN : stringToTokens s
stringToTokens ('*':        s) = MUL : stringToTokens s
stringToTokens ('/':        s) = DIV : stringToTokens s
stringToTokens ('^':        s) = RAI : stringToTokens s
stringToTokens ('(':        s) = OP  : stringToTokens s
stringToTokens (')':        s) = CL  : stringToTokens s
stringToTokens ('.':        s) = DOT : stringToTokens s
stringToTokens ('p':'i':    s) = PI  : stringToTokens s
stringToTokens (' ':        s) =        stringToTokens s
stringToTokens ('s':'i':'n':s) = SIN : stringToTokens s
stringToTokens ('c':'o':'s':s) = COS : stringToTokens s
stringToTokens (x:xs)
    | isDigit x  = PURE (read (takeWhile isDigit (x:xs))) : stringToTokens (dropWhile isDigit (x:xs))
    | isLetter x = PARAM x : stringToTokens xs
    | otherwise = undefined
