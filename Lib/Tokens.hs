module Lib.Tokens where

import Lib.Number
import Lib.CReal

import Data.Char
import GHC.Integer
import GHC.Float

data Token
    = ADD
    | MIN
    | MUL
    | DIV
    | RAI
    | UNDER
    | LOG
    | SIN
    | COS
    | TAN
    | CTG
    | OP
    | CL
    | DOT
    | E
    | PI
    | PURE  Int
    | NUM   Number'
    | PARAM Char
 deriving (Show, Eq, Ord)

isInfixR :: Token -> Bool
isInfixR RAI = True
isInfixR _ = False

makeEnd :: CReal -> CReal
makeEnd dl
 | dl < 1   = dl
 |otherwise = makeEnd $ dl / 10

fillingUp :: [Token] -> [Token]
fillingUp [] = []
fillingUp ((PURE n):(PARAM p):ts) = NUM (Integer (toInteger n)) : MUL : fillingUp (PARAM p:ts)
fillingUp ((PARAM p1):(PARAM p2):ts) = PARAM p1 : MUL : fillingUp (PARAM p2:ts)
fillingUp ((PURE n1):DOT:(PURE n2):ts) = NUM (Creal (fromIntegral n1 + makeEnd (fromIntegral n2))) : fillingUp ts
fillingUp ((PURE n):ts) = NUM (Integer (toInteger n)) : fillingUp ts
fillingUp (t:ts) = t : fillingUp ts

stringToTokens :: String -> [Token]
stringToTokens [] = []
stringToTokens ('+':        s) = ADD   : stringToTokens s
stringToTokens ('-':        s) = MIN   : stringToTokens s
stringToTokens ('*':        s) = MUL   : stringToTokens s
stringToTokens ('/':        s) = DIV   : stringToTokens s
stringToTokens ('^':        s) = RAI   : stringToTokens s
stringToTokens ('(':        s) = OP    : stringToTokens s
stringToTokens (')':        s) = CL    : stringToTokens s
stringToTokens ('.':        s) = DOT   : stringToTokens s
stringToTokens ('p':'i':    s) = PI    : stringToTokens s
stringToTokens ('e':        s) = E     : stringToTokens s
stringToTokens (' ':        s) =         stringToTokens s
stringToTokens ('l':'o':'g':'_':s) = list where
 heads = takeWhile (/= ' ') s
 list = (stringToTokens heads) ++ [LOG] ++ (stringToTokens $ dropWhile (/= ' ') s)
stringToTokens ('s':'i':'n':s) = SIN   : stringToTokens s
stringToTokens ('c':'o':'s':s) = COS   : stringToTokens s
stringToTokens ('t':'a':'n':s) = TAN   : stringToTokens s
stringToTokens ('c':'t':'g':s) = CTG   : stringToTokens s
stringToTokens (x:xs)
    | isDigit x  = PURE (read (takeWhile isDigit (x:xs))) : stringToTokens (dropWhile isDigit (x:xs))
    | isLetter x = PARAM x : stringToTokens xs
    | otherwise = undefined
