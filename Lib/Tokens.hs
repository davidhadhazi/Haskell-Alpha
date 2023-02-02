module Lib.Tokens where

import Lib.Number
import Lib.CReal
import Data.Char

data Token
 = ADD
 | MIN
 | MUL
 | DIV
 | RAI
 | LOG
 | LOG10
 | LN
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
 deriving (Eq, Ord)

instance Show Token where
    show ADD = "+"
    show MIN = "-"
    show MUL = "*"
    show DIV = "/"
    show RAI = "^"
    show LOG = "log_"
    show LOG10 = "log"
    show LN = "ln"
    show SIN = "sin"
    show COS = "cos"
    show TAN = "tan"
    show CTG = "ctg"
    show E = "e"
    show PI = "pi"
    show _ = undefined

isInfixR :: Token -> Bool
isInfixR RAI = True
isInfixR _ = False

isOperator :: Token -> Bool
isOperator E = False
isOperator PI = False
isOperator (NUM _) = False
isOperator (PARAM _) = False
isOperator _ = True

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
fillingUp (LOG :(PURE n):ts) = NUM (Integer (toInteger n)) : LOG : fillingUp ts
fillingUp (LOG : OP : ts) = [OP] ++ fillingUp (takeColumns 0 ts) ++ [CL] ++ [LOG] ++ fillingUp (dropColumns 0 ts)
fillingUp (t:ts) = t : fillingUp ts

takeColumns :: Int -> [Token] -> [Token]
takeColumns 0 (CL : _) = []
takeColumns n (OP : s) = OP : takeColumns (n+1) s
takeColumns n (CL : s) = CL : takeColumns (n-1) s
takeColumns n (l : s) = l : takeColumns n s
takeColumns _ _ = undefined

dropColumns :: Int -> [Token] -> [Token]
dropColumns 0 (CL : s) = s
dropColumns n (OP : s) = dropColumns (n+1) s
dropColumns n (CL : s) = dropColumns (n-1) s
dropColumns n (l : s) = dropColumns n s
dropColumns _ _ = undefined

stringToTokens :: String -> [Token]
stringToTokens [] = []
stringToTokens ('+':            s) = ADD   : stringToTokens s
stringToTokens ('-':            s) = MIN   : stringToTokens s
stringToTokens ('*':            s) = MUL   : stringToTokens s
stringToTokens ('/':            s) = DIV   : stringToTokens s
stringToTokens ('^':            s) = RAI   : stringToTokens s
stringToTokens ('(':            s) = OP    : stringToTokens s
stringToTokens (')':            s) = CL    : stringToTokens s
stringToTokens ('.':            s) = DOT   : stringToTokens s
stringToTokens ('p':'i':        s) = PI    : stringToTokens s
stringToTokens ('e':            s) = E     : stringToTokens s
stringToTokens (' ':            s) =         stringToTokens s
stringToTokens ('l':'o':'g':'_':s) = LOG   : stringToTokens s
stringToTokens ('l':'o':'g':    s) = LOG10 : stringToTokens s
stringToTokens ('l':'n':        s) = LN    : stringToTokens s
stringToTokens ('s':'i':'n':    s) = SIN   : stringToTokens s
stringToTokens ('c':'o':'s':    s) = COS   : stringToTokens s
stringToTokens ('t':'a':'n':    s) = TAN   : stringToTokens s
stringToTokens ('c':'t':'g':    s) = CTG   : stringToTokens s
stringToTokens (x:xs)
 | isDigit x  = PURE (read (takeWhile isDigit (x:xs))) : stringToTokens (dropWhile isDigit (x:xs))
 | isLetter x = PARAM x : stringToTokens xs
 | otherwise = undefined
