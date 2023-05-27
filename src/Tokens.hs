module Tokens (Token (..), stringToTokens, isInfixR, fillingUp) where

import Number
import CReal
import Data.Char

-- Class for the translated syntax
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
 | NEG
 | OP
 | CL
 | DOT
 | E
 | PI
 | PURE  Integer
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
    show NEG = "-"
    show E = "e"
    show PI = "pi"
    show (NUM n) = show n
    show (PURE n) = show n 
    show _ = undefined

-- To define the connects to it's expression
isInfixR :: Token -> Bool
isInfixR RAI = True
isInfixR LOG = True
isInfixR NEG = True
isInfixR _ = False

-- A number can only be a leaf in the syntaxtree
isOperator :: Token -> Bool
isOperator (PURE _) = False
isOperator (NUM _) = False
isOperator (PARAM _) = False
isOperator E = False
isOperator PI = False
isOperator CL = False
isOperator _ = True

-- Makes the pure translated language to a mathematical correct one
fillingUp :: [Token] -> [Token]
fillingUp [] = []
fillingUp ((PURE n):(PARAM p):ts) = NUM (Integer n) : MUL : fillingUp (PARAM p:ts)
fillingUp ((PARAM p1):(PARAM p2):ts) = PARAM p1 : MUL : fillingUp (PARAM p2:ts)
fillingUp ((PURE p):DOT:(NUM (Creal c)):ts) = (NUM (Creal (fromInteger p + c))) : fillingUp ts
fillingUp ((PURE n):ts) = NUM (Integer n) : fillingUp ts
fillingUp (LOG :(PURE n):ts) = NUM (Integer n) : LOG : fillingUp ts
fillingUp (LOG : OP : ts) = [OP] ++ fillingUp (takeColumns 0 ts) ++ [CL] ++ [LOG] ++ fillingUp (dropColumns 0 ts)
fillingUp (t : MIN : ts)
 | isOperator t = fillingUp $ t : NEG : ts
 |otherwise = t : (fillingUp $ MIN : ts) 
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
dropColumns n (_ : s) = dropColumns n s
dropColumns _ _ = undefined

-- Transforms string to a pure list of tokens
stringToTokens :: String -> [Token]
stringToTokens [] = []
stringToTokens ('+':            s) = ADD   : stringToTokens s
stringToTokens ('-':            s) = MIN   : stringToTokens s
stringToTokens ('*':            s) = MUL   : stringToTokens s
stringToTokens ('/':            s) = DIV   : stringToTokens s
stringToTokens ('^':            s) = RAI   : stringToTokens s
stringToTokens ('(':            s) = OP    : stringToTokens s
stringToTokens (')':            s) = CL    : stringToTokens s
stringToTokens ('.':            s) = ts where
    digits = takeWhile isDigit s
    n = NUM $ (Creal (read digits / (10 ^ (length digits)) :: CReal))
    ts = DOT : n : stringToTokens (dropWhile isDigit s)
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
stringToTokens ('x':            s) = PARAM 'x' : stringToTokens s
stringToTokens (x:xs)
 | isDigit x  = PURE (read (takeWhile isDigit (x:xs))) : stringToTokens (dropWhile isDigit (x:xs))
stringToTokens _ = error "Unvalid token."