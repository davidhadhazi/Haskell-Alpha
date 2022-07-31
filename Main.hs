import Lib.Tokens
import Lib.SyntaxTree

f :: (Num p, Floating p) => String-> p
f "a" = 0
f _ = 1.0

main :: IO ()
main = do
    print "asdghg"