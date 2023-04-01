module TestCases ( tests ) where

import Test.HUnit
import Tokens
import SyntaxTree
import Number
import Simplification

tokentest0 :: Test
tokentest0 = TestCase (assertEqual "tokenNumber0" (stringToTokens "") [])
tokentest1 :: Test
tokentest1 = TestCase (assertEqual "tokenNumber1" (stringToTokens "1 1") [PURE 1, PURE 1])
tokentest2 :: Test
tokentest2 = TestCase (assertEqual "tokenNumber2" (stringToTokens "1                1") [PURE 1, PURE 1])
tokentest3 :: Test
tokentest3 = TestCase (assertEqual "tokenNumber3" (stringToTokens "123") [PURE 123])
tokentest4 :: Test
tokentest4 = TestCase (assertEqual "tokenNumber4" (fillingUp (stringToTokens "1.2")) [NUM 1.2])
tokentest5 :: Test
tokentest5 = TestCase (assertEqual "tokenNumber5" (fillingUp (stringToTokens "pi")) [PI])
tokentest6 :: Test
tokentest6 = TestCase (assertEqual "tokenNumber6" (fillingUp (stringToTokens "e")) [E])

tokens :: Test
tokens = TestList[
                  TestLabel "token0" tokentest0,
                  TestLabel "token1" tokentest1,
                  TestLabel "token2" tokentest2, 
                  TestLabel "token3" tokentest3,
                  TestLabel "token4" tokentest4,
                  TestLabel "token5" tokentest5,
                  TestLabel "token6" tokentest6
                  ]

syntaxtest0 :: Test
syntaxtest0 = TestCase (assertEqual "syntax0" (makeSyntax "1") (SIMPLE 1))
syntaxtest1 :: Test
syntaxtest1 = TestCase (assertEqual "syntax1" (makeSyntax "x") (VAR 'x'))
syntaxtest2 :: Test
syntaxtest2 = TestCase (assertEqual "syntax2" (makeSyntax "1 + 1") (BINIX (SIMPLE 1, ADD, SIMPLE 1)))
syntaxtest3 :: Test
syntaxtest3 = TestCase (assertEqual "syntax3" (makeSyntax "sin pi") (UNIX (SIN, SIMPLE (Creal pi))))
syntaxtest4 :: Test
syntaxtest4 = TestCase (assertEqual "syntax4" (makeSyntax "2x") (BINIX (SIMPLE 2, MUL, VAR 'x')))
syntaxtest5 :: Test
syntaxtest5 = TestCase (assertEqual "syntax5" (makeSyntax "1 + 2 + 3") (BINIX (BINIX (SIMPLE 1, ADD, SIMPLE 2), ADD, SIMPLE 3)))
syntaxtest6 :: Test
syntaxtest6 = TestCase (assertEqual "syntax6" (makeSyntax "1 + (2 + 3)") (BINIX (SIMPLE 1, ADD, BINIX (SIMPLE 2, ADD, SIMPLE 3))))
syntaxtest7 :: Test
syntaxtest7 = TestCase (assertEqual "syntax7" (makeSyntax "cos (x^2)") (UNIX (COS, BINIX (VAR 'x', RAI, SIMPLE 2))))
syntaxtest8 :: Test
syntaxtest8 = TestCase (assertEqual "syntax8" (makeSyntax "log_2 4 + ln 3 * log 5") (BINIX (BINIX (SIMPLE 2, LOG, SIMPLE 4), ADD, BINIX (UNIX (LN, SIMPLE 3), MUL, UNIX (LOG10, SIMPLE 5)))))
syntaxtest9 :: Test
syntaxtest9 = TestCase (assertEqual "syntax9" (makeSyntax "tan x y") (BINIX (UNIX (TAN, VAR 'x'), MUL, VAR 'y')))
syntaxtest10 :: Test
syntaxtest10 = TestCase (assertEqual "syntax11" (makeSyntax "ctg e") (UNIX (CTG, SIMPLE (exp 1))))
-- syntaxtest0 :: Test
-- syntaxtest0 = TestCase (assertEqual "syntax0" (makeSyntax "1") (SIMPLE 1))

syntaxes :: Test
syntaxes = TestList[
                    TestLabel "syntax0" syntaxtest0,
                    TestLabel "syntax1" syntaxtest1,
                    TestLabel "syntax2" syntaxtest2,
                    TestLabel "syntax3" syntaxtest3,
                    TestLabel "syntax4" syntaxtest4,
                    TestLabel "syntax5" syntaxtest5,
                    TestLabel "syntax6" syntaxtest6,
                    TestLabel "syntax7" syntaxtest7,
                    TestLabel "syntax8" syntaxtest8,
                    TestLabel "syntax9" syntaxtest9,
                    TestLabel "syntax10" syntaxtest10
                    ]

calculationtest1 :: Test
calculationtest1 = TestCase (assertEqual "calculation1+1" (calculate (makeSyntax "1+1")) (Integer 2))
calculationtest2 :: Test
calculationtest2 = TestCase (assertEqual "calculation1/2" (calculate (makeSyntax "1/2")) (Frac (1, 2)))
calculationtest3 :: Test
calculationtest3 = TestCase (assertEqual "calculation0.5" (calculate (makeSyntax "0.5")) (Creal 0.5))
calculationtest4 :: Test
calculationtest4 = TestCase (assertEqual "calculation1/3*3" (calculate (makeSyntax "1/3*3")) (Integer 1))
calculationtest5 :: Test
calculationtest5 = TestCase (assertEqual "calculation_sin0" (calculate (makeSyntax "sin 0")) (Integer 0))
calculationtest6 :: Test
calculationtest6 = TestCase (assertEqual "calculation_cos0" (calculate (makeSyntax "cos 0")) (Integer 1))
calculationtest7 :: Test
calculationtest7 = TestCase (assertEqual "calculation_cos0" (calculate (makeSyntax "sin pi")) (Integer 0))
calculationtest8 :: Test
calculationtest8 = TestCase (assertEqual "calculation_log_2" (calculate (makeSyntax "log_2 8")) (Integer 3))
calculationtest9 :: Test
calculationtest9 = TestCase (assertEqual "calculation_log_10" (calculate (makeSyntax "log 10000")) (Integer 4))
calculationtest10 :: Test
calculationtest10 = TestCase (assertEqual "calculation_ln" (calculate (makeSyntax "ln 8")) (Creal (log 8)))
calculationtest11 :: Test
calculationtest11 = TestCase (assertEqual "calculation_ln_e^7" (calculate (makeSyntax "ln (e^7)")) (Integer 7))
calculationtest12 :: Test
calculationtest12 = TestCase (assertEqual "calculation_raise1" (calculate (makeSyntax "2 ^ 3")) (Integer 8))
calculationtest13 :: Test
calculationtest13 = TestCase (assertEqual "calculation_raise2" (calculate (makeSyntax "256 ^ (1/4)")) (Integer 4))

calculations :: Test
calculations = TestList [
                     TestLabel "calculation1" calculationtest1,
                     TestLabel "calculation2" calculationtest2,
                     TestLabel "calculation3" calculationtest3,
                     TestLabel "calculation4" calculationtest4,
                     TestLabel "calculation5" calculationtest5,
                     TestLabel "calculation6" calculationtest6,
                     TestLabel "calculation7" calculationtest7,
                     TestLabel "calculation8" calculationtest8,
                     TestLabel "calculation9" calculationtest9,
                     TestLabel "calculation10" calculationtest10,
                     TestLabel "calculation11" calculationtest11,
                     TestLabel "calculation10" calculationtest12,
                     TestLabel "calculation11" calculationtest13
                     ]

simplifyingtest0 :: Test
simplifyingtest0 = TestCase (assertEqual "simplifying0" (simplifying (makeSyntax "x")) (VAR 'x'))
simplifyingtest0' :: Test
simplifyingtest0' = TestCase (assertEqual "simplifying0'" (simplifying (makeSyntax "1")) (SIMPLE 1))
simplifyingtest1 :: Test
simplifyingtest1 = TestCase (assertEqual "simplifying1" (simplifying (makeSyntax "2 + x")) (BINIX (SIMPLE 2, ADD, VAR 'x')))
simplifyingtest1' :: Test
simplifyingtest1' = TestCase (assertEqual "simplifying1'" (simplifying (makeSyntax "x + 2")) (BINIX (SIMPLE 2, ADD, VAR 'x')))
simplifyingtest2 :: Test
simplifyingtest2 = TestCase (assertEqual "simplifying2" (simplifying (makeSyntax "6x^2 + x + 2x^7")) (BINIX (VAR 'x', ADD, 
    BINIX (BINIX (SIMPLE 6, MUL, BINIX (VAR 'x', RAI, SIMPLE 2)), ADD, BINIX (SIMPLE 2, MUL, BINIX (VAR 'x', RAI, SIMPLE 7))))))
simplifyingtest2' :: Test
simplifyingtest2' = TestCase (assertEqual "simplifying2'" (simplifying (makeSyntax "2 + 3x + 9x^2")) (BINIX (SIMPLE 2, ADD, BINIX (BINIX (SIMPLE 3, MUL, VAR 'x'), ADD,
    BINIX (SIMPLE 9, MUL, BINIX (VAR 'x', RAI, SIMPLE 2))))))
simplifyingtest3 :: Test
simplifyingtest3 = TestCase (assertEqual "simplifying3" (simplifying (makeSyntax "sin x + 2")) (BINIX (SIMPLE 2, ADD, UNIX (SIN, VAR 'x'))))
simplifyingtest3' :: Test
simplifyingtest3' = TestCase (assertEqual "simplifying3'" (simplifying (makeSyntax "2 + sin x")) (BINIX (SIMPLE 2, ADD, UNIX (SIN, VAR 'x'))))

simplifyings :: Test
simplifyings = TestList [
                    TestLabel "simplifying0" simplifyingtest0,
                    TestLabel "simplifying0'" simplifyingtest0',
                    TestLabel "simplifying1" simplifyingtest1,
                    TestLabel "simplifying1'" simplifyingtest1',
                    TestLabel "simplifying2" simplifyingtest2,
                    TestLabel "simplifying2'" simplifyingtest2',
                    TestLabel "simplifying3" simplifyingtest3,
                    TestLabel "simplifying3'" simplifyingtest3'
                ]

tests :: Test
tests = TestList [
                    TestLabel "tokens" tokens,
                    TestLabel "syntaxes" syntaxes,
                    TestLabel "calculations" calculations,
                    TestLabel "simplifyings" simplifyings
                    ]
