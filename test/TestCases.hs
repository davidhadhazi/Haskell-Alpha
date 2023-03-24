module TestCases ( tests ) where

import Test.HUnit
import Tokens
import SyntaxTree
import Number

tokentest0 :: Test
tokentest0 = TestCase (assertEqual "tokenNumber0" (stringToTokens "") [])
tokentest1 :: Test
tokentest1 = TestCase (assertEqual "tokenNumber1" (stringToTokens "1 1") [PURE 1, PURE 1])
tokentest2 :: Test
tokentest2 = TestCase (assertEqual "tokenNumber2" (stringToTokens "1                1") [PURE 1, PURE 1])
tokentest3 :: Test
tokentest3 = TestCase (assertEqual "tokenNumber3" (stringToTokens "123") [PURE 123])
tokentest4::Test
tokentest4 = TestCase (assertEqual "tokenNumber4" (fillingUp (stringToTokens "1.2")) [NUM 1.2])

tokens :: Test
tokens = TestList[
                  TestLabel "token0" tokentest0,
                  TestLabel "token1" tokentest1,
                  TestLabel "token2" tokentest2, 
                  TestLabel "token3" tokentest3,
                  TestLabel "token4" tokentest4
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

tests :: Test
tests = TestList [TestLabel "tokens" tokens, TestLabel "calculations" calculations]
