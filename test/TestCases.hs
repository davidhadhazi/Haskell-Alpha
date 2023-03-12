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

syntaxtest1 :: Test
syntaxtest1 = TestCase (assertEqual "syntax1+1" (calculate (makeSyntax "1+1")) (Integer 2))
syntaxtest2 :: Test
syntaxtest2 = TestCase (assertEqual "syntax1/2" (calculate (makeSyntax "1/2")) (Frac (1, 2)))

syntaxes :: Test
syntaxes = TestList [
                     TestLabel "syntax1" syntaxtest1,
                     TestLabel "syntax2" syntaxtest2
                     ]

tests :: Test
tests = TestList [TestLabel "tokens" tokens, TestLabel "Syntaxes" syntaxes]
