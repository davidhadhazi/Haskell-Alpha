module TestCases ( tests ) where

import Test.HUnit
import Tokens
import SyntaxTree
import Number
import Simplification
import Derivate
import Integrate

tokens :: Test
tokens = TestList[
                  TestCase (assertEqual "empty" (stringToTokens "") []),
                  TestCase (assertEqual "1 1" (stringToTokens "1 1") [PURE 1, PURE 1]),
                  TestCase (assertEqual "1              1 = 1 1" (stringToTokens "1                1") [PURE 1, PURE 1]), 
                  TestCase (assertEqual "123" (stringToTokens "123") [PURE 123]),
                  TestCase (assertEqual "1.2" (stringToTokens "1.2") [PURE 1, DOT, PURE 2]),
                  TestCase (assertEqual "pi" (stringToTokens "pi") [PI]),
                  TestCase (assertEqual "e" (stringToTokens "e") [E]),
                  TestCase (assertEqual "+" (stringToTokens "+") [ADD]),
                  TestCase (assertEqual "-" (stringToTokens "-") [MIN]),
                  TestCase (assertEqual "*" (stringToTokens "*") [MUL]),
                  TestCase (assertEqual "/" (stringToTokens "/") [DIV]),
                  TestCase (assertEqual "^" (stringToTokens "^") [RAI]),
                  TestCase (assertEqual "log" (stringToTokens "log_") [LOG]),
                  TestCase (assertEqual "log_" (stringToTokens "log") [LOG10]),
                  TestCase (assertEqual "ln" (stringToTokens "ln") [LN]),
                  TestCase (assertEqual "sin" (stringToTokens "sin") [SIN]),
                  TestCase (assertEqual "cos" (stringToTokens "cos") [COS]),
                  TestCase (assertEqual "tan" (stringToTokens "tan") [TAN]),
                  TestCase (assertEqual "ctg" (stringToTokens "ctg") [CTG]),
                  TestCase (assertEqual "(" (stringToTokens "(") [OP]),
                  TestCase (assertEqual ")" (stringToTokens ")") [CL])
                  ]

syntaxes :: Test
syntaxes = TestList[
                    TestCase (assertEqual "1" (makeSyntax "1") (SIMPLE 1)),
                    TestCase (assertEqual "x" (makeSyntax "x") (VAR 'x')),
                    TestCase (assertEqual "1 + 1" (makeSyntax "1 + 1") (BINIX (SIMPLE 1, ADD, SIMPLE 1))),
                    TestCase (assertEqual "sin pi" (makeSyntax "sin pi") (UNIX (SIN, SIMPLE (Creal pi)))),
                    TestCase (assertEqual "2 * x" (makeSyntax "2x") (BINIX (SIMPLE 2, MUL, VAR 'x'))),
                    TestCase (assertEqual "1 + 2 + 3" (makeSyntax "1 + 2 + 3") (BINIX (BINIX (SIMPLE 1, ADD, SIMPLE 2), ADD, SIMPLE 3))),
                    TestCase (assertEqual "1 + (2 + 3)" (makeSyntax "1 + (2 + 3)") (BINIX (SIMPLE 1, ADD, BINIX (SIMPLE 2, ADD, SIMPLE 3)))),
                    TestCase (assertEqual "cos (x^2)" (makeSyntax "cos (x^2)") (UNIX (COS, BINIX (VAR 'x', RAI, SIMPLE 2)))),
                    TestCase (assertEqual "log_2 4 + ln 3 * log 5" (makeSyntax "log_2 4 + ln 3 * log 5") (BINIX (BINIX (SIMPLE 2, LOG, SIMPLE 4), ADD, BINIX (UNIX (LN, SIMPLE 3), MUL, UNIX (LOG10, SIMPLE 5))))),
                    TestCase (assertEqual "tan x y" (makeSyntax "tan x y") (BINIX (UNIX (TAN, VAR 'x'), MUL, VAR 'y'))),
                    TestCase (assertEqual "ctg e" (makeSyntax "ctg e") (UNIX (CTG, SIMPLE (exp 1))))
                    ]

calculations :: Test
calculations = TestList [
                     TestCase (assertEqual "calculation1+1" (calculate (makeSyntax "1+1")) (Integer 2)),
                     TestCase (assertEqual "calculation1/2" (calculate (makeSyntax "1/2")) (Frac (1, 2))),
                     TestCase (assertEqual "calculation0.5" (calculate (makeSyntax "0.5")) (Creal 0.5)),
                     TestCase (assertEqual "calculation1/3*3" (calculate (makeSyntax "1/3*3")) (Integer 1)),
                     TestCase (assertEqual "calculation_sin0" (calculate (makeSyntax "sin 0")) (Integer 0)),
                     TestCase (assertEqual "calculation_cos0" (calculate (makeSyntax "cos 0")) (Integer 1)),
                     TestCase (assertEqual "calculation_cos0" (calculate (makeSyntax "sin pi")) (Integer 0)),
                     TestCase (assertEqual "calculation_log_2" (calculate (makeSyntax "log_2 8")) (Integer 3)),
                     TestCase (assertEqual "calculation_log_10" (calculate (makeSyntax "log 10000")) (Integer 4)),
                     TestCase (assertEqual "calculation_ln" (calculate (makeSyntax "ln 8")) (Creal (log 8))),
                     TestCase (assertEqual "calculation_ln_e^7" (calculate (makeSyntax "ln (e^7)")) (Integer 7)),
                     TestCase (assertEqual "calculation_raise1" (calculate (makeSyntax "2 ^ 3")) (Integer 8)),
                     TestCase (assertEqual "calculation_raise2" (calculate (makeSyntax "256 ^ (1/4)")) (Integer 4))
                     ]

simplifyings :: Test
simplifyings = TestList [
                    TestCase (assertEqual "x = x" (simplifying (makeSyntax "x")) (VAR 'x')),
                    TestCase (assertEqual "1 = 1" (simplifying (makeSyntax "1")) (SIMPLE 1)),
                    TestCase (assertEqual "2 + x = 2 + x" (simplifying (makeSyntax "2 + x")) (makeSyntax "2 + x")),
                    TestCase (assertEqual "x + 2 = 2 + x" (simplifying (makeSyntax "x + 2")) (makeSyntax "2 + x")),
                    TestCase (assertEqual "2 - x = 2 - x" (simplifying (makeSyntax "2 - x")) (makeSyntax "2 - x")),
                    TestCase (assertEqual "6x^2 + x + 2x^7 = x + (6 * x^2 + 2 * x^7)" (simplifying (makeSyntax "6x^2 + x + 2x^7")) (makeSyntax "x + (6 * x^2 + 2 * x^7)")),
                    TestCase (assertEqual "2 + 3x + 9x^2 = 2 + (3 * x + 9 * x^2)" (simplifying (makeSyntax "2 + 3x + 9x^2")) (makeSyntax "2 + (3 * x + 9 * x^2)")),
                    TestCase (assertEqual "sin x + 2 = 2 + sin x" (simplifying (makeSyntax "sin x + 2")) (makeSyntax "2 + sin x")),
                    TestCase (assertEqual "2 + sin x = 2 + sin x" (simplifying (makeSyntax "2 + sin x")) (makeSyntax "2 + sin x"))
                ]

derivates :: Test
derivates = TestList [
                    TestCase (assertEqual "0' = 0" (simplifying (derivate (makeSyntax "0"))) (makeSyntax "0")),
                    TestCase (assertEqual "x' = 1" (simplifying (derivate (makeSyntax "x"))) (makeSyntax "1")),
                    TestCase (assertEqual "16x' = 16" (simplifying (derivate (makeSyntax "16x"))) (makeSyntax "16")),
                    TestCase (assertEqual "x^2' = 2x" (simplifying (derivate (makeSyntax "x^2"))) (makeSyntax "2x")),
                    TestCase (assertEqual "(x+1)^2' = 2 + 2x" (simplifying (derivate (makeSyntax "(x+1)^2"))) (makeSyntax "2 + 2x")),
                    TestCase (assertEqual "(x+1)^7' = 7 * (1 + x)^6" (simplifying (derivate (makeSyntax "(x+1)^7"))) (makeSyntax "7 * (1 + x)^6")),
                    TestCase (assertEqual "sin x' = cos x" (simplifying (derivate (makeSyntax "sin x"))) (makeSyntax "cos x")),
                    TestCase (assertEqual "cos x' = -sin x" (simplifying (derivate (makeSyntax "cos x"))) (makeSyntax "-sin x")),
                    TestCase (assertEqual "sin x + 1' = cos x" (simplifying (derivate (makeSyntax "sin x + 1"))) (makeSyntax "cos x")),
                    TestCase (assertEqual "tan x' = 1 / (cos x)^2" (simplifying (derivate (makeSyntax "tan x"))) (makeSyntax "1 / (cos x)^2")),
                    TestCase (assertEqual "ctg x' = 1 / (sin x)^2" (simplifying (derivate (makeSyntax "ctg x"))) (makeSyntax "1 / (sin x)^2")),
                    TestCase (assertEqual "e^x' = e^x" (simplifying (derivate (makeSyntax "e^x"))) (makeSyntax "e^x")),
                    TestCase (assertEqual "6^x' = ln 6 * 6^x" (simplifying (derivate (makeSyntax "6^x"))) (simplifying (makeSyntax "ln 6 * 6^x")))
                    ]

indefinite_integrates :: Test
indefinite_integrates = TestList [
                                TestCase (assertEqual "integrate_0" (simplifying (integrate (makeSyntax "0"))) (makeSyntax "0")),
                                TestCase (assertEqual "integrate_1" (simplifying (integrate (makeSyntax "1"))) (makeSyntax "x")),
                                TestCase (assertEqual "integrate_5" (simplifying (integrate (makeSyntax "5"))) (makeSyntax "5x")),
                                TestCase (assertEqual "integrate_-4" (simplifying (integrate (makeSyntax "-4"))) (makeSyntax "-4x")),
                                TestCase (assertEqual "integrate_x" (simplifying (integrate (makeSyntax "x"))) (simplifying (makeSyntax "1/2 * x^2"))),
                                TestCase (assertEqual "integrate_2x" (simplifying (integrate (makeSyntax "2x"))) (makeSyntax "x^2")),
                                TestCase (assertEqual "integrate_5x" (simplifying (integrate (makeSyntax "5x"))) (simplifying (makeSyntax "5/2 * x^2"))),
                                TestCase (assertEqual "integrate_-7x" (simplifying (integrate (makeSyntax "-7x"))) (simplifying (makeSyntax "-7/2 * x^2"))),
                                TestCase (assertEqual "integrate_x^2" (simplifying (integrate (makeSyntax "x^2"))) (simplifying (makeSyntax "1/3 * x^3"))),
                                TestCase (assertEqual "integrate_3x^2" (simplifying (integrate (makeSyntax "3x^2"))) (makeSyntax "x^3")),
                                TestCase (assertEqual "integrate_3x^8" (simplifying (integrate (makeSyntax "3x^8"))) (simplifying (makeSyntax "1/3 * x^9"))),
                                TestCase (assertEqual "integrate_5x^4+2x+1" (simplifying (integrate (makeSyntax "5x^4+2x+1"))) (makeSyntax "x+(x^2+x^5)")),
                                TestCase (assertEqual "integrate_5x^4-2x+1" (simplifying (integrate (makeSyntax "5x^4-2x+1"))) (makeSyntax "x-x^2+x^5"))
                                ]

tests :: Test
tests = TestList [
                    TestLabel "tokens" tokens,
                    TestLabel "syntaxes" syntaxes,
                    TestLabel "calculations" calculations,
                    TestLabel "simplifyings" simplifyings
                    -- TestLabel "derivates" derivates,
                    -- TestLabel "indefinite_integrates" indefinite_integrates
                    ]
