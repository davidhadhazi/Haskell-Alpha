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
                  TestCase (assertEqual "1.2" (stringToTokens "1.2") [PURE 1, DOT, NUM (Creal 0.2)]),
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
                    TestCase (assertEqual "tan x" (makeSyntax "tan x") (UNIX (TAN, VAR 'x'))),
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
                    TestCase (assertEqual "2 + sin x = 2 + sin x" (simplifying (makeSyntax "2 + sin x")) (makeSyntax "2 + sin x")),
                    TestCase (assertEqual "x + x = 2x" (simplifying (makeSyntax "x + x")) (makeSyntax "2x")),
                    TestCase (assertEqual "2x + x = 3x" (simplifying (makeSyntax "2x + x")) (makeSyntax "3x")),
                    TestCase (assertEqual "2x + 3x = 5x" (simplifying (makeSyntax "2x + 3x")) (makeSyntax "5x")),
                    TestCase (assertEqual "x - x = 0" (simplifying (makeSyntax "x - x")) (makeSyntax "0")),
                    TestCase (assertEqual "x^2 + 2*x^2 = 3 * x^2" (simplifying (makeSyntax "x^2 + 2*x^2")) (makeSyntax "3 * x^2")),
                    TestCase (assertEqual "5*ln x + ln x = 6 *ln x" (simplifying (makeSyntax "5*ln x + ln x")) (makeSyntax "6 * ln x")),
                    TestCase (assertEqual "-x-x = -2 * x" (simplifying (makeSyntax "-x-x")) (simplifying (makeSyntax "(-2) * x"))),
                    TestCase (assertEqual "sin (x + x) = sin (2x)" (simplifying (makeSyntax "sin (x + x)")) (makeSyntax "sin (2x)")),
                    TestCase (assertEqual "1 + x + x = 1 + 2x" (simplifying (makeSyntax "1 + x + x")) (makeSyntax "1 + 2x")),
                    TestCase (assertEqual "x + x + sin x = 2x + sinx" (simplifying (makeSyntax "x + x + sin x")) (makeSyntax "2x + sin x")),
                    TestCase (assertEqual "x + sin x + x = 2x + sinx" (simplifying (makeSyntax "x + sin x + x")) (makeSyntax "2x + sin x")),
                    TestCase (assertEqual "sin x + x + x = 2x + sinx" (simplifying (makeSyntax "sin x + x + x")) (makeSyntax "2x + sin x")),
                    TestCase (assertEqual "sin x + x + 2x = 2x + sinx" (simplifying (makeSyntax "sin x + x + 2x")) (makeSyntax "3x + sin x")),
                    TestCase (assertEqual "11x + cos x + 9x = 20x + cos x" (simplifying (makeSyntax "11x + cos x + 9x")) (makeSyntax "20x + cos x")),
                    TestCase (assertEqual "11x + 5 * cos x + 9x = 20x + cos x" (simplifying (makeSyntax "11x + 5 * cos x + 9x")) (makeSyntax "20x + 5 *cos x")),
                    TestCase (assertEqual "6x - 5x = x" (simplifying (makeSyntax "6x - 5x")) (makeSyntax "x")),
                    TestCase (assertEqual "2 * x * x = 2 * x^2" (simplifying (makeSyntax "2 * x * x")) (makeSyntax "2 * x^2")),
                    TestCase (assertEqual "log x * x * 2x = 2 * x^2 * log x" (simplifying (makeSyntax "log x * x * 2x")) (makeSyntax "2 * (log x * x^2)")),
                    TestCase (assertEqual "log x + 6x - 5x = x" (simplifying (makeSyntax "log x + 6x - 5x")) (makeSyntax "x + log x")),
                    TestCase (assertEqual "6x + 3 - 5x = x" (simplifying (makeSyntax "6x + 3 - 5x")) (makeSyntax "3 + x")),
                    TestCase (assertEqual "log x + 5x - 6x = x" (simplifying (makeSyntax "log x + 5x - 6x")) (makeSyntax "-x + log x")),
                    TestCase (assertEqual "-6x + 3 + 5x = x" (simplifying (makeSyntax "-6x + 3 + 5x")) (makeSyntax "3 - x")),
                    TestCase (assertEqual "sin x^2 + cos x^2 = 1" (simplifying (makeSyntax "sin x^2 + cos x^2")) (makeSyntax "1")),
                    TestCase (assertEqual "2 * sin x^2 + cos x^2 = 1 + sin x^2" (simplifying (makeSyntax "2 * sin x^2 + cos x^2")) (makeSyntax "1 + sin x^2")),
                    TestCase (assertEqual "sin x^2 + 4 * cos x^2 = 1 + 3 * cos x^2" (simplifying (makeSyntax "sin x^2 + 4 * cos x^2")) (makeSyntax "1 + 3 * cos x^2")),
                    TestCase (assertEqual "4 * cos x^2 + 4 * sin x^2 = 4" (simplifying (makeSyntax "4 * cos x^2 + 4 * sin x^2")) (makeSyntax "4"))
                ]

derivates :: Test
derivates = TestList [
                    TestCase (assertEqual "0' = 0" (simplifying (derivate (makeSyntax "0"))) (makeSyntax "0")),
                    TestCase (assertEqual "x' = 1" (simplifying (derivate (makeSyntax "x"))) (makeSyntax "1")),
                    TestCase (assertEqual "16x' = 16" (simplifying (derivate (makeSyntax "16x"))) (makeSyntax "16")),
                    TestCase (assertEqual "x^2' = 2x" (simplifying (derivate (makeSyntax "x^2"))) (makeSyntax "2x")),
                    TestCase (assertEqual "(x+1)^2' = 2 + 2x" (simplifying (derivate (makeSyntax "(x+1)^2"))) (makeSyntax "2 + 2x")),
                    TestCase (assertEqual "(x+1)^7' = 7 * (1 + x)^6" (simplifying (derivate (makeSyntax "(x+1)^7"))) (makeSyntax "7 * (1 + x)^6")),
                    TestCase (assertEqual "(x-1)^7' = 7 * (-1 + x)^6" (simplifying (derivate (makeSyntax "(x-1)^7"))) (simplifying (makeSyntax "7 * (-1 + x)^6"))),
                    TestCase (assertEqual "1/x' = -1/x^2" (simplifying (derivate (makeSyntax "1/x"))) (simplifying (makeSyntax "(-1)/x^2"))),
                    TestCase (assertEqual "-1/x' = 1/x^2" (simplifying (derivate (makeSyntax "-1/x"))) (makeSyntax "1/x^2")),
                    TestCase (assertEqual "5/x^2' = -10/x^3" (simplifying (derivate (makeSyntax "5/x^2"))) (simplifying (makeSyntax "-10/x^3"))),
                    TestCase (assertEqual "-4/x^4' = 16/x^5" (simplifying (derivate (makeSyntax "-4/x^4"))) (makeSyntax "16/x^5")),
                    TestCase (assertEqual "sin x' = cos x" (simplifying (derivate (makeSyntax "sin x"))) (makeSyntax "cos x")),
                    TestCase (assertEqual "cos x' = -sin x" (simplifying (derivate (makeSyntax "cos x"))) (makeSyntax "-sin x")),
                    TestCase (assertEqual "sin x + 1' = cos x" (simplifying (derivate (makeSyntax "sin x + 1"))) (makeSyntax "cos x")),
                    TestCase (assertEqual "tan x' = 1 / (cos x)^2" (simplifying (derivate (makeSyntax "tan x"))) (makeSyntax "1 / (cos x)^2")),
                    TestCase (assertEqual "ctg x' = -1 / (sin x)^2" (simplifying (derivate (makeSyntax "ctg x"))) (simplifying (makeSyntax "-1 / (sin x)^2"))),
                    TestCase (assertEqual "e^x' = e^x" (simplifying (derivate (makeSyntax "e^x"))) (makeSyntax "e^x")),
                    TestCase (assertEqual "6^x' = ln 6 * 6^x" (simplifying (derivate (makeSyntax "6^x"))) (simplifying (makeSyntax "ln 6 * 6^x"))),

                    TestCase (assertEqual "x^x' = x^x * (1 + ln x)" (simplifying (derivate (makeSyntax "x^x"))) (makeSyntax "x^x * (1 + ln x)"))
                    ]

indefinite_integrates :: Test
indefinite_integrates = TestList [
                                TestCase (assertEqual "integrate_0" (simplifying (integrate (makeSyntax "0"))) (makeSyntax "0")),
                                TestCase (assertEqual "integrate_1" (simplifying (integrate (makeSyntax "1"))) (makeSyntax "x")),
                                TestCase (assertEqual "integrate_5" (simplifying (integrate (makeSyntax "5"))) (makeSyntax "5x")),
                                TestCase (assertEqual "integrate_-4" (simplifying (integrate (makeSyntax "-4"))) (simplifying (makeSyntax "-4x"))),
                                TestCase (assertEqual "integrate_x" (simplifying (integrate (makeSyntax "x"))) (simplifying (makeSyntax "1/2 * x^2"))),
                                TestCase (assertEqual "integrate_2x" (simplifying (integrate (makeSyntax "2x"))) (makeSyntax "x^2")),
                                TestCase (assertEqual "integrate_5x" (simplifying (integrate (makeSyntax "5x"))) (simplifying (makeSyntax "5/2 * x^2"))),
                                TestCase (assertEqual "integrate_-7x" (simplifying (integrate (makeSyntax "-7x"))) (simplifying (makeSyntax "-7/2 * x^2"))),
                                TestCase (assertEqual "integrate_x^2" (simplifying (integrate (makeSyntax "x^2"))) (simplifying (makeSyntax "1/3 * x^3"))),
                                TestCase (assertEqual "integrate_3x^2" (simplifying (integrate (makeSyntax "3x^2"))) (makeSyntax "x^3")),
                                TestCase (assertEqual "integrate_3x^8" (simplifying (integrate (makeSyntax "3x^8"))) (simplifying (makeSyntax "1/3 * x^9"))),
                                TestCase (assertEqual "integrate_5x^4+2x+1" (simplifying (integrate (makeSyntax "5x^4+2x+1"))) (makeSyntax "x+(x^2+x^5)")),
                                TestCase (assertEqual "integrate_5x^4-2x+1" (simplifying (integrate (makeSyntax "5x^4-2x+1"))) (makeSyntax "x-x^2+x^5")),
                                TestCase (assertEqual "integrate_5x^4-2x+1" (simplifying (integrate (makeSyntax "-5x^4-2x+1"))) (makeSyntax "x-x^2-x^5")),
                                TestCase (assertEqual "integrate_5x^4-2x+1" (simplifying (integrate (makeSyntax "-5x^4-2x-1"))) (simplifying (makeSyntax "-x-x^2-x^5"))),
                                TestCase (assertEqual "integrate_e^x" (simplifying (integrate (makeSyntax "e^x"))) (makeSyntax "e^x")),
                                TestCase (assertEqual "integrate_1/x" (simplifying (integrate (makeSyntax "1/x"))) (makeSyntax "ln x")),
                                TestCase (assertEqual "integrate_1/x^2" (simplifying (integrate (makeSyntax "1/x^2"))) (simplifying (makeSyntax "(-1)/x"))),
                                TestCase (assertEqual "integrate_8/x^5" (simplifying (integrate (makeSyntax "8/x^5"))) (simplifying (makeSyntax "(-2)/x^4"))),
                                TestCase (assertEqual "integrate_sinx" (simplifying (integrate (makeSyntax "sin x"))) (simplifying (makeSyntax "-cos x"))),
                                TestCase (assertEqual "integrate_cosx" (simplifying (integrate (makeSyntax "cos x"))) (simplifying (makeSyntax "sin x"))),
                                TestCase (assertEqual "integrate_tanx" (simplifying (integrate (simplifying (makeSyntax "tan x")))) (simplifying (makeSyntax "-ln(cos x)"))),
                                TestCase (assertEqual "integrate_ctgx" (simplifying (integrate (simplifying (makeSyntax "ctg x")))) (simplifying (makeSyntax "ln(sin x)"))),
                                TestCase (assertEqual "integrate_x sin(x^2)" (simplifying (integrate (simplifying (makeSyntax "x * sin(x^2)")))) (simplifying (makeSyntax "(-1/2) * cos(x^2)"))),
                                TestCase (assertEqual "integrate_2x+3 sin(x^2+3x+4)" (simplifying (integrate (simplifying (makeSyntax "(2x + 3) * sin(x^2 + 3x + 4)")))) (simplifying (makeSyntax "-cos(x^2 + 3x + 4)"))),
                                TestCase (assertEqual "integrate_x cos(x^2)" (simplifying (integrate (simplifying (makeSyntax "x * cos(x^2)")))) (simplifying (makeSyntax "(1/2) * sin(x^2)"))),
                                TestCase (assertEqual "integrate_(5x^4 + 2x + 3) cos(x^5 + x^2 + 3x -8)" (simplifying (integrate (simplifying (makeSyntax "(5x^4 + 2x + 3) * cos(x^5 + x^2 + 3x -8)")))) (simplifying (makeSyntax "sin (x^5 + x^2 + 3x - 8)"))),
                                TestCase (assertEqual "integrate_x tan (x^2)" (simplifying (integrate (simplifying (makeSyntax "x*tan(x^2)")))) (simplifying (makeSyntax "(-1/2) * ln(cos(x^2))"))),
                                TestCase (assertEqual "integrate_6x + 3 tan (x^2 + x)" (simplifying (integrate (simplifying (makeSyntax "(6x + 3)*tan(x^2 + x)")))) (simplifying (makeSyntax "(-3) * ln(cos(x^2 + x))"))),
                                TestCase (assertEqual "integrate_x ctg(x^2)" (simplifying (integrate (simplifying (makeSyntax "x * ctg(x^2)")))) (simplifying (makeSyntax "(1/2) * ln(sin(x^2))"))),
                                TestCase (assertEqual "integrate_x^3+x ctg(x^4 + 2x^2 + 4)" (simplifying (integrate (simplifying (makeSyntax "(x^3 + x) * ctg(x^4 + 2 * x^2 + 4)")))) (simplifying (makeSyntax "(1/4) * ln(sin(x^4 + 2*x^2 + 4))"))),
                                TestCase (assertEqual "integrate_sinx * cosx" (simplifying (integrate (simplifying (makeSyntax "sin x * cos x")))) (simplifying (makeSyntax "-(cos x)^2 / 2"))),
                                TestCase (assertEqual "integrate_cosx * sinx" (simplifying (integrate (simplifying (makeSyntax "cos x * sin x")))) (simplifying (makeSyntax "-(cos x)^2 / 2"))),
                                TestCase (assertEqual "integrate_xsinx" (simplifying (integrate (makeSyntax "x * sin x"))) (simplifying (makeSyntax "sin x - x * cos x"))),
                                TestCase (assertEqual "integrate_sinx * x" (simplifying (integrate (simplifying (makeSyntax "sin x * x")))) (simplifying (makeSyntax "sin x - x * cos x"))),
                                TestCase (assertEqual "integrate_xcosx" (simplifying (integrate (makeSyntax "x * cos x"))) (simplifying (makeSyntax "x * sin x + cos x"))),
                                TestCase (assertEqual "integrate_cosx * x" (simplifying (integrate (simplifying (makeSyntax "cos x * x")))) (simplifying (makeSyntax "cos x + x * sin x"))),

                                TestCase (assertEqual "integrate_x^x * (1 + ln x)" (simplifying (integrate (simplifying (makeSyntax "x^x * (1 + ln x)")))) (makeSyntax "x^x"))
                                ]

definite_integrates :: Test
definite_integrates = TestList[
                        TestCase (assertEqual "1_0_1 = 1" (definite_integrate (makeSyntax "1") 0 1) 1),
                        TestCase (assertEqual "1_1_0 = -1" (definite_integrate (makeSyntax "1") 1 0) (-1)),
                        TestCase (assertEqual "x_-1_1 = 0" (definite_integrate (makeSyntax "x") (-1) 1) 0),
                        TestCase (assertEqual "2x_-100_100" (definite_integrate (makeSyntax "2x") (-100) 100) 0),
                        TestCase (assertEqual "sin x_-2_2" (definite_integrate (makeSyntax "sin x") (-2) 2) 0),
                        TestCase (assertEqual "sin x_0_pi" (definite_integrate (makeSyntax "sin x") 0 pi) 2)
                    ]

tests :: Test
tests = TestList [
                    TestLabel "tokens" tokens,
                    TestLabel "syntaxes" syntaxes,
                    TestLabel "calculations" calculations,
                    TestLabel "simplifyings" simplifyings,
                    TestLabel "derivates" derivates,
                    TestLabel "indefinite_integrates" indefinite_integrates,
                    TestLabel "definite_integrates" definite_integrates
                    ]
