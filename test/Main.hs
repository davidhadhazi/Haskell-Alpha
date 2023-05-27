module Main (main) where

import TestCases
import Test.HUnit

-- Executing the testcases
main :: IO ()
main = runTestTTAndExit tests