module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

suite :: TestTree
suite = testGroup "Test Suite" []

main :: IO ()
main = defaultMain suite
