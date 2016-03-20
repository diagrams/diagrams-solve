module Main where

import Diagrams.Solve.Polynomial

import Test.Tasty (defaultMain, testGroup, TestTree)
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Solve" [
         testProperty "solutions found satisfy quadratic equation" $
         \a b c -> let sat x =  a * x * x + b * x + c =~ 0 in all sat (quadForm a b c)
-- could verify number of solutions, but we would just duplicate the function definition
        , testProperty "solutions found satisfy cubic equation" $
         \a b c d -> let sat x =  a * x * x * x + b * x * x + c * x + d =~ (0 :: Double) in all sat (cubForm a b c d)
        ]

(=~) :: Double -> Double -> Bool
(=~) a b = abs (a - b) < 0.001
infix 4 =~

main :: IO ()
main = defaultMain tests
