module Main where

import Data.List (sort)
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

-- some specific examples and regression tests
        , testGroup "Solve specific examples" [
            testProperty "1 * u^4 + -240 * u^3 + 25449 * u^2 + -1325880 * u + 26471900.25 = 0" $
                let [r1, r2] = sort $ quartForm 1 (-240) 25449 (-1325880) 26471900.25 in
                r1 =~ 50.6451 && r2 =~ 69.3549
            ]
        ]

(=~) :: Double -> Double -> Bool
(=~) a b = abs (a - b) < 0.001
infix 4 =~

main :: IO ()
main = defaultMain tests
