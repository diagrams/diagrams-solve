module Main where

import Data.List (sort)
import Diagrams.Solve.Polynomial

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Solve"
    [ testProperty "solutions found satisfy quadratic equation" $
        \a b c -> let sat x = a * x * x + b * x + c =~ (0 :: Double) in all sat (quadForm a b c)
    , -- could verify number of solutions, but we would just duplicate the function definition
      testProperty "solutions found satisfy cubic equation" $
        \a b c d -> let sat x = a * x * x * x + b * x * x + c * x + d =~ (0 :: Double) in all sat (cubForm a b c d)
    , -- some specific examples and regression tests
      testGroup
        "Solve specific examples"
        [ testProperty "1 * x^3 + -886.7970773009183 * x^2 + 262148.4783430062 * x + -264000817.775054 = 0" $
            let rs = cubForm 1 (-886.7970773009183) 262148.4783430062 (-264000817.775054)
             in rs =~ [915.4538593912 :: Double]
        , testProperty "1 * u^4 + -240 * u^3 + 25449 * u^2 + -1325880 * u + 26471900.25 = 0" $
            let rs = sort $ quartForm 1 (-240) 25449 (-1325880) 26471900.25
             in rs =~ [50.6451, 69.3549 :: Double]
        ]
    ]

class Eqish a where
  (=~) :: a -> a -> Bool
infix 4 =~

instance Eqish Double where
  (=~) a b = abs (a - b) < 0.001

instance Eqish a => Eqish [a] where
  [] =~ [] = True
  (a : as) =~ (b : bs) = a =~ b && as =~ bs
  _ =~ _ = False

main :: IO ()
main = defaultMain tests
