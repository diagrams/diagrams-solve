module Main where

import           Diagrams.Solve.Polynomial

import           Test.Tasty                (TestTree, defaultMain, localOption,
                                            testGroup)
import           Test.Tasty.QuickCheck

tests :: TestTree
tests
  = localOption (QuickCheckTests 1000) $
    testGroup "Solve"
  [ testProperty "solutions found satisfy quadratic equation" $
    \a b c -> all (\x -> evalPoly [c,b,a] x =~ 0) (quadForm a b c)

  , testProperty "solutions found satisfy cubic equation" $
    \a b c d -> all (\x -> evalPoly [d,c,b,a] x =~ 0) (cubForm a b c d)

  , testProperty "solutions found satisfy quartic equation" $
    \a b c d e ->
      let sat x = a*x^4 + b*x^3 + c*x^2 + d*x + e =~ (0 :: Double)
      in  all sat (quartForm a b c d e)
  ]

(=~) :: Double -> Double -> Bool
(=~) a b = abs (a - b) < 1e-5
infix 4 =~

main :: IO ()
main = defaultMain tests
