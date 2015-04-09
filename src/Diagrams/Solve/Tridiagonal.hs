{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Solve.Tridiagonal
-- Copyright   :  (c) 2011-2015 diagrams-solve team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Solving of tridiagonal and cyclic tridiagonal linear systems.
--
-----------------------------------------------------------------------------
module Diagrams.Solve.Tridiagonal
       ( solveTriDiagonal
       , solveCyclicTriDiagonal
       ) where

-- | @solveTriDiagonal as bs cs ds@ solves a system of the form @A*X = ds@
--   where 'A' is an 'n' by 'n' matrix with 'bs' as the main diagonal
--   and 'as' the diagonal below and 'cs' the diagonal above.  See:
--   <http://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm>

solveTriDiagonal :: Fractional a => [a] -> [a] -> [a] -> [a] -> [a]
solveTriDiagonal as (b0:bs) (c0:cs) (d0:ds) = h cs' ds'
  where
    cs' = c0 / b0 : f cs' as bs cs
    f _ [_] _ _ = []
    f (c':cs') (a:as) (b:bs) (c:cs) = c / (b - c' * a) : f cs' as bs cs
    f _ _ _ _ = error "solveTriDiagonal.f: impossible!"

    ds' = d0 / b0 : g ds' as bs cs' ds
    g _ [] _ _ _ = []
    g (d':ds') (a:as) (b:bs) (c':cs') (d:ds) = (d - d' * a)/(b - c' * a) : g ds' as bs cs' ds
    g _ _ _ _ _ = error "solveTriDiagonal.g: impossible!"

    h _ [d] = [d]
    h (c:cs) (d:ds) = let xs@(x:_) = h cs ds in d - c * x : xs
    h _ _ = error "solveTriDiagonal.h: impossible!"

solveTriDiagonal _ _ _ _ = error "arguments 2,3,4 to solveTriDiagonal must be nonempty"

-- Helper that applies the passed function only to the last element of a list
modifyLast :: (a -> a) -> [a] -> [a]
modifyLast _ []     = []
modifyLast f [a]    = [f a]
modifyLast f (a:as) = a : modifyLast f as

-- Helper that builds a list of length n of the form: '[s,m,m,...,m,m,e]'
sparseVector :: Int -> a -> a -> a -> [a]
sparseVector n s m e
    | n < 1     = []
    | otherwise = s : h (n - 1)
  where
    h 1 = [e]
    h n = m : h (n - 1)

-- | Solves a system similar to the tri-diagonal system using a special case
--   of the Sherman-Morrison formula (<http://en.wikipedia.org/wiki/Sherman-Morrison_formula>).
--   This code is based on /Numerical Recpies in C/'s @cyclic@ function in section 2.7.
solveCyclicTriDiagonal :: Fractional a => [a] -> [a] -> [a] -> [a] -> a -> a -> [a]
solveCyclicTriDiagonal as (b0:bs) cs ds alpha beta = zipWith ((+) . (fact *)) zs xs
  where
    l = length ds
    gamma = -b0
    us = sparseVector l gamma 0 alpha

    bs' = (b0 - gamma) : modifyLast (subtract (alpha*beta/gamma)) bs

    xs@(x:_) = solveTriDiagonal as bs' cs ds
    zs@(z:_) = solveTriDiagonal as bs' cs us

    fact = -(x + beta * last xs / gamma) / (1.0 + z + beta * last zs / gamma)

solveCyclicTriDiagonal _ _ _ _ _ _ = error "second argument to solveCyclicTriDiagonal must be nonempty"
