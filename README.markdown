[![Build Status](https://travis-ci.org/diagrams/diagrams-solve.png?branch=master)](https://travis-ci.org/diagrams/diagrams-solve)

Miscellaneous pure-Haskell solver routines used in
[diagrams](http://projects.haskell.org/diagrams/), a Haskell embedded
domain-specific language for compositional, declarative drawing.

This is split out into a separate package with no dependencies on the
rest of diagrams in case it is useful to others, but no particular
guarantees are made as to the suitability or correctness of the code
(though we are certainly open to bug reports).

Currently the package contains:

  - functions to find real roots of quadratic, cubic, and quartic
    polynomials, in `Diagrams.Solve.Polynomial`

  - functions to solve tridiagonal and cyclic tridiagonal systems of
    linear equations, in `Diagrams.Solve.Tridiagonal`
