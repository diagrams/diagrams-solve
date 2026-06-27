* 0.3 (27 June 2026)

  - Fix more pattern-match warnings, change additional list parameters
    to `NonEmpty` in `solveTriDiagonal` and `solveCyclicTridiagonal`.

* 0.2 (2 May 2026)

  - Fixes for various pattern-match warnings
  - Remove some unnecessary dependencies (`deepseq`, `tasty-hunit`)
  - The types of `solveTriDiagonal` and `solveCyclicTridiagonal` have
    changed to take arguments of type `NonEmpty a` instead of `[a]`.
    Previously, they simply crashed when given empty lists as
    arguments.
  - Test with GHC 9.14

* 0.1.3.1 (19 Feb 2025)

  Test with up through GHC 9.12

* 0.1.3 (13 Feb 2021)

  Test with up through GHC 9.0
  Allow `tasty-1.4`

  - r1: update homepage
  - r2: allow `tasty-1.5` and `deepseq-1.5`; test on GHC 9.6
  - r3: allow `tasty-quickcheck-0.11`

* 0.1.2 (5 May 2020)

  Improvements to stability/accuraty of `cubForm` and
  `quartForm`, contributed by Jasper Van der Jeugt
  ([#7](https://github.com/diagrams/diagrams-solve/pull/7), [#8](https://github.com/diagrams/diagrams-solve/pull/8))

* 0.1.1 (3 July 2017)

  allow base-4.10 for GHC-8.2
  some minor optimizations
  add QC tests

* 0.1.0.1 (14 February 2016)

  allow base-4.9 for GHC-8.0

* 0.1 (19 April 2015)

  initial release, in conjunction with `diagrams-1.3` --- some
  functionality split out from `diagrams-lib`
