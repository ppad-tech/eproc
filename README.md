# ppad-eproc

Anytime-valid sequential testing for Haskell, via e-processes and the
betting framework.

Implements the bounded-mean and paired two-sample tests of Waudby-Smith
& Ramdas (2024) using predictable-plug-in bettors (fixed-lambda,
aGRAPA, ONS). Tests are valid under optional stopping: reject as soon
as the wealth process exceeds `1/alpha`, with type-I error controlled
at `alpha` regardless of when you stop.

## Use

```haskell
import qualified Statistics.EProcess as E

-- Test H0: E[X] = 0.5 against H1: E[X] != 0.5,
-- samples bounded in [0, 1], alpha = 1e-6.
let cfg = E.meanConfig 0.5 0.0 1.0 1.0e-6 E.ons
    s0  = E.initMeanState cfg

-- Stream samples through the test:
let s1 = E.updateMean cfg s0 x1
    s2 = E.updateMean cfg s1 x2
    ...

case E.decideMean cfg sN of
  E.Reject   -> ...  -- H0 falsified
  E.Continue -> ...  -- more data needed
```

For paired two-sample testing, see `Statistics.EProcess.TwoSample`.

## Background

- Waudby-Smith & Ramdas (2024), "Estimating means of bounded random
  variables by betting." JRSS-B.
- Ramdas, Grunwald, Vovk, Shafer (2023), "Game-theoretic statistics
  and safe anytime-valid inference." Statistical Science.
- Shafer (2021), "Testing by betting." JRSS-A.
