# ppad-eproc

[![](https://img.shields.io/hackage/v/ppad-eproc?color=blue)](https://hackage.haskell.org/package/ppad-eproc)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-eproc-lightblue)](https://docs.ppad.tech/eproc)

A pure Haskell implementation of anytime-valid sequential hypothesis
tests and confidence sequences for bounded random variables, via the
e-process ("testing by betting") framework of [Waudby-Smith & Ramdas
(2024)][wsr24].

Observations are consumed one at a time. Unlike with classical tests,
you can check the verdict (or the confidence interval) after every
observation and stop whenever you like, without inflating the error
rate.

Includes tests for bounded means, paired differences, and Bernoulli
rates, mixtures of such tests, and confidence sequences for bounded
means.

## Usage

A sample GHCi session:

```
  > -- import qualified
  > import qualified Numeric.Eproc.Bounded as B
  > import Data.List (find)
  >
  > -- test H_0: E[x] = 0.5 for x in [0, 1], at level 0.001
  > let Right cfg = B.config 0.5 0.0 1.0 1.0e-3 B.Newton
  >
  > -- a stream of observations with mean 0.8
  > let xs = cycle [1, 1, 0, 1, 1, 0, 1, 1, 1, 1]
  >
  > -- after ten observations, the evidence is weak
  > let s10 = foldl' (B.update cfg) (B.initial cfg) (take 10 xs)
  > B.decide cfg s10
  Continue
  > B.p_value s10
  0.8
  >
  > -- check after every observation, stopping at the first rejection
  > let ss = scanl (B.update cfg) (B.initial cfg) xs
  > let Just s = find ((== B.Reject) . B.decide cfg) ss
  > B.samples s
  59
  > B.p_value s
  8.474770976464489e-4
```

Confidence sequences are the estimation analogue: intervals that are
valid at every sample size simultaneously. Continuing from the above:

```
  > import qualified Numeric.Eproc.ConfSeq as C
  >
  > -- 95% confidence sequence for the mean of x in [0, 1], on a
  > -- 100-point grid
  > let Right cfg = C.config 0.0 1.0 0.05 100
  > let after n = foldl' (C.update cfg) (C.initial cfg) (take n xs)
  >
  > C.interval cfg (after 10)
  Just (0.3564356435643564,1.0)
  > C.interval cfg (after 100)
  Just (0.6633663366336634,0.9108910891089109)
  > C.interval cfg (after 1000)
  Just (0.7425742574257426,0.8514851485148515)
```

See the haddocks for additional examples.

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/eproc](https://docs.ppad.tech/eproc).

## Performance

Current benchmark figures on an M4 Silicon MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking Bounded.update (one step)/newton
  time                 15.27 ns   (15.23 ns .. 15.30 ns)

  benchmarking Bounded.update (1000-sample fold)/fixed
  time                 8.271 μs   (8.266 μs .. 8.275 μs)

  benchmarking Bounded.update (1000-sample fold)/adaptive
  time                 15.38 μs   (15.35 μs .. 15.41 μs)

  benchmarking Bounded.update (1000-sample fold)/newton
  time                 17.00 μs   (16.98 μs .. 17.02 μs)

  benchmarking Bernoulli.update (1000-sample fold)/newton
  time                 14.85 μs   (14.84 μs .. 14.87 μs)

  benchmarking Bernoulli.TwoSided.update (1000-sample fold)/newton
  time                 16.70 μs   (16.68 μs .. 16.72 μs)

  benchmarking Mixture.update (one step)/K=4
  time                 22.24 ns   (22.21 ns .. 22.27 ns)

  benchmarking ConfSeq.update (one step, g = 200)/plug-in
  time                 2.100 μs   (2.097 μs .. 2.103 μs)

  benchmarking ConfSeq.update (1000-sample fold, g = 200)/plug-in
  time                 193.3 μs   (192.3 μs .. 194.5 μs)
```

You should compile with the 'llvm' flag for maximum performance.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-eproc
```

to get a REPL for the main library.

## References

- Waudby-Smith & Ramdas (2024), "[Estimating means of bounded random
  variables by betting][wsr24]." JRSS-B.
- Ramdas, Grünwald, Vovk, Shafer (2023), "[Game-theoretic statistics
  and safe anytime-valid inference][rgvs23]." Statistical Science.
- Shafer (2021), "[Testing by betting][shafer21]." JRSS-A.

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[wsr24]: https://arxiv.org/abs/2010.09686
[rgvs23]: https://arxiv.org/abs/2210.01948
[shafer21]: https://arxiv.org/abs/1909.03807
