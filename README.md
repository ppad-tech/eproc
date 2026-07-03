# ppad-eproc

[![](https://img.shields.io/hackage/v/ppad-eproc?color=blue)](https://hackage.haskell.org/package/ppad-eproc)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-eproc-lightblue)](https://docs.ppad.tech/eproc)

Anytime-valid sequential hypothesis testing and confidence sequences
for bounded random variables, via the e-process / betting framework
of [Waudby-Smith & Ramdas (2024)][wsr24].

## Usage

A sample GHCi session:

```
  > import qualified Numeric.Eproc.Bounded as Bounded
  >
  > -- hypothesis: E[X] = 0.5 for samples in [0, 1] at alpha = 1e-3, tested
  > -- with the Newton bettor. 'config' returns 'Either ConfigError Config'
  > -- and refuses inputs outside the mathematical regime.
  > let Right cfg = Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Newton
  > let s0        = Bounded.initial cfg
  >
  > -- ten observations (drifting from hypothesis), and state afterwards
  > let xs  = [1, 1, 0, 1, 1, 0, 1, 1, 1, 1]
  > let s10 = foldl' (Bounded.update cfg) s0 xs
  >
  > -- inspect current and supremum-so-far log-wealth, and the stopping
  > -- decision, at any point
  > Bounded.log_wealth s10
  0.6187772969384595
  > Bounded.log_wealth_sup s10
  0.916290731874155
  > Bounded.decide cfg s10
  Continue
  >
  > -- with enough evidence, the hypothesis is rejected
  > let s300 = foldl' (Bounded.update cfg) s0 (concat (replicate 30 xs))
  > Bounded.log_wealth_sup s300
  51.14271142862292
  > Bounded.decide cfg s300
  Reject
```

Confidence sequences invert the same machinery into time-uniform
interval estimates: valid at every sample size simultaneously, so you
can watch the interval shrink and stop whenever it's tight enough:

```
  > import qualified Numeric.Eproc.ConfSeq as CS
  >
  > -- estimate a mean in [0, 1] at 95% coverage, on a 100-point grid
  > let Right cfg = CS.config 0.0 1.0 0.05 100
  > let s0        = CS.initial cfg
  >
  > -- the same drifting stream as above; the interval closes in on
  > -- its empirical mean of 0.8
  > let xs = concat (replicate 30 [1, 1, 0, 1, 1, 0, 1, 1, 1, 1])
  > CS.interval cfg (foldl' (CS.update cfg) s0 xs)
  Just (0.7227722772277227,0.8712871287128713)
```

Every test module also reports its evidence as an anytime-valid
p-value (`p_value`) and a normalized log e-value (`log_evalue`); the
`Mixture` module combines several e-processes into a single test with
power against a union of alternatives (see its haddocks for a worked
sign-plus-magnitude example).

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/eproc](https://docs.ppad.tech/eproc).

## Performance

The aim is best-in-class performance for pure, highly-auditable Haskell
code.

Current benchmark figures on an M4 Silicon MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking Bounded.update (one step)/newton
  time                 13.96 ns   (13.88 ns .. 14.04 ns)

  benchmarking Bounded.update (1000-sample fold)/fixed
  time                 7.951 μs   (7.944 μs .. 7.959 μs)

  benchmarking Bounded.update (1000-sample fold)/adaptive
  time                 12.69 μs   (12.68 μs .. 12.71 μs)

  benchmarking Bounded.update (1000-sample fold)/newton
  time                 14.61 μs   (14.57 μs .. 14.64 μs)

  benchmarking Bernoulli.update (1000-sample fold)/newton
  time                 14.64 μs   (14.63 μs .. 14.65 μs)

  benchmarking Bernoulli.TwoSided.update (1000-sample fold)/newton
  time                 14.83 μs   (14.81 μs .. 14.84 μs)

  benchmarking Mixture.update (one step)/K=4
  time                 31.38 ns   (31.21 ns .. 31.55 ns)

  benchmarking ConfSeq.update (one step, g = 200)/plug-in
  time                 2.121 μs   (2.118 μs .. 2.124 μs)

  benchmarking ConfSeq.update (1000-sample fold, g = 200)/plug-in
  time                 241.2 μs   (239.7 μs .. 243.2 μs)
```

The `Paired` and `Bernoulli.TwoSided` modules are thin newtype
wrappers over `Bounded`, and inline through with no measurable
overhead. `ConfSeq` updates cost O(live grid candidates) per
observation, so long streams get cheaper as candidates are rejected
(visible in the sub-linear fold figure above). See the criterion
suite for the full breakdown across `Fixed` / `Adaptive` / `Newton`
bettors and per-step / fold workloads.

You should compile with the `llvm` flag for maximum performance.

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
- Ramdas, Grunwald, Vovk, Shafer (2023), "[Game-theoretic statistics
  and safe anytime-valid inference][rgvs23]." Statistical Science.
- Shafer (2021), "[Testing by betting][shafer21]." JRSS-A.

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[wsr24]: https://arxiv.org/abs/2010.09686
[rgvs23]: https://arxiv.org/abs/2210.01948
[shafer21]: https://arxiv.org/abs/1909.03807
