# ppad-eproc

[![](https://img.shields.io/hackage/v/ppad-eproc?color=blue)](https://hackage.haskell.org/package/ppad-eproc)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-eproc-lightblue)](https://docs.ppad.tech/eproc)

Anytime-valid sequential hypothesis testing for bounded random
variables, via the e-process / betting framework of
[Waudby-Smith & Ramdas (2024)][wsr24].

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
  > -- inspect (supremum-so-far) log-wealth and stopping decision at any
  > -- point
  > Bounded.log_wealth s10
  0.916290731874155
  > Bounded.decide cfg s10
  Continue
  >
  > -- with enough evidence, the hypothesis is rejected
  > let s300 = foldl' (Bounded.update cfg) s0 (concat (replicate 30 xs))
  > Bounded.log_wealth s300
  51.14271142862292
  > Bounded.decide cfg s300
  Reject
```

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
```

The `Paired` and `Bernoulli.TwoSided` modules are thin newtype
wrappers over `Bounded`, and inline through with no measurable
overhead. See the criterion suite for the full breakdown across
`Fixed` / `Adaptive` / `Newton` bettors and per-step / fold
workloads.

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
  variables by betting][wsr24]." JRSS-Bounded.
- Ramdas, Grunwald, Vovk, Shafer (2023), "[Game-theoretic statistics
  and safe anytime-valid inference][rgvs23]." Statistical Science.
- Shafer (2021), "[Testing by betting][shafer21]." JRSS-A.

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[wsr24]: https://arxiv.org/abs/2010.09686
[rgvs23]: https://arxiv.org/abs/2210.01948
[shafer21]: https://arxiv.org/abs/1909.03807
