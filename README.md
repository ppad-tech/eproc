# eproc

[![](https://img.shields.io/hackage/v/ppad-eproc?color=blue)](https://hackage.haskell.org/package/ppad-eproc)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-eproc-lightblue)](https://docs.ppad.tech/eproc)

Anytime-valid sequential hypothesis testing for bounded random
variables, via the e-process / betting framework of
[Waudby-Smith & Ramdas (2024)][wsr24]. Bounded-mean and paired
two-sample tests are valid under optional stopping: reject as soon as
the wealth process exceeds `1/alpha`, with type-I error controlled at
`alpha` regardless of when the user stops streaming samples.

## Usage

A sample GHCi session:

```
  > -- import qualified
  > import qualified Numeric.Eproc.Bettor as B
  > import qualified Numeric.Eproc.Mean as M
  >
  > -- test H_0: E[X] = 0.5 for samples in [0, 1] at alpha = 1e-3,
  > -- with the ONS bettor
  > let cfg = M.config 0.5 0.0 1.0 1.0e-3 B.Ons
  >
  > -- streaming interface: 'initial' then fold observations through 'update'
  > let s0 = M.initial cfg
  > let xs = [1, 1, 0, 1, 1, 0, 1, 1, 1, 1]  -- mean 0.8, drifts from H_0
  > let s10 = foldl (M.update cfg) s0 xs
  >
  > -- inspect wealth and verdict at any point
  > M.samples s10
  10
  > M.log_wealth s10
  0.7182493502552663
  > M.decide cfg s10
  Continue
  >
  > -- with enough evidence the test rejects
  > let s300 = foldl (M.update cfg) s0 (concat (replicate 30 xs))
  > M.log_wealth s300
  53.092214534054165
  > M.decide cfg s300
  Reject
```

For the paired two-sample mean-equality test, see
`Numeric.Eproc.Paired`.

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/eproc](https://docs.ppad.tech/eproc).

## Performance

The aim is best-in-class performance for pure, highly-auditable Haskell
code.

Current benchmark figures on an M4 Silicon MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking Mean.update (one step)/ons
  time                 13.05 ns   (12.95 ns .. 13.17 ns)
                       1.000 R²   (0.999 R² .. 1.000 R²)
  mean                 13.03 ns   (12.95 ns .. 13.15 ns)
  std dev              314.0 ps   (248.3 ps .. 422.3 ps)

  benchmarking Mean.update (1000-sample fold)/fixed
  time                 4.840 μs   (4.819 μs .. 4.867 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 4.828 μs   (4.817 μs .. 4.847 μs)
  std dev              44.90 ns   (30.94 ns .. 61.54 ns)

  benchmarking Mean.update (1000-sample fold)/agrapa
  time                 15.67 μs   (15.66 μs .. 15.69 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 15.67 μs   (15.65 μs .. 15.69 μs)
  std dev              63.74 ns   (55.65 ns .. 75.07 ns)

  benchmarking Mean.update (1000-sample fold)/ons
  time                 14.43 μs   (14.42 μs .. 14.44 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 14.43 μs   (14.42 μs .. 14.44 μs)
  std dev              46.74 ns   (34.00 ns .. 64.63 ns)
```

The inner update loop is fully fused: the `fixed` bettor allocates
nothing per step, and the `agrapa` and `ons` bettors allocate a small
constant per-step state record.

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
