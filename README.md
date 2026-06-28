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
  0.4054651081081644
  > Bounded.decide cfg s10
  Continue
  >
  > -- with enough evidence, the hypothesis is rejected
  > let s300 = foldl' (Bounded.update cfg) s0 (concat (replicate 30 xs))
  > Bounded.log_wealth s300
  51.142711428622924
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
  time                 13.05 ns   (12.95 ns .. 13.17 ns)
                       1.000 R²   (0.999 R² .. 1.000 R²)
  mean                 13.03 ns   (12.95 ns .. 13.15 ns)
  std dev              314.0 ps   (248.3 ps .. 422.3 ps)

  benchmarking Bounded.update (1000-sample fold)/fixed
  time                 4.840 μs   (4.819 μs .. 4.867 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 4.828 μs   (4.817 μs .. 4.847 μs)
  std dev              44.90 ns   (30.94 ns .. 61.54 ns)

  benchmarking Bounded.update (1000-sample fold)/adaptive
  time                 15.67 μs   (15.66 μs .. 15.69 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 15.67 μs   (15.65 μs .. 15.69 μs)
  std dev              63.74 ns   (55.65 ns .. 75.07 ns)

  benchmarking Bounded.update (1000-sample fold)/newton
  time                 14.43 μs   (14.42 μs .. 14.44 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 14.43 μs   (14.42 μs .. 14.44 μs)
  std dev              46.74 ns   (34.00 ns .. 64.63 ns)
```

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
