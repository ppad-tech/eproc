{
  description = "Anytime-valid sequential testing via e-processes.";

  inputs = {
    ppad-nixpkgs = {
      type = "git";
      url  = "git://git.ppad.tech/nixpkgs.git";
      ref  = "master";
    };
    flake-utils.follows = "ppad-nixpkgs/flake-utils";
    nixpkgs.follows = "ppad-nixpkgs/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, ppad-nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        lib = "ppad-eproc";

        pkgs = import nixpkgs { inherit system; };
        hlib = pkgs.haskell.lib;

        hpkgs = pkgs.haskell.packages.ghc910.extend (new: old: {
          ${lib} = old.callCabal2nixWithOptions lib ./. "--enable-profiling" {};
        });

        cabal = hpkgs.cabal-install;
        cc    = pkgs.stdenv.cc;
        ghc   = hpkgs.ghc;
        llvm  = pkgs.llvmPackages_19.llvm;
        clang = pkgs.llvmPackages_19.clang;
      in
        {
          packages.default = hpkgs.${lib};

          devShells.default = hpkgs.shellFor {
            packages = p: [
              (hlib.doBenchmark p.${lib})
            ];

            buildInputs = [
              cabal
              cc
              llvm
              clang
            ];

            doBenchmark = true;

            shellHook = ''
              PS1="[${lib}] \w$ "
              echo "entering ${system} shell, using"
              echo "cabal: $(${cabal}/bin/cabal --version)"
              echo "cc:    $(${cc}/bin/cc --version)"
              echo "ghc:   $(${ghc}/bin/ghc --version)"
              echo "llc:   $(${llvm}/bin/llc --version | head -2 | tail -1)"
            '';
          };
        }
      );
}
