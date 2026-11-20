{
  description = "Anytime-valid sequential testing via e-processes.";

  inputs = {
    ppad-nixpkgs = {
      type = "git";
      url  = "git://git.ppad.tech/nixpkgs.git";
      ref  = "master";
    };
    ppad-random = {
      type = "git";
      url  = "git://git.ppad.tech/random.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
    };
    flake-utils.follows = "ppad-nixpkgs/flake-utils";
    nixpkgs.follows = "ppad-nixpkgs/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, ppad-nixpkgs, ppad-random }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        lib = "ppad-eproc";

        pkgs = import nixpkgs { inherit system; };
        hlib = pkgs.haskell.lib;

        random = ppad-random.packages.${system}.default;

        hpkgs = pkgs.haskell.packages.ghc910.extend (new: old: {
          ppad-random = random;
          ${lib} = old.callCabal2nixWithOptions lib ./. "--enable-profiling" {
            ppad-random = new.ppad-random;
          };
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
