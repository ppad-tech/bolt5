{
  description = "A Haskell implementation of BOLT #5.";

  inputs = {
    ppad-nixpkgs = {
      type = "git";
      url  = "git://git.ppad.tech/nixpkgs.git";
      ref  = "master";
    };
    ppad-sha256 = {
      type = "git";
      url  = "git://git.ppad.tech/sha256.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
    };
    ppad-secp256k1 = {
      type = "git";
      url  = "git://git.ppad.tech/secp256k1.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
      inputs.ppad-sha256.follows = "ppad-sha256";
    };
    ppad-ripemd160 = {
      type = "git";
      url  = "git://git.ppad.tech/ripemd160.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
    };
    ppad-tx = {
      type = "git";
      url  = "git://git.ppad.tech/tx.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
      inputs.ppad-sha256.follows = "ppad-sha256";
    };
    ppad-bolt3 = {
      type = "git";
      url  = "git://git.ppad.tech/bolt3.git";
      ref  = "master";
      inputs.ppad-nixpkgs.follows = "ppad-nixpkgs";
      inputs.ppad-sha256.follows = "ppad-sha256";
      inputs.ppad-secp256k1.follows = "ppad-secp256k1";
      inputs.ppad-ripemd160.follows = "ppad-ripemd160";
      inputs.ppad-tx.follows = "ppad-tx";
    };
    flake-utils.follows = "ppad-nixpkgs/flake-utils";
    nixpkgs.follows = "ppad-nixpkgs/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, ppad-nixpkgs
            , ppad-sha256, ppad-secp256k1, ppad-ripemd160
            , ppad-tx, ppad-bolt3 }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        lib = "ppad-bolt5";

        pkgs  = import nixpkgs { inherit system; };
        hlib  = pkgs.haskell.lib;
        llvm  = pkgs.llvmPackages_19.llvm;
        clang = pkgs.llvmPackages_19.clang;

        sha256 = ppad-sha256.packages.${system}.default;
        sha256-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag sha256 "llvm")
            [ llvm clang ];

        secp256k1 = ppad-secp256k1.packages.${system}.default;
        secp256k1-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag secp256k1 "llvm")
            [ llvm clang ];

        ripemd160 = ppad-ripemd160.packages.${system}.default;
        ripemd160-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag ripemd160 "llvm")
            [ llvm clang ];

        tx = ppad-tx.packages.${system}.default;
        tx-llvm =
          hlib.addBuildTools
            (hlib.enableCabalFlag tx "llvm")
            [ llvm clang ];

        bolt3 = ppad-bolt3.packages.${system}.default;

        hpkgs = pkgs.haskell.packages.ghc910.extend (new: old: {
          ppad-sha256 = sha256-llvm;
          ppad-secp256k1 = secp256k1-llvm;
          ppad-ripemd160 = ripemd160-llvm;
          ppad-tx = tx-llvm;
          ppad-bolt3 = bolt3;
          ${lib} = new.callCabal2nix lib ./. { };
        });

        cc    = pkgs.stdenv.cc;
        ghc   = hpkgs.ghc;
        cabal = hpkgs.cabal-install;
      in
        {
          packages.default = hpkgs.${lib};

          packages.haddock = hpkgs.${lib}.doc;

          devShells.default = hpkgs.shellFor {
            packages = p: [
              (hlib.doBenchmark p.${lib})
            ];

            buildInputs = [
              cabal
              cc
              llvm
            ];

            shellHook = ''
              PS1="[${lib}] \w$ "
              echo "entering ${system} shell, using"
              echo "cc:    $(${cc}/bin/cc --version)"
              echo "ghc:   $(${ghc}/bin/ghc --version)"
              echo "cabal: $(${cabal}/bin/cabal --version)"
            '';
          };
        }
      );
}
