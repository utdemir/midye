{ compiler ? "ghc8104" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      midye =
        hself.callCabal2nix
          "midye"
          (gitignore ./.)
          { };
      streaming-with =
        pkgs.haskell.lib.doJailbreak hsuper."streaming-with";
      optics = hself.optics_0_4;
      optics-core = hself.optics-core_0_4;
      optics-extra = hself.optics-extra_0_4;
      optics-th = hself.optics-th_0_4;
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."midye"
    ];
    buildInputs = [
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.ormolu
      pkgs.niv
      pkgs.nixpkgs-fmt
      pkgs.z3
    ];
    withHoogle = false;
    shellHook = ''
      export CABAL_CONFIG=/dev/null
    '';
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."midye");
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  "midye" = myHaskellPackages."midye";
}
