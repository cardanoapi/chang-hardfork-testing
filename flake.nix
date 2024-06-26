{
  description = "End-to-End Testing Framework for Cardano Haskell APIs";


  inputs = {
    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";
    iohk-nix.follows = "iogx/iohk-nix";

    hackage = {
      url = "github:input-output-hk/hackage.nix/3db1b82e4dbc6b44ddd5b98a88a6ea82be29efd5";
      flake = false;
    };

    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix/34946405e89825c757aca192ab13a31313ccef8e";
      inputs.hackage.follows = "hackage";
    };
  };

  outputs = inputs: inputs.iogx.lib.mkFlake {
    inherit inputs;
    repoRoot = ./.;
    systems = [ "x86_64-darwin" "x86_64-linux" "aarch64-darwin" ];
    outputs = import ./nix/outputs.nix;
  };


  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    accept-flake-config = true;
    allow-import-from-derivation = true;
  };
}
