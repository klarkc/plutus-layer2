{
  inputs.horizon-wave-ocean.url = "git+https://gitlab.homotopic.tech/horizon/wave-ocean/horizon-wave-ocean-platform";
  inputs.nixpkgs.follows = "horizon-wave-ocean/nixpkgs";
  inputs.utils.url = "github:ursi/flake-utils";
  # to generate docs
  inputs.plutus.url = "github:input-output-hk/plutus";

  outputs = { self, utils, ... }@inputs:
    utils.apply-systems
      {
        inherit inputs;
        # TODO support additional systems on hor
        #  horizon-platform is only supporting linux
        systems = [ "x86_64-linux" ];
      }
      ({ pkgs, system, ... }@context:
        let
          hsPkgs =
            with pkgs.haskell.lib;
            inputs.horizon-wave-ocean.legacyPackages.${system}.extend (hfinal: hprev:
              {
                layer2 = hprev.developPackage {
                  root = ./.;
                };
              });
          script = pkgs.runCommand "script"
            {
              buildInputs = [ hsPkgs.layer2 ];
            }
            ''layer2 > $out'';
          script-check = pkgs.runCommand "script-check" { }
            ''cat ${script}; touch $out'';
          serve-docs = import ./nix/serve-docs.nix inputs context {
            inherit hsPkgs;
            # TODO transform additionalPkgs in excludePkgs to reduce boilerplate
            #  we could collect all entries from cabal build-depends
            #  (maybe through hsPkgs)
            additionalPkgs = [
              # FIXME cardano-api doc is not building
              #"cardano-api"
              #"layer2"
            ];
          };
        in
        {
          packages.default = hsPkgs.layer2;
          packages.script = script;
          packages.serve-docs = serve-docs;

          devShells.default = hsPkgs.layer2.env.overrideAttrs (attrs: {
            # TODO Extract GHC version from hsPkgs
            buildInputs = with pkgs.haskell.packages.ghc8107; attrs.buildInputs ++ [
              serve-docs
              pkgs.cabal-install
              haskell-language-server
              hlint
            ];
          });

          checks.default = script-check;
        });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    allow-import-from-derivation = "true";
    # This sets the flake to use nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [
      "https://klarkc.cachix.org?priority=99"
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
      "https://cache.nixos.org"
    ];
    extra-trusted-public-keys = [
      "klarkc.cachix.org-1:R+z+m4Cq0hMgfZ7AQ42WRpGuHJumLLx3k0XhwpNFq9U="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];
  };
}
