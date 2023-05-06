{
  description = "plutarch-template";

  # nixConfig = {
  #   extra-experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
  #   extra-substituters = [ "https://cache.iog.io" ];
  #   extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  #   allow-import-from-derivation = "true";
  #   bash-prompt = "\\[\\e[0;92m\\][\\[\\e[0;92m\\]nix develop:\\[\\e[0;92m\\]\\w\\[\\e[0;92m\\]]\\[\\e[0;92m\\]$ \\[\\e[0m\\]";
  #   max-jobs = "auto";
  #   auto-optimise-store = "true";
  # };

  inputs = {
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
    plutarch.url = "github:Plutonomicon/plutarch";
    ply.url = "github:mlabs-haskell/ply?ref=master";
    plutus-simple-model.url = "github:mlabs-haskell/plutus-simple-model";
    liqwid-libs.url = "github:Liqwid-Labs/liqwid-libs";
  };

  outputs = inputs@{ self, tooling, plutus-simple-model, plutarch, ply, liqwid-libs }: tooling.lib.mkFlake { inherit self; }
    {
      imports = [
        (tooling.lib.mkHaskellFlakeModule1 {
          project.src = ./.;
          project.shell.withHoogle = true;
          project.modules = [
            ({ config, ... }: {
              packages.plutus-simple-model.doHaddock = false;
            })
          ];
          project.extraHackage = [
            "${plutus-simple-model}"
            "${plutarch}"
            "${plutarch}/plutarch-extra"
            "${ply}/ply-core"
            "${ply}/ply-plutarch"
            "${liqwid-libs}/liqwid-plutarch-extra"
            "${liqwid-libs}/plutarch-quickcheck"
            "${liqwid-libs}/plutarch-unit"
            "${liqwid-libs}/liqwid-script-export"
            "${liqwid-libs}/plutarch-context-builder"
          ];
        })
      ];
    };
}