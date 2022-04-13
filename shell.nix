# https://input-output-hk.github.io/haskell.nix/tutorials/development.html#how-to-get-a-development-shell
let
  project = import ./default.nix;
in
  project.shellFor {}