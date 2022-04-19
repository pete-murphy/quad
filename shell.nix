# https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html#working-with-a-project
(import ./default.nix).shellFor {
  tools = {
    cabal = "3.2.0.0";
    hlint = "latest";
    haskell-language-server = "latest";
  };
}
