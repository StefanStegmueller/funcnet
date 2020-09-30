with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    stack
    ghc
    ghcid
    hlint
  ];
}
