with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    stack
    ghc
    ghcid 
    hlint # linter
    ormolu # code fomatting
  ];
}
