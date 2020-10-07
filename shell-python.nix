with import <nixpkgs> {};
mkShell {
  buildInputs = [
    (python38.withPackages (ps: with ps; [
      numpy
      matplotlib
    ]))
  ];
}
