{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs = { nixpkgs, ... }: with nixpkgs.legacyPackages.x86_64-linux; {
    devShells.x86_64-linux.default = mkShell {
      buildInputs = [
        haskell-language-server
        ghc
        cabal-install
        zlib
        llvm
      ];
    };
  };
}
