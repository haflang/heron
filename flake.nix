{
  description = "The Heron Core for Pure Graph Reduction";

  # pinning nixpkgs by looking at this tool for clash versions
  # https://lazamar.co.uk/nix-versions
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/e1ee359d16a1886f0771cc433a00827da98d861c";
    flake-utils.url = "github:numtide/flake-utils";
    clash-compiler.url = "github:clash-lang/clash-compiler/v1.8.1";
  };

  outputs = { self, nixpkgs, flake-utils, clash-compiler }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        ghcVersion = "ghc902";
        pkgs = import nixpkgs {
                 inherit system; overlays = [
                   clash-compiler.overlays.${ghcVersion}
                   (import ./nix/overlay.nix { inherit ghcVersion; })
                 ];
               };
      in {
        packages = {
          inherit (pkgs."heronPackages-${ghcVersion}")
            flite
            vivado
            heron-alveo
            heron-ultra96
            heron-clash
            heron-verilated
            heron-emu;
          default = pkgs."heronPackages-${ghcVersion}".heron-clash;
        };

        # Setup development shell (run via `nix develop`)
        devShells.default = pkgs.mkShell {
          LC_ALL = "C.UTF-8";
          buildInputs = with pkgs."heronPackages-${ghcVersion}"; [
            flite
            heron-emu
            # Packages for IDE support
            haskell-language-server
            apply-refact
            hlint
            stylish-haskell
            hasktags
            hoogle
            cabal-install
            ghcid
          ];
            inputsFrom = map (p: if __hasAttr "env" p
                                   then __getAttr "env" p
                                   else {})
                         [ self.packages.${system}.heron-emu
                           self.packages.${system}.heron-clash
                           self.packages.${system}.flite
                         ];
        };
        devShell = self.devShells.${system}.default;
      });
}
