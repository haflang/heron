{
  description = "The Heron Core for Pure Graph Reduction";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    clash-compiler.url = "github:clash-lang/clash-compiler";
  };

  outputs = { self, nixpkgs, flake-utils, clash-compiler }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        ghcVersion = "ghc902";
        pkgs = import nixpkgs {
                 inherit system; overlays = [
                     clash-compiler.overlays.${ghcVersion}
                     (import ./nix/overlay.nix { inherit ghcVersion; })
                 ]; };

      in {
        packages = {
          inherit (pkgs."heronPackages-${ghcVersion}")
            flite
            heron-alveo
            heron-ultra96
            heron-clash
            heron-emu;
          default = pkgs."heronPackages-${ghcVersion}".heron-clash;
        };

        # Setup development shell (run via `nix develop`)
        devShells.default = pkgs.mkShell {
          LC_ALL = "C.UTF-8";
          buildInputs = with pkgs; [
            # pkgs."heronPackages-${ghcVersion}".haskell-language-server
            pkgs."heronPackages-${ghcVersion}".flite
            pkgs."heronPackages-${ghcVersion}".heron-emu
            ghcid
            cabal-install
          ];
            inputsFrom = map (p: if __hasAttr "env" p
                                   then __getAttr "env" p
                                   else {})
                             (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      });
}
