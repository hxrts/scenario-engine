{
  description = "Scenario Planning Game (PureScript)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in {
      devShells = forAllSystems (system:
        let pkgs = nixpkgsFor.${system};
        in {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              nodejs_22
              esbuild
              just
            ];
            shellHook = ''
              # purs and spago are pinned in package.json and installed locally via npm
              export PATH="$PWD/node_modules/.bin:$PATH"
              if [ ! -x node_modules/.bin/purs ]; then
                echo "Installing pinned PureScript toolchain (npm install)..."
                npm install --silent
              fi
              echo "PureScript $(purs --version) / spago $(spago --version)"
              echo "Run 'just' to list tasks."
            '';
          };
        });
    };
}
