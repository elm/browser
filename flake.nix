{
  description = "A flake to work modify the Elm Browser package";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/d98abf5cf5914e5e4e9d57205e3af55ca90ffc1d";
  };

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      formatter.${system} = pkgs.nixfmt-rfc-style;

      packages.${system}.elmMake = pkgs.writeShellApplication {
        name = "this is a test";
        runtimeInputs = [
          pkgs.elmPackages.elm
        ];
        text = ''
          					  export ELM_HOME="$PWD"
          						cd examples
                      elm make --debug src/Count.elm
        '';
      };

    };
}
