let nixpkgs = import ../../nix/nixpkgs.nix;
in
{
  pkgs ? import nixpkgs {
    overlays = import ../../nix/overlay.nix {

    };
  }
}:
let pkg = import ./default.nix {};
in
with pkgs;

dockerTools.buildImage {
  name = "21it/reckless-trading-bot";
  contents = [ pkg ];

  config = {
    Cmd = [ "${pkg}/bin/reckless-trading-bot-exe" ];
    ExposedPorts = {
      "80/tcp" = {};
    };
  };
}
