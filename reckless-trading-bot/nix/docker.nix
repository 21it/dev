let
  project = import ./../../nix/default.nix;
  pkgs = project.pkgs;
  reckless-trading-bot = project.reckless-trading-bot.components.exes.reckless-trading-bot-exe;
in
  pkgs.dockerTools.buildImage {
    name = "21it/reckless-trading-bot";
    contents = [
      pkgs.cacert
      reckless-trading-bot
    ];
    config = {
      Cmd = [
        "${reckless-trading-bot}/bin/reckless-trading-bot-exe"
      ];
    };
  }
