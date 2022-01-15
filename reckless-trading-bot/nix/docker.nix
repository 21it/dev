let
  project = import ./../../nix/default.nix;
  reckless-trading-bot = project.reckless-trading-bot.components.exes.reckless-trading-bot-exe;
in
  project.pkgs.dockerTools.buildImage {
    name = "21it/reckless-trading-bot";
    contents = [ reckless-trading-bot ];
    config = {
      Cmd = [
        "${reckless-trading-bot}/bin/reckless-trading-bot-exe"
      ];
    };
  }
