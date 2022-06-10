let
  project = import ./../../nix/default.nix;
  pkgs = project.pkgs;
  reckless-trading-bot = project.reckless-trading-bot.components.exes.reckless-trading-bot-exe;
in
  pkgs.dockerTools.buildImage {
    name = "21it/reckless-trading-bot";
    extraCommands = "mkdir -m 0777 tmp";
    contents = [
      #
      # TODO : put all deps into package itself
      #
      pkgs.cacert
      pkgs.gnuplot
      pkgs.librsvg
      reckless-trading-bot
    ];
    config = {
      Cmd = [
        "${reckless-trading-bot}/bin/reckless-trading-bot-exe"
      ];
    };
  }
