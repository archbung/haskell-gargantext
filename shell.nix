{ pkgs ? import ./nix/pkgs.nix {} }:
let
  myBuildInputs = [
    pkgs.pkgs.docker-compose
    pkgs.pkgs.haskell-language-server
    pkgs.pkgs.stack
  ];
in
pkgs.pkgs.mkShell {
  name = pkgs.shell.name;
  LOCALE_ARCHIVE = if pkgs.pkgs.stdenv.isLinux then "${pkgs.pkgs.glibcLocales}/lib/locale/locale-archive" else "";
  #home.sessionVariables.LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  shellHook = pkgs.shell.shellHook;
  buildInputs = pkgs.shell.buildInputs ++ myBuildInputs;
}
