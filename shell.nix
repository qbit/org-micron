{
  pkgs ? import <nixpkgs> { },
}:
let
  org-test = pkgs.fetchFromGitHub {
    owner = "bzg";
    repo = "org-mode";
    tag = "release_9.7.33";
    hash = "sha256-pLpNlLO2SR3QKOFmiIkH57TEbSAmfWxe1+OVhy7mu9s=";
  };
in
pkgs.mkShell {
  shellHook = ''
    PS1='\u@\h:\w; '
    export ORG_TEST_DIR=${org-test}/testing/
  '';
  buildInputs = with pkgs; [
    gnumake
    emacs
    emacsPackages.org
    org-test
  ];
}
