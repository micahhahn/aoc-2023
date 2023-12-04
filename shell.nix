{ pkgs ? import <nixpkgs> { } }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs {};
  easy-ps = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "4b3e2613a8deba9bf3ed4724ffcea85802af69d8";
      sha256 = null;
    }) {
    inherit pkgs;
  };
in with nixpkgs; 

pkgs.mkShell { 
  buildInputs = [ 
    easy-ps.purs-0_15_8
    easy-ps.psc-package
    haskellPackages.spago
    easy-ps.purs-tidy
    nodePackages.purescript-language-server
    nodejs-slim
  ];
}