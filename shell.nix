{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "yukibot";
  buildInputs = [ zlib ];
}
