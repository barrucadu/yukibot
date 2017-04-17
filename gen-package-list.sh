#!/bin/sh

NIXFILE=yukibot-packages.nix

echo "{ stdenv, callPackage }:" > $NIXFILE
echo -n "{" >> $NIXFILE

for pkg in yukibot yukibot-core yukibot-backend-* yukibot-plugin-*; do
  echo "Generating nix expression for $pkg..."
  echo >> $NIXFILE
  echo "  ${pkg} = callPackage" >> $NIXFILE
  nix-shell -p cabal2nix --command "cabal2nix $pkg | sed '1s/^/(/' | sed '\$s/$/) {};/' | sed 's/^/    /' >> $NIXFILE"
done

echo "}" >> $NIXFILE
