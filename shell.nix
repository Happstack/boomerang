{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, mtl, stdenv, template-haskell, text }:
      mkDerivation {
        pname = "boomerang";
        version = "1.4.5";
        src = ./.;
        libraryHaskellDepends = [ base mtl template-haskell text ];
        description = "Library for invertible parsing and printing";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
