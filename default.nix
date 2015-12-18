{ mkDerivation, base, mtl, stdenv, template-haskell, text }:
mkDerivation {
  pname = "boomerang";
  version = "1.4.5";
  src = ./.;
  libraryHaskellDepends = [ base mtl template-haskell text ];
  description = "Library for invertible parsing and printing";
  license = stdenv.lib.licenses.bsd3;
}
