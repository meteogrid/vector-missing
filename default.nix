{ mkDerivation, base, primitive, stdenv, template-haskell, vector
}:
mkDerivation {
  pname = "vector-missing";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base primitive template-haskell vector ];
  license = stdenv.lib.licenses.bsd3;
}
