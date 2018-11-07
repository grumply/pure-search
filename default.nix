{ mkDerivation, base, pure, pure-theme, pure-txt-search, stdenv
}:
mkDerivation {
  pname = "pure-search";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base pure pure-theme pure-txt-search
    ];
  homepage = "github.com/grumply/pure-search";
  license = stdenv.lib.licenses.bsd3;
}