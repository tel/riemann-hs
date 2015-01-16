{ cabal, cereal, dataDefault, doctest, errors, filepath, HUnit
, lens, network, protobuf, QuickCheck, testFramework
, testFrameworkHunit, testFrameworkQuickcheck2, text, time
, transformers
}:

cabal.mkDerivation (self:
let
  lib         = self.stdenv.lib;
  isWithin    = p: dirPath: lib.hasPrefix (toString dirPath) (toString p);
  cabalFilter = path: type: (let pathBaseName = baseNameOf path; in
                               !(lib.hasSuffix "~" pathBaseName) &&
                               !(lib.hasSuffix "#" pathBaseName) &&
                               !(lib.hasPrefix "." pathBaseName) &&
                               (
                                   pathBaseName == "riemann.cabal" ||
                                   pathBaseName == "LICENSE"       ||
                                   pathBaseName == "Setup.hs"      ||
                                   isWithin path ./src             ||
                                   false
                               )
                            );
in {
  pname = "riemann";
  version = "0.0.0.1";
  src = builtins.filterSource cabalFilter ./.;
  buildDepends = [
    cereal dataDefault errors lens network protobuf text time
    transformers
  ];
  testDepends = [
    doctest filepath HUnit QuickCheck testFramework testFrameworkHunit
    testFrameworkQuickcheck2
  ];
  meta = {
    homepage = "https://github.com/tel/riemann-hs";
    description = "A Riemann client for Haskell";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})