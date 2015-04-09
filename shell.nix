let pkgs = import <nixpkgs> {};
    haskellPackages = pkgs.haskellPackages.override {
      extension = self: super: {
        riemann          = self.callPackage ./. {};
      };
    };
 in pkgs.lib.overrideDerivation haskellPackages.riemann (attrs: {
   buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
 })
