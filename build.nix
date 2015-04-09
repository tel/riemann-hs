let pkgs = import <nixpkgs> {};
    haskellPackages = pkgs.haskellPackages.override {
      extension = self: super: {
        riemann     = self.callPackage ../../serverForks/riemann-hs {};
      };
    };
 in haskellPackages.riemann
