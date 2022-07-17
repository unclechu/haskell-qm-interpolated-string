let
  sources = import nix/sources.nix;
  availableBuildTools = [ "cabal" "stack" ];
in

{ pkgs ? import sources.nixpkgs {}
, haskellPackages ? pkgs.haskellPackages

# nix-shell options
, inNixShell ? false # Automatically set to `true` when used by nix-shell
, buildTools ? [ "cabal" ] # See “availableBuildTools”
, withHoogle ? true
, withHLS ? true
}:

assert builtins.all (x: builtins.elem x availableBuildTools) buildTools;

let
  inherit (pkgs) lib;
  cleanSource = pkgs.nix-gitignore.gitignoreRecursiveSource [ ./.gitignore ];

  hsPkgs = haskellPackages.extend (self: super: {
    qm-interpolated-string =
      self.callCabal2nix "qm-interpolated-string" (cleanSource ./.) {};
  });

  generate-cr-and-crlf-tests = pkgs.writeShellApplication {
    name = "generate-cr-and-crlf-tests.sh";
    text = builtins.readFile ./generate-cr-and-crlf-tests.sh;

    runtimeInputs = [
      pkgs.coreutils
      pkgs.gnused
    ];
  };

  shell = hsPkgs.shellFor {
    name = "qm-interpolated-string-shell";
    packages = p: [ p.qm-interpolated-string ];
    inherit withHoogle;

    buildInputs = [
      generate-cr-and-crlf-tests
    ] ++ lib.optional (builtins.elem "cabal" buildTools) hsPkgs.cabal-install
      ++ lib.optional (builtins.elem "stack" buildTools) hsPkgs.stack
      ++ lib.optional withHLS hsPkgs.haskell-language-server;
  };
in

(if inNixShell then shell else {}) // {
  inherit shell;
  inherit (hsPkgs) qm-interpolated-string;
}
