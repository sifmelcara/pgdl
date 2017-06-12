{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, binary, brick, bytestring, Cabal
      , conduit, conduit-extra, configurator, containers, data-default
      , directory, directory-listing-webpage-parser, filepath
      , http-conduit, http-types, microlens, process, resourcet, stdenv
      , tagsoup, text, time, transformers, unix, vector, vty
      }:
      mkDerivation {
        pname = "pgdl";
        version = "10.8";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base binary brick bytestring Cabal conduit conduit-extra
          configurator containers directory
          directory-listing-webpage-parser filepath http-conduit http-types
          microlens process resourcet tagsoup text time transformers unix
          vector vty
        ];
        description = "browse directory listing webpages and download files from them";
        license = stdenv.lib.licenses.publicDomain;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
