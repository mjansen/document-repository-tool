{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, BoundedChan, bytestring, cereal
      , containers, directory, filepath, process, SHA, stdenv, unix
      }:
      mkDerivation {
        pname = "document-repository-tool";
        version = "0.1.0.3";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          async base BoundedChan bytestring cereal containers directory
          filepath process SHA unix
        ];
        description = "Tool to collect pdf, ps, djvu, etc documents into a repository";
        license = stdenv.lib.licenses.gpl3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
