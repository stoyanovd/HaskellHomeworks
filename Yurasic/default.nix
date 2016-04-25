{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, acid-state, base, bytestring, Chart
      , Chart-gtk, colour, containers, data-default-class, glib, gtk
      , HandsomeSoup, HTTP, hxt, lens, mtl, network, network-uri
      , parallel-io, safecopy, stdenv, text, transformers, url
      }:
      mkDerivation {
        pname = "Yurasic";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          acid-state base bytestring Chart Chart-gtk colour containers
          data-default-class glib gtk HandsomeSoup HTTP hxt lens mtl network
          network-uri parallel-io safecopy text transformers url
        ];
        license = stdenv.lib.licenses.bsd2;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
