{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, configurator, containers
      , directory, fgl, filepath, HDBC, HDBC-postgresql, HDBC-sqlite3
      , HUnit, MissingH, mtl, mysql-simple, process, random, split
      , stdenv, template-haskell, text, time, yaml-light
      }:
      mkDerivation {
        pname = "dbmigrations";
        version = "1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring configurator containers directory fgl filepath HDBC
          HDBC-postgresql HDBC-sqlite3 mtl mysql-simple random
          template-haskell text time yaml-light
        ];
        executableHaskellDepends = [ base configurator split ];
        testHaskellDepends = [
          base bytestring configurator containers directory fgl filepath HDBC
          HDBC-postgresql HDBC-sqlite3 HUnit MissingH mtl process
          template-haskell text time yaml-light
        ];
        description = "An implementation of relational database \"migrations\"";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
