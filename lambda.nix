{ pkgs ? import ./haskell.nix
, hsPkgs ? import ./default.nix {} }:

let
  buildLambda = cabalProject: executable:
    let
      exeComponent = hsPkgs.${cabalProject}.components.exes.${executable};
    in
      pkgs.stdenv.mkDerivation {
        name = executable;
        buildInputs = with pkgs; [ exeComponent patchelf zip ];
        src = ./.;
        phases = [ "installPhase" ];

        installPhase = ''
          mkdir $out/
          pushd $out/

          # Copy the binary to `bootstrap`, which is what AWS Lambda expects:
          # https://docs.aws.amazon.com/lambda/latest/dg/runtimes-custom.html
          cp ${exeComponent}/bin/${executable} bootstrap

          # Copy the shared objects that our binary depends on to a subfolder `lib/`.
          mkdir lib
          cp $(ldd bootstrap | grep -F '=> /' | awk '{print $3}') lib/

          # Patch the binary to point the ELF interpreter and the run-time search
          # path to the shared objects we provide. Note that these paths are location
          # independent: as long as the binary is in the same directory as the folder
          # containing our shared objects, this will work.
          chmod +w bootstrap
          patchelf --set-interpreter ./lib/ld-linux-x86-64.so.2 --set-rpath ./lib --force-rpath bootstrap
          chmod -w bootstrap

          # Finally, we can zip up our binary and the subfolder holding our shared objects.
          # This zip file is the output artefact of this derivation and can be uploaded to AWS
          # Lambda as-is.
          zip -qr ${executable}.zip .

          rm -r lib bootstrap
          popd
        '';
      };
in
  buildLambda "shiba" "shiba-scraper"