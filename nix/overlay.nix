{ ghcVersion }:
final: prev:

{
  "heronPackages-${ghcVersion}" =
    let

      inherit (prev.haskell.lib) doJailbreak dontCheck markUnbroken;

      hOverlay = hfinal: hprev:
        {

          # Heron emulator written in C
          heron-emu =
            prev.stdenv.mkDerivation {
              name = "emu";
              src = ../emulator;
              buildPhase = "gcc -std=c99 -O2 -Wall -o emu ./emu.c";
              installPhase = "mkdir -p $out/bin; install -t $out/bin emu";
            };

          # Fork of retroclash-lib (bumps clash deps only)
          retroclash-lib =
            let
              unmodified =  prev.fetchFromGitHub {
                owner = "gergoerdi";
                repo = "retroclash-lib";
                rev = "d071810e05862ff0e5a79014997d77d9862244b7";
                sha256 = "sha256-qDLJ1GiQNxoQ7XzjzhGgeRr5N0nkTxTgEVlFM5UFxQM=";
              };
            in doJailbreak (dontCheck (hprev.callCabal2nix "retroclash-lib" unmodified {}));

          # development version of barbies-th (nixpkgs version is marked as broken)
          barbies =
            let
              unmodified =  prev.fetchFromGitHub {
                owner = "jcpetruzza";
                repo = "barbies";
                rev = "44ef90af3a87ecf3526b0d977b2849d3db4d7cc4";
                sha256 = "sha256-t3MdlbZY5gh8srwfmcyF8Q/j64uSPk/mnYpNLd/xSXc=";
              };
            in dontCheck (hprev.callCabal2nix "barbies" unmodified {});

          # development version of barbies-th (nixpkgs version is marked as broken)
          barbies-th =
            let
              unmodified =  prev.fetchFromGitHub {
                owner = "fumieval";
                repo = "barbies-th";
                rev = "75eb7d022cbca201beb4c98a1e88ef67679145ed";
                sha256 = "sha256-fVk4KSpdMTC80zsKFe0V1fCX2zc+ySqkpzesq780qhE=";
              };
            in dontCheck (hprev.callCabal2nix "barbies-th" unmodified {});

          # F-lite compiler
          flite =
            let
              unmodified = hprev.callCabal2nixWithOptions "flite" ../flite "" {};
            in unmodified.overrideAttrs (old: {
                 # Copy example benchmarks to install dir
                 postInstall = (old.postInstall or "") + ''
                   mkdir -p "$out/examples"
                   cp -r "examples" "$out/examples"
                 '';
            });
    
          # HDL description of Heron Core
          heron-clash =
            let
              unmodified =
                hprev.callCabal2nixWithOptions "heron" ../hdl "" {
                  inherit (hfinal) flite retroclash-lib barbies barbies-th;
                };
            in
            dontCheck (unmodified.overrideAttrs (old: {

              # Depend on emulator (as reference during testing)
              nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
                hfinal.heron-emu
              ];

              # Generate verilog netlist during build
              postBuild = (old.postBuild or "") + ''
                dist/build/heron/heron --clash -g --verilog Heron.Board -fclash-aggressive-x-optimization -fclash-force-undefined0 -package-db dist/package.conf.inplace -fclash-inline-limit=1000 -fclash-spec-limit=100 -fclash-hdldir ./verilog
              '';
    
              # Copy verilog netlist to install folder
              postInstall = (old.postInstall or "") + ''
                mkdir -p "$out/share"
                cp -r "verilog/" "$out/share/verilog"
              '';
            }));

          # A verilator wrapper around heron-clash for faster simulation
          heron-verilated =
            prev.stdenv.mkDerivation {
              name = "heron-verilated";
              src = ../verilator;
              buildInputs = [ prev.verilator hfinal.heron-clash ];
              buildPhase  = "HERON_VERILOG=${hfinal.heron-clash.outPath}/share/verilog/ make";
              installPhase = "mkdir -p $out/bin; cp ./build/VtopEntity $out/bin/heron-verilated";
            };

          # Vivado-compatible environment installed outside of nix
          # (a la https://github.com/m-labs/artiq)
          vivado = prev.buildFHSEnv {
            name = "vivado";
            targetPkgs = pkgs: with pkgs; let
              ncurses' = prev.ncurses5.overrideAttrs (old: {
                configureFlags = old.configureFlags ++ [ "--with-termlib" ];
                postFixup = "";
              });
            in [
              prev.libxcrypt-legacy
              (ncurses'.override { unicodeSupport = false; })
              prev.zlib
              prev.libuuid
              prev.xorg.libSM
              prev.xorg.libICE
              prev.xorg.libXrender
              prev.xorg.libX11
              prev.xorg.libXext
              prev.xorg.libXtst
              prev.xorg.libXi
              prev.freetype
              prev.fontconfig
            ];

            runScript = "vivado";

            profile = ''
              set -e
              VIVADO_DIR=${builtins.getEnv "XILINX_VIVADO"}
              if [ -z "$VIVADO_DIR" ];
                then echo "Vivado has not been sourced. Exiting."; exit 1;
                else source $VIVADO_DIR/settings64.sh;
              fi
            '';
          };
            
          # Vivado build for Alveo U280
          heron-alveo =
            prev.stdenv.mkDerivation {
              name = "heron-alveo";
              src = ../vivado;
              buildInputs = [ hfinal.heron-clash prev.which hfinal.vivado ];
              doCheck = false;
            
              buildPhase = ''
                export HERON_VERILOG=${hfinal.heron-clash.outPath}/share/verilog/
                ( cd ./alveo_u280; vivado -mode batch -source synth.tcl )
              '';
            
              checkPhase = ''
                ( cd ./sim; vivado -mode batch -source sim.tcl )
              '';
            
              installPhase = ''
                mkdir -p $out/share/heron/;
                # cp ./alveo_u280/heron_alveo.bit       $out/share/heron/;
                cp ./alveo_u280/post_route_prj.zip    $out/share/heron/;
                cp ./alveo_u280/post_route_util.rpt   $out/share/heron/;
                cp ./alveo_u280/post_route_timing.rpt $out/share/heron/;
              '';
            };

          # Vivado build for Ultra96 board (with PYNQ wrapper for prototyping)
          heron-ultra96 = prev.stdenv.mkDerivation {

              broken = true;
              # Vivado segfaults on synthesis after forcing 'x'
              # values to '0'. Since this target requires an IPI block design, it
              # will remain broken until we find a workaround for the segfault....

              name = "heron-ultra96";
              src  = ../vivado;
              buildInputs = [ hfinal.heron-clash prev.which hfinal.vivado ];
              doCheck = false;
            
              buildPhase = ''
                export HERON_VERILOG=${hfinal.heron-clash.outPath}/share/verilog/
                ( cd ./ultra96_pynq; vivado -mode batch -source synth.tcl )
              '';
            
              checkPhase = ''
                ( cd ./sim; vivado -mode batch -source sim.tcl )
              '';
            
              installPhase = ''
                mkdir -p $out/share/heron/;
                cp ./ultra96_pynq/heron_ultra96.bit     $out/share/heron/;
                cp ./ultra96_pynq/heron_ultra96.hwh     $out/share/heron/;
                cp ./ultra96_pynq/heron_notebook.ipynb  $out/share/heron/;
                cp ./ultra96_pynq/post_route_prj.zip    $out/share/heron/;
                cp ./ultra96_pynq/post_route_util.rpt   $out/share/heron/;
                cp ./ultra96_pynq/post_route_timing.rpt $out/share/heron/;
              '';
            };

          # Vivado build for PYNQ-Z2 board (using VIO for programmatic control over JTAG)
          heron-pynqz2-vio =
            prev.stdenv.mkDerivation {
              name = "heron-pynqz2-vio";
              src = ../vivado;
              buildInputs = [ hfinal.heron-clash prev.which hfinal.vivado ];
              doCheck = false;

              buildPhase = ''
                export HERON_VERILOG=${hfinal.heron-clash.outPath}/share/verilog/
                ( cd ./pynqz2_vio; vivado -mode batch -source synth.tcl )
              '';

              checkPhase = ''
                ( cd ./sim; vivado -mode batch -source sim.tcl )
              '';

              installPhase = ''
                mkdir -p $out/share/heron/;
                mkdir -p $out/bin/;
                cp ./pynqz2_vio/heron_pynqz2_vio.bit  $out/share/heron/;
                cp ./pynqz2_vio/heron_pynqz2_vio.ltx  $out/share/heron/;
                cp ./pynqz2_vio/post_route_prj.zip    $out/share/heron/;
                cp ./pynqz2_vio/post_route_util.rpt   $out/share/heron/;
                cp ./pynqz2_vio/post_route_timing.rpt $out/share/heron/;
                cp ./pynqz2_vio/run_vio*              $out/share/heron/;
                ln -s $out/share/heron/run_vio $out/bin/run_vio;
                cat > $out/bin/run_vio <<'EOF'
#!/bin/bash
cd "$(dirname "$0")"
cd ../share/heron/
./run_vio.sh "$@"
EOF
                chmod +x $out/bin/run_vio;
              '';
            };

        };
    in
    prev."clashPackages-${ghcVersion}".extend hOverlay;
}
