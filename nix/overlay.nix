{ ghcVersion }:
final: prev:

{
  "heronPackages-${ghcVersion}" =
    let

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
                owner = "cramsay";
                repo = "retroclash-lib";
                rev = "7835d15fadcd9c85b2ca630ca3d4e836cb2bfc9f";
                sha256 = "sha256:156df5mrgwkw90q3r9jiqws7r4p18hbfqkhk1kl0dh8whv5qnv8k";
              };
            in
            prev.haskell.lib.dontCheck (
              hprev.callCabal2nix "retroclash-lib" unmodified {}
            );

          # F-lite compiler
          flite =
            let
              unmodified = hprev.callCabal2nixWithOptions "flite" ../flite "" {};
            in
            unmodified.overrideAttrs (old: {
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
                  inherit (hfinal) flite retroclash-lib;
                };
            in
            unmodified.overrideAttrs (old: {

              # Depend on emulator (as reference during testing)
              nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
                hfinal.heron-emu
              ];

              # Generate verilog netlist during build
              postBuild = (old.postBuild or "") + ''
                dist/build/heron/heron --clash --verilog Heron.Core.Board -fclash-aggressive-x-optimization                          -package-db dist/package.conf.inplace -fclash-hdldir ./verilog
                dist/build/heron/heron --clash --verilog Heron.Core.Board -fclash-aggressive-x-optimization -fclash-force-undefined0 -package-db dist/package.conf.inplace -fclash-hdldir ./verilog_no_x
              '';
    
              # Copy verilog netlist to install folder
              postInstall = (old.postInstall or "") + ''
                mkdir -p "$out/share"
                cp -r "dist/doc/" "$out/share/doc"
                cp -r "verilog/" "$out/share/verilog"
                cp -r "verilog_no_x/" "$out/share/verilog_no_x"
              '';
            });
            
          heron-alveo =
            prev.stdenv.mkDerivation {
              name = "heron-alveo";
              src = ../vivado;
              buildInputs = [ hfinal.heron-clash prev.which ];
              impureEnvVars = [ "XILINX_VIVADO" ];
              doCheck = true;
            
              buildPhase = ''
                export HERON_VERILOG=${hfinal.heron-clash.outPath}/share/verilog/ ;
                export HOME=$TEMPDIR;
                export VIVADO_BIN=$XILINX_VIVADO/bin/vivado ;
                export PATH=$PATH:${builtins.getEnv "XILINX_VIVADO"}/bin ;
                ./check_env.sh &&
                (cd ./alveo_u280/ && vivado -mode batch -source synth.tcl);
              '';
            
              checkPhase = ''
                export HERON_VERILOG=${hfinal.heron-clash.outPath}/share/verilog/ ;
                export HOME=$TEMPDIR;
                export XILINX_VIVADO=${builtins.getEnv "XILINX_VIVADO"}; 
                export PATH=$PATH:${builtins.getEnv "XILINX_VIVADO"}/bin ;
                ./check_env.sh &&
                (cd ./sim/ && vivado -mode batch -source sim.tcl);
              '';
            
              installPhase = ''
                mkdir -p $out/share/heron/;
                # cp ./alveo_u280/heron_alveo.bit       $out/share/heron/;
                cp ./alveo_u280/post_route_prj.zip    $out/share/heron/;
                cp ./alveo_u280/post_route_util.rpt   $out/share/heron/;
                cp ./alveo_u280/post_route_timing.rpt $out/share/heron/;
              '';
            };

          heron-ultra96 =
            prev.stdenv.mkDerivation {
              name = "heron-ultra96";
              src = ../vivado;
              buildInputs = [ hfinal.heron-clash prev.which ];
              impureEnvVars = [ "XILINX_VIVADO" ];
              doCheck = true;
            
              buildPhase = ''
                export HERON_VERILOG=${hfinal.heron-clash.outPath}/share/verilog_no_x/ ;
                export HOME=$TEMPDIR;
                export VIVADO_BIN=$XILINX_VIVADO/bin/vivado ;
                export PATH=$PATH:${builtins.getEnv "XILINX_VIVADO"}/bin ;
                ./check_env.sh &&
                (cd ./ultra96_pynq/ && vivado -mode batch -source synth.tcl);
              '';
            
              checkPhase = ''
                export HERON_VERILOG=${hfinal.heron-clash.outPath}/share/verilog_no_x/ ;
                export HOME=$TEMPDIR;
                export XILINX_VIVADO=${builtins.getEnv "XILINX_VIVADO"}; 
                export PATH=$PATH:${builtins.getEnv "XILINX_VIVADO"}/bin ;
                ./check_env.sh &&
                (cd ./sim/ && vivado -mode batch -source sim.tcl);
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
    
        };
    in
    prev."clashPackages-${ghcVersion}".extend hOverlay;
}
