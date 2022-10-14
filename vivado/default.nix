{ self
, pkgs
}:

pkgs.stdenv.mkDerivation {
  name = "heron_vivado";
  src = self;
  buildInputs = [ pkgs.haskellPackages.heron ];

  buildPhase = ''
    export HERON_VERILOG=${pkgs.haskellPackages.heron.outPath}/share/verilog/ ;
    export VIVADO_BIN=$XILINX_VIVADO/bin/vivado ;
    ./vivado/check_env.sh &&
    (cd ./vivado/alveo_u280/ && $VIVADO_BIN -mode batch -source synth.tcl);
  '';

  testPhase = ''
    export HERON_VERILOG=${pkgs.haskellPackages.heron.outPath}/share/verilog/ ;
    export VIVADO_BIN=$XILINX_VIVADO/bin/vivado ;
    ./vivado/check_env.sh &&
    (cd ./vivado/sim/ && $VIVADO_BIN -mode batch -source sim.tcl);
  '';

  installPhase = ''
    mkdir -p $out/share/heron/;
    cp ./vivado/alveo_u280/post_route.dcp        $out/share/heron/
    cp ./vivado/alveo_u280/post_route_util.rpt   $out/share/heron/
    cp ./vivado/alveo_u280/post_route_timing.rpt $out/share/heron/
  '';
}
