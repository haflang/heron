package require fileutil

# Parse verilog sources
foreach file [fileutil::findByPattern $env(HERON_VERILOG) *.v] {
    exec xvlog $file
}

# Elaborate
set unisim_dir $env(XILINX_VIVADO)/data/verilog/src/unisims/
exec xvlog $env(XILINX_VIVADO)/data/verilog/src/glbl.v
exec xvlog $env(XILINX_VIVADO)/data/verilog/src/unisims/BUFGCE_DIV.v
exec xelab -debug typical -top testBench work.glbl -snapshot tb_snapshot

# Simulate
exec xsim tb_snapshot -tclbatch sim_cfg.tcl

#exec xsim --gui tb_snapshot.wdb

if [catch {exec grep -c "finish called at time" xsim.log} isFinished] {
    error "FAILURE: XSim did not finish the simulation"
} else {
    if [catch {exec grep -c "outputVerifier, expected" xsim.log} numErrors] {
        puts "### Simulation success!"
    } else {
        error "FAILURE: Assertions raised ${numErrors} errors"
    }
}
