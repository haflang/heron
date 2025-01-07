create_project prj . -force

# Project properties
set_property board_part tul.com.tw:pynq-z2:1.0 [current_project]

# Setup run
set_property STEPS.SYNTH_DESIGN.ARGS.MAX_BRAM_CASCADE_HEIGHT 1 [get_runs synth_1]
set_property STEPS.PLACE_DESIGN.ARGS.DIRECTIVE EarlyBlockPlacement [get_runs impl_1]

# Add sources
add_files -scan_for_includes $env(HERON_VERILOG)
add_files wrapper.v
set_property top wrapper [current_fileset]
update_compile_order -fileset sources_1

# Run clash's VIO IP instantiation script
package require fileutil
set tclIface ""
source [fileutil::findByPattern $env(HERON_VERILOG) topWithVIO_vioProbe_*.clash.tcl]
createIp $ipName

# Instantiate default MPSoC IP to get a 100MHz clock
create_ip -name processing_system7 \
          -vendor xilinx.com \
          -library ip \
          -version 5.5 \
          -module_name zynq_ps
set_property -dict [list \
  CONFIG.PCW_UIPARAM_DDR_ENABLE {0} \
  CONFIG.PCW_USE_M_AXI_GP0 {0} \
] [get_ips zynq_ps]

# Synthesise
launch_runs impl_1 -jobs 3
wait_on_run impl_1

# Report
open_run impl_1
report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose -max_paths 10 -input_pins -routable_nets -name timing_1 -file ./post_route_timing.rpt
report_utilization -file ./post_route_util.rpt
archive_project -force -include_config_settings post_route_prj.zip

# Check timing
set fd [open ./post_route_timing.rpt r]
set timing_met 0
while { [gets $fd line] >= 0 } {
    if [string match {All user specified timing constraints are met.} $line]  {
        set timing_met 1
        break
    }
}
if {$timing_met == 0} {
    puts "ERROR: bitstream generation does not meet timing."
    exit 1
}
puts "Timing constraints are met."

# Generate PYNQ files
launch_runs impl_1 -to_step write_bitstream -jobs 1
wait_on_run impl_1
exec cp ./prj.runs/impl_1/wrapper.bit heron_pynqz2_vio.bit
exec cp ./prj.runs/impl_1/wrapper.ltx heron_pynqz2_vio.ltx

close_project
