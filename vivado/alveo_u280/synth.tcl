create_project prj . -force -part xcu280-fsvh2892-2L-e

#create_project prj . -force -part xcu55c-fsvh2892-2L-e

# Project properties
set_property board_part xilinx.com:au280:part0:1.1 [current_project]
#set_property board_part xilinx.com:au55c:part0:1.0 [current_project]

# Add sources
add_files $env(HERON_VERILOG)
set_property top topEntity [current_fileset]
update_compile_order -fileset sources_1
add_files -fileset constrs_1 ./constrs.xdc
set_property USED_IN {synthesis implementation} [get_files  ./constrs.xdc]

# Run clash's VIO IP instantiation script
package require fileutil
set tclIface ""
source [fileutil::findByPattern $env(HERON_VERILOG) topWithVIO_vioProbe_*.clash.tcl]
createIp $ipName

# Build floorplan constraints (needs tcl constructs not available in xdc)
source ./floorplan.tcl

# Setup run
set_property STEPS.SYNTH_DESIGN.ARGS.RETIMING true [get_runs synth_1]
#set_property STEPS.SYNTH_DESIGN.ARGS.DIRECTIVE PerformanceOptimized [get_runs synth_1]
set_property STEPS.SYNTH_DESIGN.ARGS.MAX_URAM_CASCADE_HEIGHT 1 [get_runs synth_1]
#set_property STEPS.SYNTH_DESIGN.ARGS.MAX_BRAM_CASCADE_HEIGHT 1 [get_runs synth_1]
set_property STEPS.SYNTH_DESIGN.ARGS.DIRECTIVE AreaOptimized_high [get_runs synth_1]
set_property STEPS.OPT_DESIGN.IS_ENABLED true [get_runs impl_1]
set_property STEPS.PHYS_OPT_DESIGN.IS_ENABLED true [get_runs impl_1]
#set_property STEPS.POST_ROUTE_PHYS_OPT_DESIGN.ARGS.DIRECTIVE AggressiveExplore [get_runs impl_1]

# Synthesise
set_param general.maxThreads 1
launch_runs impl_1 -jobs 1
wait_on_run impl_1

# Report
open_run impl_1
report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose -max_paths 100 -input_pins -routable_nets -name timing_1 -file ./post_route_timing.rpt
report_utilization -file ./post_route_util.rpt
archive_project -force -include_config_settings post_route_prj.zip

# Write bitstream
#write_bitstream heron_alveo.bit

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

close_project
