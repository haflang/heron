create_project prj . -force -part xcu280-fsvh2892-2L-e

# Project properties
set_property board_part xilinx.com:au280:part0:1.1 [current_project]

# Add sources
add_files $env(HERON_VERILOG)
set_property top topEntity [current_fileset]
update_compile_order -fileset sources_1
add_files -fileset constrs_1 $env(HERON_VERILOG)/Heron.Core.Board.topEntity/topEntity.sdc

# Synthesise
launch_runs impl_1 -jobs 6
wait_on_run impl_1

# Report
open_run impl_1
report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose -max_paths 10 -input_pins -routable_nets -name timing_1 -file ./post_route_timing.rpt
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
