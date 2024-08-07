create_project prj . -force

# Project properties
set_property board_part avnet.com:ultra96v1:part0:1.2 [current_project]

# Setup run
set_property STEPS.SYNTH_DESIGN.ARGS.RETIMING true [get_runs synth_1]
set_property STEPS.SYNTH_DESIGN.ARGS.DIRECTIVE PerformanceOptimized [get_runs synth_1]
set_property STEPS.SYNTH_DESIGN.ARGS.MAX_BRAM_CASCADE_HEIGHT 1 [get_runs synth_1]
set_property STEPS.SYNTH_DESIGN.ARGS.MAX_URAM_CASCADE_HEIGHT 1 [get_runs synth_1]
set_property STEPS.OPT_DESIGN.ARGS.DIRECTIVE Explore [get_runs impl_1]
set_property STEPS.PLACE_DESIGN.ARGS.DIRECTIVE EarlyBlockPlacement [get_runs impl_1]
set_property STEPS.PHYS_OPT_DESIGN.ARGS.DIRECTIVE Explore [get_runs impl_1]
set_property STEPS.ROUTE_DESIGN.ARGS.DIRECTIVE NoTimingRelaxation [get_runs impl_1]
set_property STEPS.POST_ROUTE_PHYS_OPT_DESIGN.IS_ENABLED true [get_runs impl_1]
set_property STEPS.POST_ROUTE_PHYS_OPT_DESIGN.ARGS.DIRECTIVE AggressiveExplore [get_runs impl_1]

# Add sources
add_files $env(HERON_VERILOG)
set_property top topEntity [current_fileset]
update_compile_order -fileset sources_1
#add_files -fileset constrs_1 ./constrs.xdc
add_files -fileset utils_1 ./rqs

# Make an IP block from our filter sources
ipx::package_project -root_dir ./ip -vendor user.org -library user -taxonomy /UserIP -import_files -set_current false topEntity
#ipx::unload_core ./ip/component.xml
ipx::edit_ip_in_project -upgrade true -name tmp_edit_project -directory ./ip ./ip/component.xml
update_compile_order -fileset sources_1
set_property core_revision 1 [ipx::current_core]
ipx::create_xgui_files [ipx::current_core]
ipx::update_checksums [ipx::current_core]
ipx::save_core [ipx::current_core]
close_project -delete

# Include IP in main project
set_property  ip_repo_paths  ./ip [current_project]
update_ip_catalog
update_compile_order -fileset sources_1

# Generate block design
source block_design.tcl
update_compile_order -fileset sources_1
make_wrapper -files [get_files ./prj.srcs/sources_1/bd/design_1/design_1.bd] -top
add_files -norecurse ./prj.gen/sources_1/bd/design_1/hdl/design_1_wrapper.v
set_property top design_1_wrapper [current_fileset]
update_compile_order -fileset sources_1

# Synthesise
launch_runs impl_1 -jobs 2
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
launch_runs impl_1 -to_step write_bitstream -jobs 6
wait_on_run impl_1
exec cp ./prj.runs/impl_1/design_1_wrapper.bit heron_ultra96.bit
exec cp ./prj.gen/sources_1/bd/design_1/hw_handoff/design_1.hwh heron_ultra96.hwh

close_project
