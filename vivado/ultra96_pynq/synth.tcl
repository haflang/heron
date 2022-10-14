create_project prj . -force

# Project properties
set_property board_part em.avnet.com:ultra96v2:part0:1.0 [current_project]

# Add sources
add_files $env(HERON_VERILOG)
update_compile_order -fileset sources_1
set_property top topEntity [current_fileset]

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
launch_runs impl_1 -jobs 6
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
