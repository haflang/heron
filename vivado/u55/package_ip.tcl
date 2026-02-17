# Create project
create_project siege ./siege -force -part xcu55c-fsvh2892-2L-e
# set_property board_part xilinx.com:au55c:part0:1.0 [current_project]

# Add sources
add_files $env(HERON_VERILOG)
add_files ./wrapper
set_property top siege_axi [current_fileset]
update_compile_order -fileset sources_1
add_files -fileset constrs_1 ./constrs.xdc
update_compile_order -fileset sources_1

# Package IP
ipx::package_project -root_dir ./ip_repo -vendor user.org -library user -taxonomy /UserIP -import_files

# IP Properties
set_property name siege_axi [ipx::current_core]
set_property display_name siege_axi [ipx::current_core]
set_property description siege_axi_v1_0_test [ipx::current_core]
set_property ipi_drc {ignore_freq_hz false} [ipx::current_core]
set_property sdx_kernel true [ipx::current_core]
set_property sdx_kernel_type rtl [ipx::current_core]
set_property vitis_drc {ctrl_protocol user_managed} [ipx::current_core]
set_property ipi_drc {ignore_freq_hz true} [ipx::current_core]
set_property supported_families {virtexuplus Production virtexuplusHBM Production zynquplus Production kintexu Production} [ipx::current_core]

# # Suggest 100MHz clock
# ipx::add_bus_parameter FREQ_HZ [ipx::get_bus_interfaces s_axi_control_ACLK -of_objects [ipx::current_core]]
# set_property value 100000000 [ipx::get_bus_parameters FREQ_HZ -of_objects [ipx::get_bus_interfaces s_axi_control_ACLK -of_objects [ipx::current_core]]]

# Define register map...
set_property name ctrl [ipx::get_address_blocks reg0 -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
set_property display_name ctrl [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
set_property description {Main control registers} [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]

ipx::add_register ctrl [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register global_interrupt_enable [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register ip_interrupt_enable [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register ip_interrupt_status [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register go [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register gc_threshold [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register ret_valid [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register ret_value [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register stats_mut [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register stats_root [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register stats_wait [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register stats_max_stall [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register stats_ctxt [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register code_addr [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register code_data0 [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register name_data1 [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register name_data2 [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register name_data3 [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register name_data4 [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register name_data5 [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register name_data6 [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register name_data7 [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register name_data8 [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register name_data9 [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]
ipx::add_register name_data10 [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]

set_property address_offset 0x4 [ipx::get_registers global_interrupt_enable -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x8 [ipx::get_registers ip_interrupt_enable -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0xC [ipx::get_registers ip_interrupt_status -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x10 [ipx::get_registers go -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x14 [ipx::get_registers gc_threshold -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x18 [ipx::get_registers ret_valid -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x1C [ipx::get_registers ret_value -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x20 [ipx::get_registers stats_mut -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x24 [ipx::get_registers stats_root -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x28 [ipx::get_registers stats_wait -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x2C [ipx::get_registers stats_max_stall -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x30 [ipx::get_registers stats_ctxt -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x34 [ipx::get_registers code_addr -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x38 [ipx::get_registers code_data0 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x3C [ipx::get_registers name_data1 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x40 [ipx::get_registers name_data2 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x44 [ipx::get_registers name_data3 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x48 [ipx::get_registers name_data4 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x4C [ipx::get_registers name_data5 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x50 [ipx::get_registers name_data6 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x54 [ipx::get_registers name_data7 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x58 [ipx::get_registers name_data8 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x5C [ipx::get_registers name_data9 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property address_offset 0x60 [ipx::get_registers name_data10 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]

set_property size 32 [ipx::get_registers ctrl -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers global_interrupt_enable -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers ip_interrupt_enable -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers ip_interrupt_status -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers go -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers gc_threshold -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers ret_valid -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers ret_value -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers stats_mut -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers stats_root -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers stats_wait -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers stats_max_stall -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers stats_ctxt -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers code_addr -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers code_data0 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers name_data1 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers name_data2 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers name_data3 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers name_data4 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers name_data5 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers name_data6 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers name_data7 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers name_data8 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers name_data9 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]
set_property size 32 [ipx::get_registers name_data10 -of_objects [ipx::get_address_blocks ctrl -of_objects [ipx::get_memory_maps s_axi_control -of_objects [ipx::current_core]]]]

# Generate files
set_property ip_repo_paths ./ip_repo [current_project]
ipx::add_bus_parameter FREQ_TOLERANCE_HZ [ipx::get_bus_interfaces s_axi_control_ACLK -of_objects [ipx::current_core]]
set_property value -1 [ipx::get_bus_parameters FREQ_TOLERANCE_HZ -of_objects [ipx::get_bus_interfaces s_axi_control_ACLK -of_objects [ipx::current_core]]]
set_property core_revision 2 [ipx::current_core]
ipx::create_xgui_files [ipx::current_core]
ipx::update_checksums [ipx::current_core]
ipx::check_integrity -kernel [ipx::current_core]
ipx::save_core [ipx::current_core]
package_xo -force -xo_path ./siege_axi.xo -kernel_name siege_axi -ip_directory ./ip_repo -ctrl_protocol user_managed
update_ip_catalog
