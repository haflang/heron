
################################################################
# This is a generated script based on design: design_1
#
# Though there are limitations about the generated script,
# the main purpose of this utility is to make learning
# IP Integrator Tcl commands easier.
################################################################

namespace eval _tcl {
proc get_script_folder {} {
   set script_path [file normalize [info script]]
   set script_folder [file dirname $script_path]
   return $script_folder
}
}
variable script_folder
set script_folder [_tcl::get_script_folder]

################################################################
# Check if script is running in correct Vivado version.
################################################################
set scripts_vivado_version 2023.1
set current_vivado_version [version -short]

if { [string first $scripts_vivado_version $current_vivado_version] == -1 } {
   puts ""
   catch {common::send_gid_msg -ssname BD::TCL -id 2041 -severity "ERROR" "This script was generated using Vivado <$scripts_vivado_version> and is being run in <$current_vivado_version> of Vivado. Please run the script in Vivado <$scripts_vivado_version> then open the design in Vivado <$current_vivado_version>. Upgrade the design by running \"Tools => Report => Report IP Status...\", then run write_bd_tcl to create an updated script."}

   return 1
}

################################################################
# START
################################################################

# To test this script, run the following commands from Vivado Tcl console:
# source design_1_script.tcl

# If there is no project opened, this script will create a
# project, but make sure you do not have an existing project
# <./myproj/project_1.xpr> in the current working folder.

set list_projs [get_projects -quiet]
if { $list_projs eq "" } {
   create_project project_1 myproj -part xczu3eg-sbva484-1-e
   set_property BOARD_PART avnet.com:ultra96v1:part0:1.2 [current_project]
}


# CHANGE DESIGN NAME HERE
variable design_name
set design_name design_1

# If you do not already have an existing IP Integrator design open,
# you can create a design using the following command:
#    create_bd_design $design_name

# Creating design if needed
set errMsg ""
set nRet 0

set cur_design [current_bd_design -quiet]
set list_cells [get_bd_cells -quiet]

if { ${design_name} eq "" } {
   # USE CASES:
   #    1) Design_name not set

   set errMsg "Please set the variable <design_name> to a non-empty value."
   set nRet 1

} elseif { ${cur_design} ne "" && ${list_cells} eq "" } {
   # USE CASES:
   #    2): Current design opened AND is empty AND names same.
   #    3): Current design opened AND is empty AND names diff; design_name NOT in project.
   #    4): Current design opened AND is empty AND names diff; design_name exists in project.

   if { $cur_design ne $design_name } {
      common::send_gid_msg -ssname BD::TCL -id 2001 -severity "INFO" "Changing value of <design_name> from <$design_name> to <$cur_design> since current design is empty."
      set design_name [get_property NAME $cur_design]
   }
   common::send_gid_msg -ssname BD::TCL -id 2002 -severity "INFO" "Constructing design in IPI design <$cur_design>..."

} elseif { ${cur_design} ne "" && $list_cells ne "" && $cur_design eq $design_name } {
   # USE CASES:
   #    5) Current design opened AND has components AND same names.

   set errMsg "Design <$design_name> already exists in your project, please set the variable <design_name> to another value."
   set nRet 1
} elseif { [get_files -quiet ${design_name}.bd] ne "" } {
   # USE CASES: 
   #    6) Current opened design, has components, but diff names, design_name exists in project.
   #    7) No opened design, design_name exists in project.

   set errMsg "Design <$design_name> already exists in your project, please set the variable <design_name> to another value."
   set nRet 2

} else {
   # USE CASES:
   #    8) No opened design, design_name not in project.
   #    9) Current opened design, has components, but diff names, design_name not in project.

   common::send_gid_msg -ssname BD::TCL -id 2003 -severity "INFO" "Currently there is no design <$design_name> in project, so creating one..."

   create_bd_design $design_name

   common::send_gid_msg -ssname BD::TCL -id 2004 -severity "INFO" "Making design <$design_name> as current_bd_design."
   current_bd_design $design_name

}

common::send_gid_msg -ssname BD::TCL -id 2005 -severity "INFO" "Currently the variable <design_name> is equal to \"$design_name\"."

if { $nRet != 0 } {
   catch {common::send_gid_msg -ssname BD::TCL -id 2006 -severity "ERROR" $errMsg}
   return $nRet
}

set bCheckIPsPassed 1
##################################################################
# CHECK IPs
##################################################################
set bCheckIPs 1
if { $bCheckIPs == 1 } {
   set list_check_ips "\ 
xilinx.com:ip:zynq_ultra_ps_e:3.5\
user.org:user:topEntity:1.0\
xilinx.com:ip:proc_sys_reset:5.0\
xilinx.com:ip:xlconstant:1.1\
xilinx.com:ip:axi_dma:7.1\
xilinx.com:ip:smartconnect:1.0\
xilinx.com:ip:xlconcat:2.1\
xilinx.com:ip:util_reduced_logic:2.0\
xilinx.com:ip:axi_gpio:2.0\
xilinx.com:ip:xlslice:1.0\
"

   set list_ips_missing ""
   common::send_gid_msg -ssname BD::TCL -id 2011 -severity "INFO" "Checking if the following IPs exist in the project's IP catalog: $list_check_ips ."

   foreach ip_vlnv $list_check_ips {
      set ip_obj [get_ipdefs -all $ip_vlnv]
      if { $ip_obj eq "" } {
         lappend list_ips_missing $ip_vlnv
      }
   }

   if { $list_ips_missing ne "" } {
      catch {common::send_gid_msg -ssname BD::TCL -id 2012 -severity "ERROR" "The following IPs are not found in the IP Catalog:\n  $list_ips_missing\n\nResolution: Please add the repository containing the IP(s) to the project." }
      set bCheckIPsPassed 0
   }

}

if { $bCheckIPsPassed != 1 } {
  common::send_gid_msg -ssname BD::TCL -id 2023 -severity "WARNING" "Will not continue with creation of design due to the error(s) above."
  return 3
}

##################################################################
# DESIGN PROCs
##################################################################


# Hierarchical cell: hier_joiner
proc create_hier_cell_hier_joiner { parentCell nameHier } {

  variable script_folder

  if { $parentCell eq "" || $nameHier eq "" } {
     catch {common::send_gid_msg -ssname BD::TCL -id 2092 -severity "ERROR" "create_hier_cell_hier_joiner() - Empty argument(s)!"}
     return
  }

  # Get object for parentCell
  set parentObj [get_bd_cells $parentCell]
  if { $parentObj == "" } {
     catch {common::send_gid_msg -ssname BD::TCL -id 2090 -severity "ERROR" "Unable to find parent cell <$parentCell>!"}
     return
  }

  # Make sure parentObj is hier blk
  set parentType [get_property TYPE $parentObj]
  if { $parentType ne "hier" } {
     catch {common::send_gid_msg -ssname BD::TCL -id 2091 -severity "ERROR" "Parent <$parentObj> has TYPE = <$parentType>. Expected to be <hier>."}
     return
  }

  # Save current instance; Restore later
  set oldCurInst [current_bd_instance .]

  # Set parent object as current
  current_bd_instance $parentObj

  # Create cell and set as current instance
  set hier_obj [create_bd_cell -type hier $nameHier]
  current_bd_instance $hier_obj

  # Create interface pins

  # Create pins
  create_bd_pin -dir I -from 18 -to 0 In0
  create_bd_pin -dir I -from 127 -to 0 In1
  create_bd_pin -dir O -from 255 -to 0 dout

  # Create instance: const_pad, and set properties
  set const_pad [ create_bd_cell -type ip -vlnv xilinx.com:ip:xlconstant:1.1 const_pad ]
  set_property -dict [list \
    CONFIG.CONST_VAL {0} \
    CONFIG.CONST_WIDTH {109} \
  ] $const_pad


  # Create instance: concat, and set properties
  set concat [ create_bd_cell -type ip -vlnv xilinx.com:ip:xlconcat:2.1 concat ]
  set_property -dict [list \
    CONFIG.IN0_WIDTH {19} \
    CONFIG.IN1_WIDTH {128} \
    CONFIG.IN2_WIDTH {109} \
    CONFIG.NUM_PORTS {3} \
  ] $concat


  # Create port connections
  connect_bd_net -net concat_dout [get_bd_pins concat/dout] [get_bd_pins dout]
  connect_bd_net -net const_pad_dout [get_bd_pins const_pad/dout] [get_bd_pins concat/In2]
  connect_bd_net -net topEntity_0_ret [get_bd_pins In0] [get_bd_pins concat/In0]
  connect_bd_net -net topEntity_0_stats [get_bd_pins In1] [get_bd_pins concat/In1]

  # Restore current instance
  current_bd_instance $oldCurInst
}

# Hierarchical cell: hier_splitter
proc create_hier_cell_hier_splitter { parentCell nameHier } {

  variable script_folder

  if { $parentCell eq "" || $nameHier eq "" } {
     catch {common::send_gid_msg -ssname BD::TCL -id 2092 -severity "ERROR" "create_hier_cell_hier_splitter() - Empty argument(s)!"}
     return
  }

  # Get object for parentCell
  set parentObj [get_bd_cells $parentCell]
  if { $parentObj == "" } {
     catch {common::send_gid_msg -ssname BD::TCL -id 2090 -severity "ERROR" "Unable to find parent cell <$parentCell>!"}
     return
  }

  # Make sure parentObj is hier blk
  set parentType [get_property TYPE $parentObj]
  if { $parentType ne "hier" } {
     catch {common::send_gid_msg -ssname BD::TCL -id 2091 -severity "ERROR" "Parent <$parentObj> has TYPE = <$parentType>. Expected to be <hier>."}
     return
  }

  # Save current instance; Restore later
  set oldCurInst [current_bd_instance .]

  # Set parent object as current
  current_bd_instance $parentObj

  # Create cell and set as current instance
  set hier_obj [create_bd_cell -type hier $nameHier]
  current_bd_instance $hier_obj

  # Create interface pins

  # Create pins
  create_bd_pin -dir I -from 0 -to 0 In1
  create_bd_pin -dir I -from 511 -to 0 Din
  create_bd_pin -dir O -from 9 -to 0 addr
  create_bd_pin -dir O wen
  create_bd_pin -dir O go
  create_bd_pin -dir O -from 333 -to 0 code_data

  # Create instance: concat_gate_go, and set properties
  set concat_gate_go [ create_bd_cell -type ip -vlnv xilinx.com:ip:xlconcat:2.1 concat_gate_go ]

  # Create instance: slice_wen, and set properties
  set slice_wen [ create_bd_cell -type ip -vlnv xilinx.com:ip:xlslice:1.0 slice_wen ]
  set_property -dict [list \
    CONFIG.DIN_FROM {1} \
    CONFIG.DIN_TO {1} \
    CONFIG.DIN_WIDTH {512} \
  ] $slice_wen


  # Create instance: concat_gate_wen, and set properties
  set concat_gate_wen [ create_bd_cell -type ip -vlnv xilinx.com:ip:xlconcat:2.1 concat_gate_wen ]

  # Create instance: slice_addr, and set properties
  set slice_addr [ create_bd_cell -type ip -vlnv xilinx.com:ip:xlslice:1.0 slice_addr ]
  set_property -dict [list \
    CONFIG.DIN_FROM {11} \
    CONFIG.DIN_TO {2} \
    CONFIG.DIN_WIDTH {512} \
  ] $slice_addr


  # Create instance: slice_data, and set properties
  set slice_data [ create_bd_cell -type ip -vlnv xilinx.com:ip:xlslice:1.0 slice_data ]
  set_property -dict [list \
    CONFIG.DIN_FROM {345} \
    CONFIG.DIN_TO {12} \
    CONFIG.DIN_WIDTH {512} \
    CONFIG.DOUT_WIDTH {334} \
  ] $slice_data


  # Create instance: gate_wen, and set properties
  set gate_wen [ create_bd_cell -type ip -vlnv xilinx.com:ip:util_reduced_logic:2.0 gate_wen ]
  set_property -dict [list \
    CONFIG.C_OPERATION {and} \
    CONFIG.C_SIZE {2} \
  ] $gate_wen


  # Create instance: slice_go, and set properties
  set slice_go [ create_bd_cell -type ip -vlnv xilinx.com:ip:xlslice:1.0 slice_go ]
  set_property CONFIG.DIN_WIDTH {512} $slice_go


  # Create instance: gate_go, and set properties
  set gate_go [ create_bd_cell -type ip -vlnv xilinx.com:ip:util_reduced_logic:2.0 gate_go ]
  set_property -dict [list \
    CONFIG.C_OPERATION {and} \
    CONFIG.C_SIZE {2} \
  ] $gate_go


  # Create port connections
  connect_bd_net -net axi_dma_0_m_axis_mm2s_tdata [get_bd_pins Din] [get_bd_pins slice_addr/Din] [get_bd_pins slice_data/Din] [get_bd_pins slice_go/Din] [get_bd_pins slice_wen/Din]
  connect_bd_net -net axi_dma_0_m_axis_mm2s_tvalid [get_bd_pins In1] [get_bd_pins concat_gate_wen/In1] [get_bd_pins concat_gate_go/In1]
  connect_bd_net -net concat_gate_wen_dout [get_bd_pins concat_gate_wen/dout] [get_bd_pins gate_wen/Op1]
  connect_bd_net -net gate_go_Res [get_bd_pins gate_go/Res] [get_bd_pins go]
  connect_bd_net -net gate_wen_Res [get_bd_pins gate_wen/Res] [get_bd_pins wen]
  connect_bd_net -net slice_addr_Dout [get_bd_pins slice_addr/Dout] [get_bd_pins addr]
  connect_bd_net -net slice_data_Dout [get_bd_pins slice_data/Dout] [get_bd_pins code_data]
  connect_bd_net -net slice_go_Dout [get_bd_pins slice_go/Dout] [get_bd_pins concat_gate_go/In0]
  connect_bd_net -net slice_wen_Dout [get_bd_pins slice_wen/Dout] [get_bd_pins concat_gate_wen/In0]
  connect_bd_net -net xlconcat_0_dout [get_bd_pins concat_gate_go/dout] [get_bd_pins gate_go/Op1]

  # Restore current instance
  current_bd_instance $oldCurInst
}


# Procedure to create entire design; Provide argument to make
# procedure reusable. If parentCell is "", will use root.
proc create_root_design { parentCell } {

  variable script_folder
  variable design_name

  if { $parentCell eq "" } {
     set parentCell [get_bd_cells /]
  }

  # Get object for parentCell
  set parentObj [get_bd_cells $parentCell]
  if { $parentObj == "" } {
     catch {common::send_gid_msg -ssname BD::TCL -id 2090 -severity "ERROR" "Unable to find parent cell <$parentCell>!"}
     return
  }

  # Make sure parentObj is hier blk
  set parentType [get_property TYPE $parentObj]
  if { $parentType ne "hier" } {
     catch {common::send_gid_msg -ssname BD::TCL -id 2091 -severity "ERROR" "Parent <$parentObj> has TYPE = <$parentType>. Expected to be <hier>."}
     return
  }

  # Save current instance; Restore later
  set oldCurInst [current_bd_instance .]

  # Set parent object as current
  current_bd_instance $parentObj


  # Create interface ports

  # Create ports

  # Create instance: zynq_ultra_ps_e_0, and set properties
  set zynq_ultra_ps_e_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:zynq_ultra_ps_e:3.5 zynq_ultra_ps_e_0 ]
  set_property -dict [list \
    CONFIG.PSU_BANK_0_IO_STANDARD {LVCMOS18} \
    CONFIG.PSU_BANK_1_IO_STANDARD {LVCMOS18} \
    CONFIG.PSU_BANK_2_IO_STANDARD {LVCMOS18} \
    CONFIG.PSU_BANK_3_IO_STANDARD {LVCMOS18} \
    CONFIG.PSU_DDR_RAM_HIGHADDR {0x7FFFFFFF} \
    CONFIG.PSU_DDR_RAM_HIGHADDR_OFFSET {0x00000002} \
    CONFIG.PSU_DDR_RAM_LOWADDR_OFFSET {0x80000000} \
    CONFIG.PSU_MIO_12_POLARITY {Default} \
    CONFIG.PSU_MIO_13_DRIVE_STRENGTH {4} \
    CONFIG.PSU_MIO_14_DRIVE_STRENGTH {4} \
    CONFIG.PSU_MIO_15_DRIVE_STRENGTH {4} \
    CONFIG.PSU_MIO_16_DRIVE_STRENGTH {4} \
    CONFIG.PSU_MIO_17_POLARITY {Default} \
    CONFIG.PSU_MIO_18_POLARITY {Default} \
    CONFIG.PSU_MIO_19_POLARITY {Default} \
    CONFIG.PSU_MIO_20_POLARITY {Default} \
    CONFIG.PSU_MIO_21_DRIVE_STRENGTH {4} \
    CONFIG.PSU_MIO_22_DRIVE_STRENGTH {4} \
    CONFIG.PSU_MIO_23_POLARITY {Default} \
    CONFIG.PSU_MIO_25_POLARITY {Default} \
    CONFIG.PSU_MIO_31_POLARITY {Default} \
    CONFIG.PSU_MIO_35_POLARITY {Default} \
    CONFIG.PSU_MIO_36_POLARITY {Default} \
    CONFIG.PSU_MIO_37_POLARITY {Default} \
    CONFIG.PSU_MIO_39_DRIVE_STRENGTH {12} \
    CONFIG.PSU_MIO_39_POLARITY {Default} \
    CONFIG.PSU_MIO_39_SLEW {fast} \
    CONFIG.PSU_MIO_40_POLARITY {Default} \
    CONFIG.PSU_MIO_44_POLARITY {Default} \
    CONFIG.PSU_MIO_45_POLARITY {Default} \
    CONFIG.PSU_MIO_76_POLARITY {Default} \
    CONFIG.PSU_MIO_77_POLARITY {Default} \
    CONFIG.PSU_MIO_7_POLARITY {Default} \
    CONFIG.PSU_MIO_8_POLARITY {Default} \
    CONFIG.PSU_MIO_TREE_PERIPHERALS {UART 1#UART 1#UART 0#UART 0#I2C 1#I2C 1#SPI 1#GPIO0 MIO#GPIO0 MIO#SPI 1#SPI 1#SPI 1#GPIO0 MIO#SD 0#SD 0#SD 0#SD 0#GPIO0 MIO#GPIO0 MIO#GPIO0 MIO#GPIO0 MIO#SD 0#SD 0#GPIO0\
MIO#SD 0#GPIO0 MIO#PMU GPI 0#DPAUX#DPAUX#DPAUX#DPAUX#GPIO1 MIO#PMU GPO 0#PMU GPO 1#PMU GPO 2#GPIO1 MIO#GPIO1 MIO#GPIO1 MIO#SPI 0#GPIO1 MIO#GPIO1 MIO#SPI 0#SPI 0#SPI 0#GPIO1 MIO#GPIO1 MIO#SD 1#SD 1#SD 1#SD\
1#SD 1#SD 1#USB 0#USB 0#USB 0#USB 0#USB 0#USB 0#USB 0#USB 0#USB 0#USB 0#USB 0#USB 0#USB 1#USB 1#USB 1#USB 1#USB 1#USB 1#USB 1#USB 1#USB 1#USB 1#USB 1#USB 1#GPIO2 MIO#GPIO2 MIO} \
    CONFIG.PSU_MIO_TREE_SIGNALS {txd#rxd#rxd#txd#scl_out#sda_out#sclk_out#gpio0[7]#gpio0[8]#n_ss_out[0]#miso#mosi#gpio0[12]#sdio0_data_out[0]#sdio0_data_out[1]#sdio0_data_out[2]#sdio0_data_out[3]#gpio0[17]#gpio0[18]#gpio0[19]#gpio0[20]#sdio0_cmd_out#sdio0_clk_out#gpio0[23]#sdio0_cd_n#gpio0[25]#gpi[0]#dp_aux_data_out#dp_hot_plug_detect#dp_aux_data_oe#dp_aux_data_in#gpio1[31]#gpo[0]#gpo[1]#gpo[2]#gpio1[35]#gpio1[36]#gpio1[37]#sclk_out#gpio1[39]#gpio1[40]#n_ss_out[0]#miso#mosi#gpio1[44]#gpio1[45]#sdio1_data_out[0]#sdio1_data_out[1]#sdio1_data_out[2]#sdio1_data_out[3]#sdio1_cmd_out#sdio1_clk_out#ulpi_clk_in#ulpi_dir#ulpi_tx_data[2]#ulpi_nxt#ulpi_tx_data[0]#ulpi_tx_data[1]#ulpi_stp#ulpi_tx_data[3]#ulpi_tx_data[4]#ulpi_tx_data[5]#ulpi_tx_data[6]#ulpi_tx_data[7]#ulpi_clk_in#ulpi_dir#ulpi_tx_data[2]#ulpi_nxt#ulpi_tx_data[0]#ulpi_tx_data[1]#ulpi_stp#ulpi_tx_data[3]#ulpi_tx_data[4]#ulpi_tx_data[5]#ulpi_tx_data[6]#ulpi_tx_data[7]#gpio2[76]#gpio2[77]}\
\
    CONFIG.PSU_SD0_INTERNAL_BUS_WIDTH {4} \
    CONFIG.PSU_SD1_INTERNAL_BUS_WIDTH {4} \
    CONFIG.PSU_USB3__DUAL_CLOCK_ENABLE {1} \
    CONFIG.PSU__ACT_DDR_FREQ_MHZ {533.333313} \
    CONFIG.PSU__AFI0_COHERENCY {0} \
    CONFIG.PSU__CAN1__PERIPHERAL__ENABLE {0} \
    CONFIG.PSU__CRF_APB__ACPU_CTRL__ACT_FREQMHZ {1200.000000} \
    CONFIG.PSU__CRF_APB__ACPU_CTRL__DIVISOR0 {1} \
    CONFIG.PSU__CRF_APB__ACPU_CTRL__SRCSEL {APLL} \
    CONFIG.PSU__CRF_APB__APLL_CTRL__DIV2 {1} \
    CONFIG.PSU__CRF_APB__APLL_CTRL__FBDIV {72} \
    CONFIG.PSU__CRF_APB__APLL_CTRL__SRCSEL {PSS_REF_CLK} \
    CONFIG.PSU__CRF_APB__APLL_TO_LPD_CTRL__DIVISOR0 {3} \
    CONFIG.PSU__CRF_APB__DBG_FPD_CTRL__ACT_FREQMHZ {250.000000} \
    CONFIG.PSU__CRF_APB__DBG_FPD_CTRL__DIVISOR0 {2} \
    CONFIG.PSU__CRF_APB__DBG_FPD_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRF_APB__DBG_TRACE_CTRL__DIVISOR0 {2} \
    CONFIG.PSU__CRF_APB__DBG_TRACE_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRF_APB__DBG_TSTMP_CTRL__ACT_FREQMHZ {250.000000} \
    CONFIG.PSU__CRF_APB__DBG_TSTMP_CTRL__DIVISOR0 {2} \
    CONFIG.PSU__CRF_APB__DBG_TSTMP_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRF_APB__DDR_CTRL__ACT_FREQMHZ {266.666656} \
    CONFIG.PSU__CRF_APB__DDR_CTRL__DIVISOR0 {4} \
    CONFIG.PSU__CRF_APB__DDR_CTRL__FREQMHZ {533} \
    CONFIG.PSU__CRF_APB__DDR_CTRL__SRCSEL {DPLL} \
    CONFIG.PSU__CRF_APB__DPDMA_REF_CTRL__ACT_FREQMHZ {600.000000} \
    CONFIG.PSU__CRF_APB__DPDMA_REF_CTRL__DIVISOR0 {2} \
    CONFIG.PSU__CRF_APB__DPDMA_REF_CTRL__SRCSEL {APLL} \
    CONFIG.PSU__CRF_APB__DPLL_CTRL__DIV2 {1} \
    CONFIG.PSU__CRF_APB__DPLL_CTRL__FBDIV {64} \
    CONFIG.PSU__CRF_APB__DPLL_CTRL__SRCSEL {PSS_REF_CLK} \
    CONFIG.PSU__CRF_APB__DPLL_TO_LPD_CTRL__DIVISOR0 {3} \
    CONFIG.PSU__CRF_APB__DP_AUDIO_REF_CTRL__ACT_FREQMHZ {24.576040} \
    CONFIG.PSU__CRF_APB__DP_AUDIO_REF_CTRL__DIVISOR0 {16} \
    CONFIG.PSU__CRF_APB__DP_AUDIO_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRF_APB__DP_AUDIO_REF_CTRL__SRCSEL {RPLL} \
    CONFIG.PSU__CRF_APB__DP_AUDIO__FRAC_ENABLED {1} \
    CONFIG.PSU__CRF_APB__DP_STC_REF_CTRL__ACT_FREQMHZ {26.214443} \
    CONFIG.PSU__CRF_APB__DP_STC_REF_CTRL__DIVISOR0 {15} \
    CONFIG.PSU__CRF_APB__DP_STC_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRF_APB__DP_STC_REF_CTRL__SRCSEL {RPLL} \
    CONFIG.PSU__CRF_APB__DP_VIDEO_REF_CTRL__ACT_FREQMHZ {297.029572} \
    CONFIG.PSU__CRF_APB__DP_VIDEO_REF_CTRL__DIVISOR0 {4} \
    CONFIG.PSU__CRF_APB__DP_VIDEO_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRF_APB__DP_VIDEO_REF_CTRL__SRCSEL {VPLL} \
    CONFIG.PSU__CRF_APB__DP_VIDEO__FRAC_ENABLED {1} \
    CONFIG.PSU__CRF_APB__GDMA_REF_CTRL__ACT_FREQMHZ {600.000000} \
    CONFIG.PSU__CRF_APB__GDMA_REF_CTRL__DIVISOR0 {2} \
    CONFIG.PSU__CRF_APB__GDMA_REF_CTRL__SRCSEL {APLL} \
    CONFIG.PSU__CRF_APB__GPU_REF_CTRL__ACT_FREQMHZ {500.000000} \
    CONFIG.PSU__CRF_APB__GPU_REF_CTRL__DIVISOR0 {1} \
    CONFIG.PSU__CRF_APB__GPU_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRF_APB__PCIE_REF_CTRL__DIVISOR0 {2} \
    CONFIG.PSU__CRF_APB__SATA_REF_CTRL__DIVISOR0 {2} \
    CONFIG.PSU__CRF_APB__TOPSW_LSBUS_CTRL__ACT_FREQMHZ {100.000000} \
    CONFIG.PSU__CRF_APB__TOPSW_LSBUS_CTRL__DIVISOR0 {5} \
    CONFIG.PSU__CRF_APB__TOPSW_LSBUS_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRF_APB__TOPSW_MAIN_CTRL__ACT_FREQMHZ {533.333313} \
    CONFIG.PSU__CRF_APB__TOPSW_MAIN_CTRL__DIVISOR0 {2} \
    CONFIG.PSU__CRF_APB__TOPSW_MAIN_CTRL__SRCSEL {DPLL} \
    CONFIG.PSU__CRF_APB__VPLL_CTRL__DIV2 {1} \
    CONFIG.PSU__CRF_APB__VPLL_CTRL__FBDIV {71} \
    CONFIG.PSU__CRF_APB__VPLL_CTRL__FRACDATA {0.2871} \
    CONFIG.PSU__CRF_APB__VPLL_CTRL__FRACFREQ {300} \
    CONFIG.PSU__CRF_APB__VPLL_CTRL__SRCSEL {PSS_REF_CLK} \
    CONFIG.PSU__CRF_APB__VPLL_FRAC_CFG__ENABLED {1} \
    CONFIG.PSU__CRF_APB__VPLL_TO_LPD_CTRL__DIVISOR0 {3} \
    CONFIG.PSU__CRL_APB__ADMA_REF_CTRL__ACT_FREQMHZ {500.000000} \
    CONFIG.PSU__CRL_APB__ADMA_REF_CTRL__DIVISOR0 {3} \
    CONFIG.PSU__CRL_APB__ADMA_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__AFI6_REF_CTRL__DIVISOR0 {3} \
    CONFIG.PSU__CRL_APB__AMS_REF_CTRL__ACT_FREQMHZ {51.724136} \
    CONFIG.PSU__CRL_APB__AMS_REF_CTRL__DIVISOR0 {29} \
    CONFIG.PSU__CRL_APB__AMS_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__AMS_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__CAN0_REF_CTRL__DIVISOR0 {15} \
    CONFIG.PSU__CRL_APB__CAN0_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__CAN0_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__CAN1_REF_CTRL__DIVISOR0 {15} \
    CONFIG.PSU__CRL_APB__CAN1_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__CAN1_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__CPU_R5_CTRL__ACT_FREQMHZ {500.000000} \
    CONFIG.PSU__CRL_APB__CPU_R5_CTRL__DIVISOR0 {3} \
    CONFIG.PSU__CRL_APB__CPU_R5_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__CSU_PLL_CTRL__ACT_FREQMHZ {500} \
    CONFIG.PSU__CRL_APB__CSU_PLL_CTRL__DIVISOR0 {4} \
    CONFIG.PSU__CRL_APB__CSU_PLL_CTRL__FREQMHZ {400} \
    CONFIG.PSU__CRL_APB__CSU_PLL_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__DBG_LPD_CTRL__ACT_FREQMHZ {250.000000} \
    CONFIG.PSU__CRL_APB__DBG_LPD_CTRL__DIVISOR0 {6} \
    CONFIG.PSU__CRL_APB__DBG_LPD_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__DLL_REF_CTRL__ACT_FREQMHZ {1500.000000} \
    CONFIG.PSU__CRL_APB__GEM0_REF_CTRL__DIVISOR0 {12} \
    CONFIG.PSU__CRL_APB__GEM0_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__GEM0_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__GEM1_REF_CTRL__DIVISOR0 {12} \
    CONFIG.PSU__CRL_APB__GEM1_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__GEM1_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__GEM2_REF_CTRL__DIVISOR0 {12} \
    CONFIG.PSU__CRL_APB__GEM2_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__GEM2_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__GEM3_REF_CTRL__DIVISOR0 {12} \
    CONFIG.PSU__CRL_APB__GEM3_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__GEM3_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__GEM_TSU_REF_CTRL__DIVISOR0 {6} \
    CONFIG.PSU__CRL_APB__GEM_TSU_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__GEM_TSU_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__I2C0_REF_CTRL__DIVISOR0 {15} \
    CONFIG.PSU__CRL_APB__I2C0_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__I2C0_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__I2C1_REF_CTRL__ACT_FREQMHZ {100.000000} \
    CONFIG.PSU__CRL_APB__I2C1_REF_CTRL__DIVISOR0 {15} \
    CONFIG.PSU__CRL_APB__I2C1_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__I2C1_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__IOPLL_CTRL__DIV2 {0} \
    CONFIG.PSU__CRL_APB__IOPLL_CTRL__FBDIV {45} \
    CONFIG.PSU__CRL_APB__IOPLL_CTRL__SRCSEL {PSS_REF_CLK} \
    CONFIG.PSU__CRL_APB__IOPLL_TO_FPD_CTRL__DIVISOR0 {3} \
    CONFIG.PSU__CRL_APB__IOU_SWITCH_CTRL__ACT_FREQMHZ {250.000000} \
    CONFIG.PSU__CRL_APB__IOU_SWITCH_CTRL__DIVISOR0 {6} \
    CONFIG.PSU__CRL_APB__IOU_SWITCH_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__LPD_LSBUS_CTRL__ACT_FREQMHZ {100.000000} \
    CONFIG.PSU__CRL_APB__LPD_LSBUS_CTRL__DIVISOR0 {15} \
    CONFIG.PSU__CRL_APB__LPD_LSBUS_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__LPD_SWITCH_CTRL__ACT_FREQMHZ {500.000000} \
    CONFIG.PSU__CRL_APB__LPD_SWITCH_CTRL__DIVISOR0 {3} \
    CONFIG.PSU__CRL_APB__LPD_SWITCH_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__NAND_REF_CTRL__DIVISOR0 {15} \
    CONFIG.PSU__CRL_APB__NAND_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__NAND_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__PCAP_CTRL__ACT_FREQMHZ {187.500000} \
    CONFIG.PSU__CRL_APB__PCAP_CTRL__DIVISOR0 {8} \
    CONFIG.PSU__CRL_APB__PCAP_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__PL0_REF_CTRL__ACT_FREQMHZ {100.000000} \
    CONFIG.PSU__CRL_APB__PL0_REF_CTRL__DIVISOR0 {15} \
    CONFIG.PSU__CRL_APB__PL0_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__PL0_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__PL1_REF_CTRL__ACT_FREQMHZ {24.999975} \
    CONFIG.PSU__CRL_APB__PL1_REF_CTRL__DIVISOR0 {15} \
    CONFIG.PSU__CRL_APB__PL1_REF_CTRL__DIVISOR1 {4} \
    CONFIG.PSU__CRL_APB__PL2_REF_CTRL__ACT_FREQMHZ {299.999700} \
    CONFIG.PSU__CRL_APB__PL2_REF_CTRL__DIVISOR0 {5} \
    CONFIG.PSU__CRL_APB__PL2_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__PL3_REF_CTRL__ACT_FREQMHZ {374.999625} \
    CONFIG.PSU__CRL_APB__PL3_REF_CTRL__DIVISOR0 {4} \
    CONFIG.PSU__CRL_APB__PL3_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__QSPI_REF_CTRL__DIVISOR0 {12} \
    CONFIG.PSU__CRL_APB__QSPI_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__QSPI_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__RPLL_CTRL__DIV2 {1} \
    CONFIG.PSU__CRL_APB__RPLL_CTRL__FBDIV {70} \
    CONFIG.PSU__CRL_APB__RPLL_CTRL__FRACDATA {0.779} \
    CONFIG.PSU__CRL_APB__RPLL_CTRL__FRACFREQ {25} \
    CONFIG.PSU__CRL_APB__RPLL_CTRL__SRCSEL {PSS_REF_CLK} \
    CONFIG.PSU__CRL_APB__RPLL_FRAC_CFG__ENABLED {1} \
    CONFIG.PSU__CRL_APB__RPLL_TO_FPD_CTRL__DIVISOR0 {3} \
    CONFIG.PSU__CRL_APB__SDIO0_REF_CTRL__ACT_FREQMHZ {187.500000} \
    CONFIG.PSU__CRL_APB__SDIO0_REF_CTRL__DIVISOR0 {8} \
    CONFIG.PSU__CRL_APB__SDIO0_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__SDIO0_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__SDIO1_REF_CTRL__ACT_FREQMHZ {187.500000} \
    CONFIG.PSU__CRL_APB__SDIO1_REF_CTRL__DIVISOR0 {8} \
    CONFIG.PSU__CRL_APB__SDIO1_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__SDIO1_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__SPI0_REF_CTRL__ACT_FREQMHZ {187.500000} \
    CONFIG.PSU__CRL_APB__SPI0_REF_CTRL__DIVISOR0 {8} \
    CONFIG.PSU__CRL_APB__SPI0_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__SPI0_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__SPI1_REF_CTRL__ACT_FREQMHZ {187.500000} \
    CONFIG.PSU__CRL_APB__SPI1_REF_CTRL__DIVISOR0 {8} \
    CONFIG.PSU__CRL_APB__SPI1_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__SPI1_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__TIMESTAMP_REF_CTRL__ACT_FREQMHZ {100.000000} \
    CONFIG.PSU__CRL_APB__TIMESTAMP_REF_CTRL__DIVISOR0 {15} \
    CONFIG.PSU__CRL_APB__TIMESTAMP_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__UART0_REF_CTRL__ACT_FREQMHZ {100.000000} \
    CONFIG.PSU__CRL_APB__UART0_REF_CTRL__DIVISOR0 {15} \
    CONFIG.PSU__CRL_APB__UART0_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__UART0_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__UART1_REF_CTRL__ACT_FREQMHZ {100.000000} \
    CONFIG.PSU__CRL_APB__UART1_REF_CTRL__DIVISOR0 {15} \
    CONFIG.PSU__CRL_APB__UART1_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__UART1_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__USB0_BUS_REF_CTRL__ACT_FREQMHZ {250.000000} \
    CONFIG.PSU__CRL_APB__USB0_BUS_REF_CTRL__DIVISOR0 {6} \
    CONFIG.PSU__CRL_APB__USB0_BUS_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__USB0_BUS_REF_CTRL__FREQMHZ {250} \
    CONFIG.PSU__CRL_APB__USB0_BUS_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__USB1_BUS_REF_CTRL__ACT_FREQMHZ {250.000000} \
    CONFIG.PSU__CRL_APB__USB1_BUS_REF_CTRL__DIVISOR0 {6} \
    CONFIG.PSU__CRL_APB__USB1_BUS_REF_CTRL__DIVISOR1 {1} \
    CONFIG.PSU__CRL_APB__USB1_BUS_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__USB3_DUAL_REF_CTRL__ACT_FREQMHZ {20.000000} \
    CONFIG.PSU__CRL_APB__USB3_DUAL_REF_CTRL__DIVISOR0 {5} \
    CONFIG.PSU__CRL_APB__USB3_DUAL_REF_CTRL__DIVISOR1 {15} \
    CONFIG.PSU__CRL_APB__USB3_DUAL_REF_CTRL__FREQMHZ {20} \
    CONFIG.PSU__CRL_APB__USB3_DUAL_REF_CTRL__SRCSEL {IOPLL} \
    CONFIG.PSU__CRL_APB__USB3__ENABLE {1} \
    CONFIG.PSU__CSUPMU__PERIPHERAL__VALID {1} \
    CONFIG.PSU__DDRC__ADDR_MIRROR {1} \
    CONFIG.PSU__DDRC__BRC_MAPPING {ROW_BANK_COL} \
    CONFIG.PSU__DDRC__BUS_WIDTH {32 Bit} \
    CONFIG.PSU__DDRC__CLOCK_STOP_EN {0} \
    CONFIG.PSU__DDRC__DEVICE_CAPACITY {8192 MBits} \
    CONFIG.PSU__DDRC__DM_DBI {DM_NO_DBI} \
    CONFIG.PSU__DDRC__DQMAP_0_3 {0} \
    CONFIG.PSU__DDRC__DQMAP_12_15 {0} \
    CONFIG.PSU__DDRC__DQMAP_16_19 {0} \
    CONFIG.PSU__DDRC__DQMAP_20_23 {0} \
    CONFIG.PSU__DDRC__DQMAP_24_27 {0} \
    CONFIG.PSU__DDRC__DQMAP_28_31 {0} \
    CONFIG.PSU__DDRC__DQMAP_32_35 {0} \
    CONFIG.PSU__DDRC__DQMAP_36_39 {0} \
    CONFIG.PSU__DDRC__DQMAP_40_43 {0} \
    CONFIG.PSU__DDRC__DQMAP_44_47 {0} \
    CONFIG.PSU__DDRC__DQMAP_48_51 {0} \
    CONFIG.PSU__DDRC__DQMAP_4_7 {0} \
    CONFIG.PSU__DDRC__DQMAP_52_55 {0} \
    CONFIG.PSU__DDRC__DQMAP_56_59 {0} \
    CONFIG.PSU__DDRC__DQMAP_60_63 {0} \
    CONFIG.PSU__DDRC__DQMAP_64_67 {0} \
    CONFIG.PSU__DDRC__DQMAP_68_71 {0} \
    CONFIG.PSU__DDRC__DQMAP_8_11 {0} \
    CONFIG.PSU__DDRC__DRAM_WIDTH {32 Bits} \
    CONFIG.PSU__DDRC__ECC {Disabled} \
    CONFIG.PSU__DDRC__ENABLE_DP_SWITCH {1} \
    CONFIG.PSU__DDRC__ENABLE_LP4_HAS_ECC_COMP {0} \
    CONFIG.PSU__DDRC__LPDDR4_T_REF_RANGE {Normal (0-85)} \
    CONFIG.PSU__DDRC__MEMORY_TYPE {LPDDR 4} \
    CONFIG.PSU__DDRC__PER_BANK_REFRESH {0} \
    CONFIG.PSU__DDRC__PHY_DBI_MODE {0} \
    CONFIG.PSU__DDRC__RANK_ADDR_COUNT {1} \
    CONFIG.PSU__DDRC__ROW_ADDR_COUNT {15} \
    CONFIG.PSU__DDRC__SPEED_BIN {LPDDR4_1066} \
    CONFIG.PSU__DDRC__STATIC_RD_MODE {0} \
    CONFIG.PSU__DDRC__TRAIN_DATA_EYE {1} \
    CONFIG.PSU__DDRC__TRAIN_READ_GATE {1} \
    CONFIG.PSU__DDRC__TRAIN_WRITE_LEVEL {1} \
    CONFIG.PSU__DDRC__T_FAW {40.0} \
    CONFIG.PSU__DDRC__T_RAS_MIN {42} \
    CONFIG.PSU__DDRC__T_RC {63} \
    CONFIG.PSU__DDRC__T_RCD {10} \
    CONFIG.PSU__DDRC__T_RP {12} \
    CONFIG.PSU__DDRC__VENDOR_PART {OTHERS} \
    CONFIG.PSU__DDRC__VREF {0} \
    CONFIG.PSU__DDR_HIGH_ADDRESS_GUI_ENABLE {0} \
    CONFIG.PSU__DDR_QOS_ENABLE {1} \
    CONFIG.PSU__DDR_QOS_HP0_RDQOS {7} \
    CONFIG.PSU__DDR_QOS_HP0_WRQOS {15} \
    CONFIG.PSU__DDR_QOS_HP1_RDQOS {3} \
    CONFIG.PSU__DDR_QOS_HP1_WRQOS {3} \
    CONFIG.PSU__DDR_QOS_HP2_RDQOS {3} \
    CONFIG.PSU__DDR_QOS_HP2_WRQOS {3} \
    CONFIG.PSU__DDR_QOS_HP3_RDQOS {3} \
    CONFIG.PSU__DDR_QOS_HP3_WRQOS {3} \
    CONFIG.PSU__DDR_QOS_PORT0_TYPE {Low Latency} \
    CONFIG.PSU__DDR_QOS_PORT1_VN1_TYPE {Low Latency} \
    CONFIG.PSU__DDR_QOS_PORT1_VN2_TYPE {Best Effort} \
    CONFIG.PSU__DDR_QOS_PORT2_VN1_TYPE {Low Latency} \
    CONFIG.PSU__DDR_QOS_PORT2_VN2_TYPE {Best Effort} \
    CONFIG.PSU__DDR_QOS_PORT3_TYPE {Video Traffic} \
    CONFIG.PSU__DDR_QOS_PORT4_TYPE {Best Effort} \
    CONFIG.PSU__DDR_QOS_PORT5_TYPE {Best Effort} \
    CONFIG.PSU__DDR_QOS_RD_HPR_THRSHLD {0} \
    CONFIG.PSU__DDR_QOS_RD_LPR_THRSHLD {16} \
    CONFIG.PSU__DDR_QOS_WR_THRSHLD {16} \
    CONFIG.PSU__DDR__INTERFACE__FREQMHZ {266.500} \
    CONFIG.PSU__DISPLAYPORT__LANE0__ENABLE {1} \
    CONFIG.PSU__DISPLAYPORT__LANE0__IO {GT Lane1} \
    CONFIG.PSU__DISPLAYPORT__LANE1__ENABLE {1} \
    CONFIG.PSU__DISPLAYPORT__LANE1__IO {GT Lane0} \
    CONFIG.PSU__DISPLAYPORT__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__DLL__ISUSED {1} \
    CONFIG.PSU__DPAUX__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__DPAUX__PERIPHERAL__IO {MIO 27 .. 30} \
    CONFIG.PSU__DP__LANE_SEL {Dual Lower} \
    CONFIG.PSU__DP__REF_CLK_FREQ {27} \
    CONFIG.PSU__DP__REF_CLK_SEL {Ref Clk1} \
    CONFIG.PSU__ENET3__PERIPHERAL__ENABLE {0} \
    CONFIG.PSU__FPD_SLCR__WDT1__ACT_FREQMHZ {100.000000} \
    CONFIG.PSU__FPGA_PL0_ENABLE {1} \
    CONFIG.PSU__FPGA_PL1_ENABLE {0} \
    CONFIG.PSU__FPGA_PL2_ENABLE {0} \
    CONFIG.PSU__FPGA_PL3_ENABLE {0} \
    CONFIG.PSU__GPIO0_MIO__IO {MIO 0 .. 25} \
    CONFIG.PSU__GPIO0_MIO__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__GPIO1_MIO__IO {MIO 26 .. 51} \
    CONFIG.PSU__GPIO1_MIO__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__GPIO2_MIO__IO {MIO 52 .. 77} \
    CONFIG.PSU__GPIO2_MIO__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__GT__LINK_SPEED {HBR} \
    CONFIG.PSU__GT__PRE_EMPH_LVL_4 {0} \
    CONFIG.PSU__GT__VLT_SWNG_LVL_4 {0} \
    CONFIG.PSU__I2C0__PERIPHERAL__ENABLE {0} \
    CONFIG.PSU__I2C1__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__I2C1__PERIPHERAL__IO {MIO 4 .. 5} \
    CONFIG.PSU__IOU_SLCR__IOU_TTC_APB_CLK__TTC0_SEL {APB} \
    CONFIG.PSU__IOU_SLCR__IOU_TTC_APB_CLK__TTC1_SEL {APB} \
    CONFIG.PSU__IOU_SLCR__IOU_TTC_APB_CLK__TTC2_SEL {APB} \
    CONFIG.PSU__IOU_SLCR__IOU_TTC_APB_CLK__TTC3_SEL {APB} \
    CONFIG.PSU__IOU_SLCR__TTC0__ACT_FREQMHZ {100.000000} \
    CONFIG.PSU__IOU_SLCR__TTC1__ACT_FREQMHZ {100.000000} \
    CONFIG.PSU__IOU_SLCR__TTC2__ACT_FREQMHZ {100.000000} \
    CONFIG.PSU__IOU_SLCR__TTC3__ACT_FREQMHZ {100.000000} \
    CONFIG.PSU__IOU_SLCR__WDT0__ACT_FREQMHZ {100.000000} \
    CONFIG.PSU__LPD_SLCR__CSUPMU__ACT_FREQMHZ {100.000000} \
    CONFIG.PSU__MAXIGP0__DATA_WIDTH {128} \
    CONFIG.PSU__OVERRIDE__BASIC_CLOCK {1} \
    CONFIG.PSU__PL_CLK0_BUF {TRUE} \
    CONFIG.PSU__PMU_COHERENCY {0} \
    CONFIG.PSU__PMU__AIBACK__ENABLE {0} \
    CONFIG.PSU__PMU__EMIO_GPI__ENABLE {0} \
    CONFIG.PSU__PMU__EMIO_GPO__ENABLE {0} \
    CONFIG.PSU__PMU__GPI0__ENABLE {1} \
    CONFIG.PSU__PMU__GPI0__IO {MIO 26} \
    CONFIG.PSU__PMU__GPI1__ENABLE {0} \
    CONFIG.PSU__PMU__GPI2__ENABLE {0} \
    CONFIG.PSU__PMU__GPI3__ENABLE {0} \
    CONFIG.PSU__PMU__GPI4__ENABLE {0} \
    CONFIG.PSU__PMU__GPI5__ENABLE {0} \
    CONFIG.PSU__PMU__GPO0__ENABLE {1} \
    CONFIG.PSU__PMU__GPO0__IO {MIO 32} \
    CONFIG.PSU__PMU__GPO1__ENABLE {1} \
    CONFIG.PSU__PMU__GPO1__IO {MIO 33} \
    CONFIG.PSU__PMU__GPO2__ENABLE {1} \
    CONFIG.PSU__PMU__GPO2__IO {MIO 34} \
    CONFIG.PSU__PMU__GPO2__POLARITY {high} \
    CONFIG.PSU__PMU__GPO3__ENABLE {0} \
    CONFIG.PSU__PMU__GPO4__ENABLE {0} \
    CONFIG.PSU__PMU__GPO5__ENABLE {0} \
    CONFIG.PSU__PMU__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__PMU__PLERROR__ENABLE {0} \
    CONFIG.PSU__PRESET_APPLIED {1} \
    CONFIG.PSU__PROTECTION__MASTERS {USB1:NonSecure;1|USB0:NonSecure;1|S_AXI_LPD:NA;0|S_AXI_HPC1_FPD:NA;0|S_AXI_HPC0_FPD:NA;1|S_AXI_HP3_FPD:NA;0|S_AXI_HP2_FPD:NA;0|S_AXI_HP1_FPD:NA;0|S_AXI_HP0_FPD:NA;0|S_AXI_ACP:NA;0|S_AXI_ACE:NA;0|SD1:NonSecure;1|SD0:NonSecure;1|SATA1:NonSecure;0|SATA0:NonSecure;0|RPU1:Secure;1|RPU0:Secure;1|QSPI:NonSecure;0|PMU:NA;1|PCIe:NonSecure;0|NAND:NonSecure;0|LDMA:NonSecure;1|GPU:NonSecure;1|GEM3:NonSecure;0|GEM2:NonSecure;0|GEM1:NonSecure;0|GEM0:NonSecure;0|FDMA:NonSecure;1|DP:NonSecure;1|DAP:NA;1|Coresight:NA;1|CSU:NA;1|APU:NA;1}\
\
    CONFIG.PSU__PROTECTION__SLAVES {LPD;USB3_1_XHCI;FE300000;FE3FFFFF;1|LPD;USB3_1;FF9E0000;FF9EFFFF;1|LPD;USB3_0_XHCI;FE200000;FE2FFFFF;1|LPD;USB3_0;FF9D0000;FF9DFFFF;1|LPD;UART1;FF010000;FF01FFFF;1|LPD;UART0;FF000000;FF00FFFF;1|LPD;TTC3;FF140000;FF14FFFF;1|LPD;TTC2;FF130000;FF13FFFF;1|LPD;TTC1;FF120000;FF12FFFF;1|LPD;TTC0;FF110000;FF11FFFF;1|FPD;SWDT1;FD4D0000;FD4DFFFF;1|LPD;SWDT0;FF150000;FF15FFFF;1|LPD;SPI1;FF050000;FF05FFFF;1|LPD;SPI0;FF040000;FF04FFFF;1|FPD;SMMU_REG;FD5F0000;FD5FFFFF;1|FPD;SMMU;FD800000;FDFFFFFF;1|FPD;SIOU;FD3D0000;FD3DFFFF;1|FPD;SERDES;FD400000;FD47FFFF;1|LPD;SD1;FF170000;FF17FFFF;1|LPD;SD0;FF160000;FF16FFFF;1|FPD;SATA;FD0C0000;FD0CFFFF;0|LPD;RTC;FFA60000;FFA6FFFF;1|LPD;RSA_CORE;FFCE0000;FFCEFFFF;1|LPD;RPU;FF9A0000;FF9AFFFF;1|LPD;R5_TCM_RAM_GLOBAL;FFE00000;FFE3FFFF;1|LPD;R5_1_Instruction_Cache;FFEC0000;FFECFFFF;1|LPD;R5_1_Data_Cache;FFED0000;FFEDFFFF;1|LPD;R5_1_BTCM_GLOBAL;FFEB0000;FFEBFFFF;1|LPD;R5_1_ATCM_GLOBAL;FFE90000;FFE9FFFF;1|LPD;R5_0_Instruction_Cache;FFE40000;FFE4FFFF;1|LPD;R5_0_Data_Cache;FFE50000;FFE5FFFF;1|LPD;R5_0_BTCM_GLOBAL;FFE20000;FFE2FFFF;1|LPD;R5_0_ATCM_GLOBAL;FFE00000;FFE0FFFF;1|LPD;QSPI_Linear_Address;C0000000;DFFFFFFF;1|LPD;QSPI;FF0F0000;FF0FFFFF;0|LPD;PMU_RAM;FFDC0000;FFDDFFFF;1|LPD;PMU_GLOBAL;FFD80000;FFDBFFFF;1|FPD;PCIE_MAIN;FD0E0000;FD0EFFFF;0|FPD;PCIE_LOW;E0000000;EFFFFFFF;0|FPD;PCIE_HIGH2;8000000000;BFFFFFFFFF;0|FPD;PCIE_HIGH1;600000000;7FFFFFFFF;0|FPD;PCIE_DMA;FD0F0000;FD0FFFFF;0|FPD;PCIE_ATTRIB;FD480000;FD48FFFF;0|LPD;OCM_XMPU_CFG;FFA70000;FFA7FFFF;1|LPD;OCM_SLCR;FF960000;FF96FFFF;1|OCM;OCM;FFFC0000;FFFFFFFF;1|LPD;NAND;FF100000;FF10FFFF;0|LPD;MBISTJTAG;FFCF0000;FFCFFFFF;1|LPD;LPD_XPPU_SINK;FF9C0000;FF9CFFFF;1|LPD;LPD_XPPU;FF980000;FF98FFFF;1|LPD;LPD_SLCR_SECURE;FF4B0000;FF4DFFFF;1|LPD;LPD_SLCR;FF410000;FF4AFFFF;1|LPD;LPD_GPV;FE100000;FE1FFFFF;1|LPD;LPD_DMA_7;FFAF0000;FFAFFFFF;1|LPD;LPD_DMA_6;FFAE0000;FFAEFFFF;1|LPD;LPD_DMA_5;FFAD0000;FFADFFFF;1|LPD;LPD_DMA_4;FFAC0000;FFACFFFF;1|LPD;LPD_DMA_3;FFAB0000;FFABFFFF;1|LPD;LPD_DMA_2;FFAA0000;FFAAFFFF;1|LPD;LPD_DMA_1;FFA90000;FFA9FFFF;1|LPD;LPD_DMA_0;FFA80000;FFA8FFFF;1|LPD;IPI_CTRL;FF380000;FF3FFFFF;1|LPD;IOU_SLCR;FF180000;FF23FFFF;1|LPD;IOU_SECURE_SLCR;FF240000;FF24FFFF;1|LPD;IOU_SCNTRS;FF260000;FF26FFFF;1|LPD;IOU_SCNTR;FF250000;FF25FFFF;1|LPD;IOU_GPV;FE000000;FE0FFFFF;1|LPD;I2C1;FF030000;FF03FFFF;1|LPD;I2C0;FF020000;FF02FFFF;0|FPD;GPU;FD4B0000;FD4BFFFF;1|LPD;GPIO;FF0A0000;FF0AFFFF;1|LPD;GEM3;FF0E0000;FF0EFFFF;0|LPD;GEM2;FF0D0000;FF0DFFFF;0|LPD;GEM1;FF0C0000;FF0CFFFF;0|LPD;GEM0;FF0B0000;FF0BFFFF;0|FPD;FPD_XMPU_SINK;FD4F0000;FD4FFFFF;1|FPD;FPD_XMPU_CFG;FD5D0000;FD5DFFFF;1|FPD;FPD_SLCR_SECURE;FD690000;FD6CFFFF;1|FPD;FPD_SLCR;FD610000;FD68FFFF;1|FPD;FPD_DMA_CH7;FD570000;FD57FFFF;1|FPD;FPD_DMA_CH6;FD560000;FD56FFFF;1|FPD;FPD_DMA_CH5;FD550000;FD55FFFF;1|FPD;FPD_DMA_CH4;FD540000;FD54FFFF;1|FPD;FPD_DMA_CH3;FD530000;FD53FFFF;1|FPD;FPD_DMA_CH2;FD520000;FD52FFFF;1|FPD;FPD_DMA_CH1;FD510000;FD51FFFF;1|FPD;FPD_DMA_CH0;FD500000;FD50FFFF;1|LPD;EFUSE;FFCC0000;FFCCFFFF;1|FPD;Display\
Port;FD4A0000;FD4AFFFF;1|FPD;DPDMA;FD4C0000;FD4CFFFF;1|FPD;DDR_XMPU5_CFG;FD050000;FD05FFFF;1|FPD;DDR_XMPU4_CFG;FD040000;FD04FFFF;1|FPD;DDR_XMPU3_CFG;FD030000;FD03FFFF;1|FPD;DDR_XMPU2_CFG;FD020000;FD02FFFF;1|FPD;DDR_XMPU1_CFG;FD010000;FD01FFFF;1|FPD;DDR_XMPU0_CFG;FD000000;FD00FFFF;1|FPD;DDR_QOS_CTRL;FD090000;FD09FFFF;1|FPD;DDR_PHY;FD080000;FD08FFFF;1|DDR;DDR_LOW;0;7FFFFFFF;1|DDR;DDR_HIGH;800000000;800000000;0|FPD;DDDR_CTRL;FD070000;FD070FFF;1|LPD;Coresight;FE800000;FEFFFFFF;1|LPD;CSU_DMA;FFC80000;FFC9FFFF;1|LPD;CSU;FFCA0000;FFCAFFFF;1|LPD;CRL_APB;FF5E0000;FF85FFFF;1|FPD;CRF_APB;FD1A0000;FD2DFFFF;1|FPD;CCI_REG;FD5E0000;FD5EFFFF;1|LPD;CAN1;FF070000;FF07FFFF;0|LPD;CAN0;FF060000;FF06FFFF;0|FPD;APU;FD5C0000;FD5CFFFF;1|LPD;APM_INTC_IOU;FFA20000;FFA2FFFF;1|LPD;APM_FPD_LPD;FFA30000;FFA3FFFF;1|FPD;APM_5;FD490000;FD49FFFF;1|FPD;APM_0;FD0B0000;FD0BFFFF;1|LPD;APM2;FFA10000;FFA1FFFF;1|LPD;APM1;FFA00000;FFA0FFFF;1|LPD;AMS;FFA50000;FFA5FFFF;1|FPD;AFI_5;FD3B0000;FD3BFFFF;1|FPD;AFI_4;FD3A0000;FD3AFFFF;1|FPD;AFI_3;FD390000;FD39FFFF;1|FPD;AFI_2;FD380000;FD38FFFF;1|FPD;AFI_1;FD370000;FD37FFFF;1|FPD;AFI_0;FD360000;FD36FFFF;1|LPD;AFIFM6;FF9B0000;FF9BFFFF;1|FPD;ACPU_GIC;F9010000;F907FFFF;1}\
\
    CONFIG.PSU__PSS_REF_CLK__FREQMHZ {33.333333} \
    CONFIG.PSU__QSPI__PERIPHERAL__ENABLE {0} \
    CONFIG.PSU__SATA__PERIPHERAL__ENABLE {0} \
    CONFIG.PSU__SAXIGP0__DATA_WIDTH {128} \
    CONFIG.PSU__SD0_COHERENCY {0} \
    CONFIG.PSU__SD0_ROUTE_THROUGH_FPD {0} \
    CONFIG.PSU__SD0__CLK_50_SDR_ITAP_DLY {0x15} \
    CONFIG.PSU__SD0__CLK_50_SDR_OTAP_DLY {0x5} \
    CONFIG.PSU__SD0__DATA_TRANSFER_MODE {4Bit} \
    CONFIG.PSU__SD0__GRP_CD__ENABLE {1} \
    CONFIG.PSU__SD0__GRP_CD__IO {MIO 24} \
    CONFIG.PSU__SD0__GRP_POW__ENABLE {0} \
    CONFIG.PSU__SD0__GRP_WP__ENABLE {0} \
    CONFIG.PSU__SD0__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__SD0__PERIPHERAL__IO {MIO 13 .. 16 21 22} \
    CONFIG.PSU__SD0__SLOT_TYPE {SD 2.0} \
    CONFIG.PSU__SD1_COHERENCY {0} \
    CONFIG.PSU__SD1_ROUTE_THROUGH_FPD {0} \
    CONFIG.PSU__SD1__CLK_50_SDR_ITAP_DLY {0x15} \
    CONFIG.PSU__SD1__CLK_50_SDR_OTAP_DLY {0x5} \
    CONFIG.PSU__SD1__DATA_TRANSFER_MODE {4Bit} \
    CONFIG.PSU__SD1__GRP_CD__ENABLE {0} \
    CONFIG.PSU__SD1__GRP_POW__ENABLE {0} \
    CONFIG.PSU__SD1__GRP_WP__ENABLE {0} \
    CONFIG.PSU__SD1__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__SD1__PERIPHERAL__IO {MIO 46 .. 51} \
    CONFIG.PSU__SD1__SLOT_TYPE {SD 2.0} \
    CONFIG.PSU__SPI0__GRP_SS0__IO {MIO 41} \
    CONFIG.PSU__SPI0__GRP_SS1__ENABLE {0} \
    CONFIG.PSU__SPI0__GRP_SS2__ENABLE {0} \
    CONFIG.PSU__SPI0__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__SPI0__PERIPHERAL__IO {MIO 38 .. 43} \
    CONFIG.PSU__SPI1__GRP_SS0__IO {MIO 9} \
    CONFIG.PSU__SPI1__GRP_SS1__ENABLE {0} \
    CONFIG.PSU__SPI1__GRP_SS2__ENABLE {0} \
    CONFIG.PSU__SPI1__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__SPI1__PERIPHERAL__IO {MIO 6 .. 11} \
    CONFIG.PSU__SWDT0__CLOCK__ENABLE {0} \
    CONFIG.PSU__SWDT0__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__SWDT0__RESET__ENABLE {0} \
    CONFIG.PSU__SWDT1__CLOCK__ENABLE {0} \
    CONFIG.PSU__SWDT1__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__SWDT1__RESET__ENABLE {0} \
    CONFIG.PSU__TTC0__CLOCK__ENABLE {0} \
    CONFIG.PSU__TTC0__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__TTC0__WAVEOUT__ENABLE {0} \
    CONFIG.PSU__TTC1__CLOCK__ENABLE {0} \
    CONFIG.PSU__TTC1__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__TTC1__WAVEOUT__ENABLE {0} \
    CONFIG.PSU__TTC2__CLOCK__ENABLE {0} \
    CONFIG.PSU__TTC2__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__TTC2__WAVEOUT__ENABLE {0} \
    CONFIG.PSU__TTC3__CLOCK__ENABLE {0} \
    CONFIG.PSU__TTC3__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__TTC3__WAVEOUT__ENABLE {0} \
    CONFIG.PSU__UART0__BAUD_RATE {115200} \
    CONFIG.PSU__UART0__MODEM__ENABLE {0} \
    CONFIG.PSU__UART0__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__UART0__PERIPHERAL__IO {MIO 2 .. 3} \
    CONFIG.PSU__UART1__BAUD_RATE {115200} \
    CONFIG.PSU__UART1__MODEM__ENABLE {0} \
    CONFIG.PSU__UART1__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__UART1__PERIPHERAL__IO {MIO 0 .. 1} \
    CONFIG.PSU__USB0_COHERENCY {0} \
    CONFIG.PSU__USB0__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__USB0__PERIPHERAL__IO {MIO 52 .. 63} \
    CONFIG.PSU__USB0__REF_CLK_FREQ {26} \
    CONFIG.PSU__USB0__REF_CLK_SEL {Ref Clk0} \
    CONFIG.PSU__USB1_COHERENCY {0} \
    CONFIG.PSU__USB1__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__USB1__PERIPHERAL__IO {MIO 64 .. 75} \
    CONFIG.PSU__USB1__REF_CLK_FREQ {26} \
    CONFIG.PSU__USB1__REF_CLK_SEL {Ref Clk0} \
    CONFIG.PSU__USB2_0__EMIO__ENABLE {0} \
    CONFIG.PSU__USB2_1__EMIO__ENABLE {0} \
    CONFIG.PSU__USB3_0__EMIO__ENABLE {0} \
    CONFIG.PSU__USB3_0__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__USB3_0__PERIPHERAL__IO {GT Lane2} \
    CONFIG.PSU__USB3_1__EMIO__ENABLE {0} \
    CONFIG.PSU__USB3_1__PERIPHERAL__ENABLE {1} \
    CONFIG.PSU__USB3_1__PERIPHERAL__IO {GT Lane3} \
    CONFIG.PSU__USB__RESET__MODE {Boot Pin} \
    CONFIG.PSU__USB__RESET__POLARITY {Active Low} \
    CONFIG.PSU__USE__IRQ0 {1} \
    CONFIG.PSU__USE__M_AXI_GP0 {1} \
    CONFIG.PSU__USE__M_AXI_GP1 {0} \
    CONFIG.PSU__USE__M_AXI_GP2 {0} \
    CONFIG.PSU__USE__S_AXI_GP0 {1} \
  ] $zynq_ultra_ps_e_0


  # Create instance: heron, and set properties
  set heron [ create_bd_cell -type ip -vlnv user.org:user:topEntity:1.0 heron ]

  # Create instance: rst_ps8_0_100M, and set properties
  set rst_ps8_0_100M [ create_bd_cell -type ip -vlnv xilinx.com:ip:proc_sys_reset:5.0 rst_ps8_0_100M ]

  # Create instance: const_high, and set properties
  set const_high [ create_bd_cell -type ip -vlnv xilinx.com:ip:xlconstant:1.1 const_high ]

  # Create instance: dma_tx, and set properties
  set dma_tx [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_dma:7.1 dma_tx ]
  set_property -dict [list \
    CONFIG.c_include_s2mm {0} \
    CONFIG.c_include_sg {0} \
    CONFIG.c_m_axis_mm2s_tdata_width {512} \
    CONFIG.c_sg_length_width {20} \
  ] $dma_tx


  # Create instance: hier_splitter
  create_hier_cell_hier_splitter [current_bd_instance .] hier_splitter

  # Create instance: hier_joiner
  create_hier_cell_hier_joiner [current_bd_instance .] hier_joiner

  # Create instance: dma_rx, and set properties
  set dma_rx [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_dma:7.1 dma_rx ]
  set_property -dict [list \
    CONFIG.c_include_mm2s {0} \
    CONFIG.c_include_sg {0} \
    CONFIG.c_m_axi_s2mm_data_width {256} \
    CONFIG.c_s_axis_s2mm_tdata_width {256} \
    CONFIG.c_sg_length_width {20} \
  ] $dma_rx


  # Create instance: ps8_0_axi_periph, and set properties
  set ps8_0_axi_periph [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_interconnect:2.1 ps8_0_axi_periph ]
  set_property CONFIG.NUM_MI {3} $ps8_0_axi_periph


  # Create instance: axi_smc, and set properties
  set axi_smc [ create_bd_cell -type ip -vlnv xilinx.com:ip:smartconnect:1.0 axi_smc ]
  set_property CONFIG.NUM_SI {2} $axi_smc


  # Create instance: xlconcat_0, and set properties
  set xlconcat_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:xlconcat:2.1 xlconcat_0 ]

  # Create instance: util_reduced_logic_0, and set properties
  set util_reduced_logic_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:util_reduced_logic:2.0 util_reduced_logic_0 ]
  set_property CONFIG.C_SIZE {2} $util_reduced_logic_0


  # Create instance: gc_threshold, and set properties
  set gc_threshold [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_gpio:2.0 gc_threshold ]
  set_property CONFIG.C_ALL_OUTPUTS {1} $gc_threshold


  # Create interface connections
  connect_bd_intf_net -intf_net axi_smc_M00_AXI [get_bd_intf_pins axi_smc/M00_AXI] [get_bd_intf_pins zynq_ultra_ps_e_0/S_AXI_HPC0_FPD]
  connect_bd_intf_net -intf_net dma_rx_M_AXI_S2MM [get_bd_intf_pins dma_rx/M_AXI_S2MM] [get_bd_intf_pins axi_smc/S00_AXI]
  connect_bd_intf_net -intf_net dma_tx_M_AXI_MM2S [get_bd_intf_pins dma_tx/M_AXI_MM2S] [get_bd_intf_pins axi_smc/S01_AXI]
  connect_bd_intf_net -intf_net ps8_0_axi_periph_M00_AXI [get_bd_intf_pins ps8_0_axi_periph/M00_AXI] [get_bd_intf_pins dma_rx/S_AXI_LITE]
  connect_bd_intf_net -intf_net ps8_0_axi_periph_M01_AXI [get_bd_intf_pins ps8_0_axi_periph/M01_AXI] [get_bd_intf_pins dma_tx/S_AXI_LITE]
  connect_bd_intf_net -intf_net ps8_0_axi_periph_M02_AXI [get_bd_intf_pins ps8_0_axi_periph/M02_AXI] [get_bd_intf_pins gc_threshold/S_AXI]
  connect_bd_intf_net -intf_net zynq_ultra_ps_e_0_M_AXI_HPM0_FPD [get_bd_intf_pins zynq_ultra_ps_e_0/M_AXI_HPM0_FPD] [get_bd_intf_pins ps8_0_axi_periph/S00_AXI]

  # Create port connections
  connect_bd_net -net axi_dma_0_m_axis_mm2s_tdata [get_bd_pins dma_tx/m_axis_mm2s_tdata] [get_bd_pins hier_splitter/Din]
  connect_bd_net -net axi_dma_0_m_axis_mm2s_tvalid [get_bd_pins dma_tx/m_axis_mm2s_tvalid] [get_bd_pins hier_splitter/In1]
  connect_bd_net -net axi_gpio_0_gpio_io_o [get_bd_pins gc_threshold/gpio_io_o] [get_bd_pins heron/gcThres]
  connect_bd_net -net const_high_dout [get_bd_pins const_high/dout] [get_bd_pins heron/en] [get_bd_pins dma_tx/m_axis_mm2s_tready]
  connect_bd_net -net dma_rx_s2mm_introut [get_bd_pins dma_rx/s2mm_introut] [get_bd_pins xlconcat_0/In1]
  connect_bd_net -net dma_tx_mm2s_introut [get_bd_pins dma_tx/mm2s_introut] [get_bd_pins xlconcat_0/In0]
  connect_bd_net -net gate_wen_Res [get_bd_pins hier_splitter/wen] [get_bd_pins heron/codeWE]
  connect_bd_net -net go [get_bd_pins hier_splitter/go] [get_bd_pins heron/go]
  connect_bd_net -net heron_retVld [get_bd_pins heron/retVld] [get_bd_pins dma_rx/s_axis_s2mm_tvalid] [get_bd_pins dma_rx/s_axis_s2mm_tlast]
  connect_bd_net -net hier_joiner_dout [get_bd_pins hier_joiner/dout] [get_bd_pins dma_rx/s_axis_s2mm_tdata]
  connect_bd_net -net hier_splitter_Dout [get_bd_pins hier_splitter/code_data] [get_bd_pins heron/codeData]
  connect_bd_net -net rst_ps8_0_100M_peripheral_aresetn [get_bd_pins rst_ps8_0_100M/peripheral_aresetn] [get_bd_pins ps8_0_axi_periph/S00_ARESETN] [get_bd_pins dma_rx/axi_resetn] [get_bd_pins ps8_0_axi_periph/M00_ARESETN] [get_bd_pins ps8_0_axi_periph/ARESETN] [get_bd_pins dma_tx/axi_resetn] [get_bd_pins ps8_0_axi_periph/M01_ARESETN] [get_bd_pins axi_smc/aresetn] [get_bd_pins gc_threshold/s_axi_aresetn] [get_bd_pins ps8_0_axi_periph/M02_ARESETN]
  connect_bd_net -net rst_ps8_0_100M_peripheral_reset [get_bd_pins rst_ps8_0_100M/peripheral_reset] [get_bd_pins heron/rst]
  connect_bd_net -net slice_addr_Dout [get_bd_pins hier_splitter/addr] [get_bd_pins heron/codeAddr]
  connect_bd_net -net topEntity_0_ret [get_bd_pins heron/ret] [get_bd_pins hier_joiner/In0]
  connect_bd_net -net topEntity_0_stats [get_bd_pins heron/stats] [get_bd_pins hier_joiner/In1]
  connect_bd_net -net util_reduced_logic_0_Res [get_bd_pins util_reduced_logic_0/Res] [get_bd_pins zynq_ultra_ps_e_0/pl_ps_irq0]
  connect_bd_net -net xlconcat_0_dout [get_bd_pins xlconcat_0/dout] [get_bd_pins util_reduced_logic_0/Op1]
  connect_bd_net -net zynq_ultra_ps_e_0_pl_clk0 [get_bd_pins zynq_ultra_ps_e_0/pl_clk0] [get_bd_pins heron/clk] [get_bd_pins rst_ps8_0_100M/slowest_sync_clk] [get_bd_pins zynq_ultra_ps_e_0/maxihpm0_fpd_aclk] [get_bd_pins ps8_0_axi_periph/S00_ACLK] [get_bd_pins dma_rx/s_axi_lite_aclk] [get_bd_pins ps8_0_axi_periph/M00_ACLK] [get_bd_pins ps8_0_axi_periph/ACLK] [get_bd_pins dma_tx/s_axi_lite_aclk] [get_bd_pins ps8_0_axi_periph/M01_ACLK] [get_bd_pins dma_rx/m_axi_s2mm_aclk] [get_bd_pins axi_smc/aclk] [get_bd_pins zynq_ultra_ps_e_0/saxihpc0_fpd_aclk] [get_bd_pins dma_tx/m_axi_mm2s_aclk] [get_bd_pins gc_threshold/s_axi_aclk] [get_bd_pins ps8_0_axi_periph/M02_ACLK]
  connect_bd_net -net zynq_ultra_ps_e_0_pl_resetn0 [get_bd_pins zynq_ultra_ps_e_0/pl_resetn0] [get_bd_pins rst_ps8_0_100M/ext_reset_in]

  # Create address segments
  assign_bd_address -offset 0xA0000000 -range 0x00010000 -target_address_space [get_bd_addr_spaces zynq_ultra_ps_e_0/Data] [get_bd_addr_segs dma_rx/S_AXI_LITE/Reg] -force
  assign_bd_address -offset 0xA0010000 -range 0x00010000 -target_address_space [get_bd_addr_spaces zynq_ultra_ps_e_0/Data] [get_bd_addr_segs dma_tx/S_AXI_LITE/Reg] -force
  assign_bd_address -offset 0xA0020000 -range 0x00010000 -target_address_space [get_bd_addr_spaces zynq_ultra_ps_e_0/Data] [get_bd_addr_segs gc_threshold/S_AXI/Reg] -force
  assign_bd_address -offset 0x00000000 -range 0x80000000 -target_address_space [get_bd_addr_spaces dma_tx/Data_MM2S] [get_bd_addr_segs zynq_ultra_ps_e_0/SAXIGP0/HPC0_DDR_LOW] -force
  assign_bd_address -offset 0x00000000 -range 0x80000000 -target_address_space [get_bd_addr_spaces dma_rx/Data_S2MM] [get_bd_addr_segs zynq_ultra_ps_e_0/SAXIGP0/HPC0_DDR_LOW] -force

  # Exclude Address Segments
  exclude_bd_addr_seg -offset 0xFF000000 -range 0x01000000 -target_address_space [get_bd_addr_spaces dma_rx/Data_S2MM] [get_bd_addr_segs zynq_ultra_ps_e_0/SAXIGP0/HPC0_LPS_OCM]
  exclude_bd_addr_seg -offset 0xFF000000 -range 0x01000000 -target_address_space [get_bd_addr_spaces dma_tx/Data_MM2S] [get_bd_addr_segs zynq_ultra_ps_e_0/SAXIGP0/HPC0_LPS_OCM]


  # Restore current instance
  current_bd_instance $oldCurInst

  save_bd_design
}
# End of create_root_design()


##################################################################
# MAIN FLOW
##################################################################

create_root_design ""


common::send_gid_msg -ssname BD::TCL -id 2053 -severity "WARNING" "This Tcl script was generated from a block design that has not been validated. It is possible that design <$design_name> may result in errors during validation."

