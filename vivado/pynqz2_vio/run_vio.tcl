# See UG908 for VIO tcl command reference:
# https://www.xilinx.com/support/documents/sw_manuals/xilinx2022_1/ug908-vivado-programming-debugging.pdf

# Program device and set VIO probe properties
proc vioBoardSetup {device bitstream probes} {
    open_hw_target
    set_property PROBES.FILE {} [get_hw_devices $device]
    set_property FULL_PROBES.FILE $probes [get_hw_devices $device]
    set_property PROGRAM.FILE $bitstream [get_hw_devices $device]
    current_hw_device [get_hw_devices $device]
    refresh_hw_device [lindex [get_hw_devices $device] 0]
    program_hw_devices [get_hw_devices $device]
    refresh_hw_device [lindex [get_hw_devices $device] 0]
    set_property OUTPUT_VALUE_RADIX BINARY [get_hw_probes -filter {TYPE == vio_output}]
    set_property OUTPUT_VALUE_RADIX UNSIGNED [get_hw_probes */codeAddr]
    set_property OUTPUT_VALUE_RADIX UNSIGNED [get_hw_probes */gcThres]
    set_property INPUT_VALUE_RADIX BINARY  [get_hw_probes -filter {TYPE == vio_input}]
    return
}

# Write templates to program memory via VIO probes
proc vioWriteTemplates {fname} {

  #  Read template binary file
  set fp [open $fname r]
  set contents [read $fp]
  close $fp

  #  Write to program memory
  set tmpl_addr 0
  foreach tmpl [split $contents "\n"] {

      # Skip trailing empty lines
      if {$tmpl eq ""} {
          break
      }

      # Pack templates into two 256-bit words
      set msb [format {%0*s} 256 [string range $tmpl 0   end-256]]
      set lsb [format {%0*s} 256 [string range $tmpl end-255 end]]
      set addr_string [format {%0*s} [get_property PROBE_PORT_BIT_COUNT [get_hw_probes */codeAddr]] $tmpl_addr]
      incr tmpl_addr

      # Send template
      set_property OUTPUT_VALUE $lsb [get_hw_probes */codeDataLsb]
      set_property OUTPUT_VALUE $msb [get_hw_probes */codeDataMsb]
      set_property OUTPUT_VALUE $addr_string [get_hw_probes */codeAddr]
      set_property OUTPUT_VALUE 1 [get_hw_probes */codeWE]
      commit_hw_vio [get_hw_probes "*/codeDataLsb */codeDataMsb */codeWE */codeAddr "]
  }

  # End template writing
  set gcThres [expr {1 << [get_property PROBE_PORT_BIT_COUNT [get_hw_probes */gcThres]]-3}]
  set_property OUTPUT_VALUE $gcThres [get_hw_probes */gcThres]
  set_property OUTPUT_VALUE 0 [get_hw_probes */codeWE]
  commit_hw_vio [get_hw_probes "*/codeWE */gcThres"]
  after 5000

  # Go!
  set_property OUTPUT_VALUE 1 [get_hw_probes */go]
  commit_hw_vio [get_hw_probes "*/go"]

  # Lower the go flag
  set_property OUTPUT_VALUE 0 [get_hw_probes */go]
  commit_hw_vio [get_hw_probes "*/go"]
  return
}

# Report run results
proc vioReport {} {

    # Block until result is valid
    while 1 {
        # Sleep for 500 ms (in part, because it's the default refresh rate on
        # the VIO... we don't want to mistake a previous run's result for the
        # next one)
        after 500
        set done [get_property INPUT_VALUE [get_hw_probes */retVld]]
        if $done { break }
    }

    set ret [get_property INPUT_VALUE [get_hw_probes */ret]]
    set ret_tag [expr "0b[string range $ret 0 2]"]
    set ret_contents [expr "0b[string range $ret 3 end]"]

    set stats [get_property INPUT_VALUE [get_hw_probes */stats]]
    set cycles_mut          [expr "0b[string range $stats 0   31]"]
    set cycles_gc_root      [expr "0b[string range $stats 32  63]"]
    set cycles_gc_wait      [expr "0b[string range $stats 64  95]"]
    set cycles_gc_max_stall [expr "0b[string range $stats 96 127]"]

    set cycles_gc [expr $cycles_gc_root + $cycles_gc_wait]
    set cycles_total [expr $cycles_gc + $cycles_mut]
    set cycles_productivity [expr 100 * $cycles_mut / $cycles_total]
    puts "\n\nHeron results"
    puts "============="
    puts "Result: tag -> $ret_tag; contents -> $ret_contents"
    puts "Cycles: mut -> $cycles_mut;  GC root id -> $cycles_gc_root;  GC wait -> $cycles_gc_wait; worst stall -> $cycles_gc_max_stall;"
    puts "Total cycles -> $cycles_total"
    puts "Productivity -> $cycles_productivity%"
    return
}

# Run test program
proc vioRun {fname {init false}} {
    if $init {
      # User needs to wait for PYNQ board to fully boot (LEDs 0-5 should flash after ~1 minute)
      # If it hasn't booted yet, the PS clock won't be running and Vivado won't find the VIO debug IP.
      # We don't/can't check that in this script...

      open_hw_manager
      connect_hw_server -allow_non_jtag
      vioBoardSetup xc7z020_1 ./heron_pynqz2_vio.bit ./heron_pynqz2_vio.ltx
    }

    vioWriteTemplates $fname
    vioReport
}

# If we're passed one arg on the command line, interpret it as a binary template
# file and run the test.
if {$argc == 1} {
    vioRun [lindex $argv 0] true
}
