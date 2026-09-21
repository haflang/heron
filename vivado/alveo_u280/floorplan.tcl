set peCols 4
set peRows 2
set peMeshes 3
set fp_xdc ./floorplan.xdc

proc peName {x y} {
    # Get filename of top-level verilog
    set dut_src [get_files "topEntity.v"]

    # Get the directory of the current script
    set script_dir [file dirname [file normalize [info script]]]

    # Build the path to bash mapping script
    set bash_script [file join $script_dir "pe_map.sh"]

    # Return instance name
    set name [exec $bash_script $dut_src $x $y]
    return $name
}

proc peRegion {x y} {
    # x : 0 => 0-3, 1 => 4-7
    # TODO Needs generalised for when peCols > 2
    set xl 999
    set xu 999
    if { $x == 0 } {
      set xl 0
      set xu 3
    }
    if { $x == 1 } {
      set xl 4
      set xu 7
    }
    # y 0 => 11, 1 => 10, ...
    set yl [expr 11-$y]
    return "CLOCKREGION_X${xl}Y${yl}:CLOCKREGION_X${xu}Y${yl}"
}

set xdc_fd [ open $fp_xdc w ]
for {set s 0} {$s < $peMeshes} {incr s} {
  for {set r 0} {$r < $peRows} {incr r} {
    for {set c 0} {$c < $peCols} {incr c} {

      # Derive pblock coordinates
      set block_x [expr $r]
      set block_y [expr $c + $s * $peCols]
      set pblockName "pblock_pe_${block_x}_${block_y}"

      # Derive PE id coordinates
      set pe_x    [expr $block_y % $peCols]
      set pe_y    [expr $block_x + $s * $peRows]
      set cell    [peName $pe_x $pe_y]

      # Derive region TODO Continue from here!
      set region [peRegion $block_x $block_y]
      puts "SIEGE FLOORPLAN: Mapping ${cell}"
      puts "                 ::  block ${pblockName}"
      puts "                 |-> pe              ${pe_x},${pe_y}"
      puts "                 |-> ${region}"

      puts $xdc_fd [subst {create_pblock ${pblockName}}]
      puts $xdc_fd [subst {add_cells_to_pblock ${pblockName} \[get_cells -hierarchical {${cell}}\] -clear_locs}]
      puts $xdc_fd [subst {resize_pblock ${pblockName} -add {${region}}}]
      puts $xdc_fd { }
      }
  }
}
close $xdc_fd

read_xdc $fp_xdc
puts "SIEGE FLOORPLAN: Finished"
