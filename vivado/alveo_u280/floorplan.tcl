set peCols 2
set peRows 4
set fp_xdc ./floorplan.xdc

proc peName {x y} {
    global peRows
    global peCols
    if { $x == 0 } {
        # We're the first of a row
        set n [expr (3 * $y)+1]
        return "genRow_lvl6_heronPE_ds1_${n}"
    } else {
        # We're in the tail of a row
        # TODO This probably needs generalised for when peCols > 2
        set n [expr $y-1]
        if { [expr $x + $y] == 1 } {
          return "genRow_lvl6_heronPE_c\$x_case_scrut"
        } else {
          return "genRow_lvl6_heronPE_c\$x_case_scrut_${n}"
        }
   }
}

proc peRegion {x y} {
    # x 0 => 0-3, 1 => 4-7
    # TODO Needs  generalised for when peCols > 2
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
for {set y 0} {$y < $peRows} {incr y} {
  for {set x 0} {$x < $peCols} {incr x} {
      set pblockName "pblock_pe_${x}_${y}"
      set cell   [peName $x $y]
      set region [peRegion $x $y]
      puts "SIEGE FLOORPLAN: Mapping ${cell}"
      puts "                 |-> ${region}"

      puts $xdc_fd [subst {create_pblock ${pblockName}}]
      puts $xdc_fd [subst {add_cells_to_pblock ${pblockName} \[get_cells -hierarchical {${cell}}\] -clear_locs}]
      puts $xdc_fd [subst {resize_pblock ${pblockName} -add {${region}}}]
      puts $xdc_fd { }
  }
}
close $xdc_fd

read_xdc $fp_xdc
puts "SIEGE FLOORPLAN: Finished"
