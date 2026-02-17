set peCols 2
set peRows 4
set fp_xdc ./floorplan.xdc

# This is hard-coded for 2x4 meshes
#
# 0,0 heronPE genRow_lvl6_heronPE_ds1_1
# 1,0 heronPE genRow_lvl6_heronPE_c$x_case_scrut
# 0,1 heronPE genRow_lvl6_heronPE_ds1_4
# 1,1 heronPE genRow_lvl6_heronPE_c$x_case_scrut_0
# 0,2 heronPE genRow_lvl6_heronPE_ds1_7
# 1,2 heronPE genRow_lvl6_heronPE_c$x_case_scrut_1
# 0,3 heronPE genRow_lvl6_heronPE_ds1_10
# 1,3 heronPE genRow_lvl6_heronPE_c$x_case_scrut_2

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
    # x 0 => 0-3, 1 => 3-6
    # Overlap is to ease placement when the XRT shell reserves some space for
    # static logic, usually on either side
    # TODO Needs  generalised for when peCols > 2
    set xl 999
    set xu 999
    if { $x == 0 } {
      set xl 0
      set xu 3
    }
    if { $x == 1 } {
      set xl 3
      set xu 6
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
