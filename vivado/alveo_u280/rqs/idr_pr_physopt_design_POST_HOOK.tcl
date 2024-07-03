set fp [file dirname [file normalize [info script]]]
set src_rc [catch {source [file join $fp idr_eco.tcl]} _RESULT]
if {$src_rc} {
  puts "ERROR: $_RESULT"
}
