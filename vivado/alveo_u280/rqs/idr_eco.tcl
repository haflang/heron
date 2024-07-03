phys_opt_design -clock_opt -retime -lut_opt
place_design -directive LastMile
phys_opt_design -directive Explore
route_design -directive Explore
