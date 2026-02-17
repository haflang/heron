set_property RAM_STYLE "block" [get_cells *zipWith*RAM*]
#create_clock -name {clk} -period 10.000 -waveform {0.000 5.000} [get_ports {clk}]
create_clock -name {clk} -period 8.333 -waveform {0.000 4.167} [get_ports {clk}]