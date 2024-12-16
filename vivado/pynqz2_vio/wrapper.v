module wrapper (
    FIXED_IO_mio,
    FIXED_IO_ps_clk,
    FIXED_IO_ps_porb,
    FIXED_IO_ps_srstb
);

  inout wire [53 : 0] FIXED_IO_mio;
  inout wire FIXED_IO_ps_clk;
  inout wire FIXED_IO_ps_porb;
  inout wire FIXED_IO_ps_srstb;

   wire rstn;
   wire rst;
   wire clk;

   assign rst = ~rstn;

   topWithVIO dut
     ( .clk (clk)
     , .rst (rst)
     );

   zynq_ps ps
     ( .FCLK_CLK0 (clk)
     , .FCLK_RESET0_N (rstn)
     // Fixed PS interfaces below
     , .MIO(FIXED_IO_mio[53:0])
     , .PS_CLK(FIXED_IO_ps_clk)
     , .PS_PORB(FIXED_IO_ps_porb)
     , .PS_SRSTB(FIXED_IO_ps_srstb)
     );

endmodule
