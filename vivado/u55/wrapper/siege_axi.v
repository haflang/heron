////////////////////////////////////////////////////////////////////////////////
// }}}
// Copyright (C) 2018-2025, Gisselquist Technology, LLC
// {{{
// This file is part of the WB2AXIP project.
//
// The WB2AXIP project contains free software and gateware, licensed under the
// Apache License, Version 2.0 (the "License").  You may not use this project,
// or this file, except in compliance with the License.  You may obtain a copy
// of the License at
// }}}
//	http://www.apache.org/licenses/LICENSE-2.0
// {{{
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
// License for the specific language governing permissions and limitations
// under the License.
//
////////////////////////////////////////////////////////////////////////////////
//
`default_nettype none
`timescale 1 ns / 1 ps
// }}}
module	siege_axi #(
		// {{{
		// Users to add parameters here
		parameter [0:0] OPT_READ_SIDEEFFECTS = 1,
		// User parameters ends
		// Do not modify the parameters beyond this line
		// Width of s_axi_control data bus
		parameter integer C_s_axi_control_DATA_WIDTH	= 32,
		// Width of s_axi_control address bus
		parameter integer C_s_axi_control_ADDR_WIDTH	= 8
		// }}}
	) (
		// {{{
		// Users to add ports here
		// No user ports (yet) in this design
		// User ports ends

		// Do not modify the ports beyond this line
		// Global Clock Signal
		input wire  s_axi_control_ACLK,
		// Global Reset Signal. This Signal is Active LOW
		input wire  s_axi_control_ARESETN,
		// Write address (issued by master, acceped by Slave)
		input wire [C_s_axi_control_ADDR_WIDTH-1 : 0] s_axi_control_AWADDR,
		// Write channel Protection type. This signal indicates the
    		// privilege and security level of the transaction, and whether
    		// the transaction is a data access or an instruction access.
		input wire [2 : 0] s_axi_control_AWPROT,
		// Write address valid. This signal indicates that the master
		// signaling valid write address and control information.
		input wire  s_axi_control_AWVALID,
		// Write address ready. This signal indicates that the slave
		// is ready to accept an address and associated control signals.
		output wire  s_axi_control_AWREADY,
		// Write data (issued by master, acceped by Slave)
		input wire [C_s_axi_control_DATA_WIDTH-1 : 0] s_axi_control_WDATA,
		// Write strobes. This signal indicates which byte lanes hold
    		// valid data. There is one write strobe bit for each eight
    		// bits of the write data bus.
		input wire [(C_s_axi_control_DATA_WIDTH/8)-1 : 0] s_axi_control_WSTRB,
		// Write valid. This signal indicates that valid write
    		// data and strobes are available.
		input wire  s_axi_control_WVALID,
		// Write ready. This signal indicates that the slave
    		// can accept the write data.
		output wire  s_axi_control_WREADY,
		// Write response. This signal indicates the status
    		// of the write transaction.
		output wire [1 : 0] s_axi_control_BRESP,
		// Write response valid. This signal indicates that the channel
    		// is signaling a valid write response.
		output wire  s_axi_control_BVALID,
		// Response ready. This signal indicates that the master
    		// can accept a write response.
		input wire  s_axi_control_BREADY,
		// Read address (issued by master, acceped by Slave)
		input wire [C_s_axi_control_ADDR_WIDTH-1 : 0] s_axi_control_ARADDR,
		// Protection type. This signal indicates the privilege
    		// and security level of the transaction, and whether the
    		// transaction is a data access or an instruction access.
		input wire [2 : 0] s_axi_control_ARPROT,
		// Read address valid. This signal indicates that the channel
    		// is signaling valid read address and control information.
		input wire  s_axi_control_ARVALID,
		// Read address ready. This signal indicates that the slave is
    		// ready to accept an address and associated control signals.
		output wire  s_axi_control_ARREADY,
		// Read data (issued by slave)
		output wire [C_s_axi_control_DATA_WIDTH-1 : 0] s_axi_control_RDATA,
		// Read response. This signal indicates the status of the
    		// read transfer.
		output wire [1 : 0] s_axi_control_RRESP,
		// Read valid. This signal indicates that the channel is
    		// signaling the required read data.
		output wire  s_axi_control_RVALID,
		// Read ready. This signal indicates that the master can
    		// accept the read data and response information.
		input wire  s_axi_control_RREADY
		// }}}
	);

	// Local declarations
	// {{{
	// AXI4LITE signals
	reg		axi_awready;
	reg		axi_wready;
	reg		axi_bvalid;
	reg		axi_arready;
	reg [C_s_axi_control_DATA_WIDTH-1 : 0] 	axi_rdata;
	reg		axi_rvalid;

	// Example-specific design signals
	// local parameter for addressing 32 bit / 64 bit C_s_axi_control_DATA_WIDTH
	// ADDR_LSB is used for addressing 32/64 bit registers/memories
	// ADDR_LSB = 2 for 32 bits (n downto 2)
	// ADDR_LSB = 3 for 64 bits (n downto 3)
	localparam integer ADDR_LSB = 2;
	localparam integer AW = C_s_axi_control_ADDR_WIDTH-2;
	localparam integer DW = C_s_axi_control_DATA_WIDTH;
	//----------------------------------------------
	//-- Signals for user logic register space example
	//------------------------------------------------
	reg [DW-1:0]	slv_mem	[0:63];

	wire dut_rst;
	wire [18:0] dut_ret;
	wire [159:0] dut_stats;
	wire dut_retVld;
	wire [336:0] dut_cdata;
    assign dut_rst	= !s_axi_control_ARESETN;
    assign dut_cdata = {slv_mem[24][16:0], slv_mem[23], slv_mem[22], slv_mem[21], slv_mem[20], slv_mem[19], slv_mem[18], slv_mem[17], slv_mem[16], slv_mem[15], slv_mem[14] };

	// I/O Connections assignments

	assign s_axi_control_AWREADY	= axi_awready;
	assign s_axi_control_WREADY	= axi_wready;
	assign s_axi_control_BRESP	= 2'b00; // The OKAY response
	assign s_axi_control_BVALID	= axi_bvalid;
	assign s_axi_control_ARREADY	= axi_arready;
	assign s_axi_control_RDATA	= axi_rdata;
	assign s_axi_control_RRESP	= 2'b00; // The OKAY response
	assign s_axi_control_RVALID	= axi_rvalid;
	// Implement axi_*wready generation
	// }}}
	//////////////////////////////////////
	//
	// Read processing
	//
	//
	wire	valid_read_request,
		read_response_stall;

	assign	valid_read_request  =  s_axi_control_ARVALID || !s_axi_control_ARREADY;
	assign	read_response_stall =  s_axi_control_RVALID  && !s_axi_control_RREADY;

	//
	// The read response channel valid signal
	//
	initial	axi_rvalid = 1'b0;
	always @(posedge s_axi_control_ACLK )
	if (!s_axi_control_ARESETN)
		axi_rvalid <= 0;
	else if (read_response_stall)
		// Need to stay valid as long as the return path is stalled
		axi_rvalid <= 1'b1;
	else if (valid_read_request)
		axi_rvalid <= 1'b1;
	else
		// Any stall has cleared, so we can always
		// clear the valid signal in this case
		axi_rvalid <= 1'b0;

	reg [C_s_axi_control_ADDR_WIDTH-1 : 0] 	pre_raddr, rd_addr;

	// Buffer the address
	always @(posedge s_axi_control_ACLK)
	if (s_axi_control_ARREADY)
		pre_raddr <= s_axi_control_ARADDR;

	always @(*)
	if (!axi_arready)
		rd_addr = pre_raddr;
	else
		rd_addr = s_axi_control_ARADDR;

	//
	// Read the data
	//
	always @(posedge s_axi_control_ACLK)
	if (!read_response_stall
		&&(!OPT_READ_SIDEEFFECTS || valid_read_request))
		// If the outgoing channel is not stalled (above)
		// then read
		axi_rdata <=
		  (rd_addr[AW+ADDR_LSB-1:ADDR_LSB] == 6'h6) ? {31'h0, dut_retVld} :
		  (rd_addr[AW+ADDR_LSB-1:ADDR_LSB] == 6'h7) ? {13'h0, dut_ret}    :
		  (rd_addr[AW+ADDR_LSB-1:ADDR_LSB] == 6'h8) ? dut_stats[159:128]  :
		  (rd_addr[AW+ADDR_LSB-1:ADDR_LSB] == 6'h9) ? dut_stats[127:96]   :
		  (rd_addr[AW+ADDR_LSB-1:ADDR_LSB] == 6'hA) ? dut_stats[95:64]    :
		  (rd_addr[AW+ADDR_LSB-1:ADDR_LSB] == 6'hB) ? dut_stats[63:32]    :
		  (rd_addr[AW+ADDR_LSB-1:ADDR_LSB] == 6'hC) ? dut_stats[31:0]     :
		  slv_mem[rd_addr[AW+ADDR_LSB-1:ADDR_LSB]];

	//
	// The read address channel ready signal
	//
	initial	axi_arready = 1'b0;
	always @(posedge s_axi_control_ACLK)
	if (!s_axi_control_ARESETN)
		axi_arready <= 1'b1;
	else if (read_response_stall)
	begin
		// Outgoing channel is stalled
		//    As long as something is already in the buffer,
		//    axi_arready needs to stay low
		axi_arready <= !valid_read_request;
	end else
		axi_arready <= 1'b1;

	//////////////////////////////////////
	//
	// Write processing
	//
	//
	reg [C_s_axi_control_ADDR_WIDTH-1 : 0]		pre_waddr, waddr;
	reg [C_s_axi_control_DATA_WIDTH-1 : 0]		pre_wdata, wdata;
	reg [(C_s_axi_control_DATA_WIDTH/8)-1 : 0]	pre_wstrb, wstrb;

	wire	valid_write_address, valid_write_data,
		write_response_stall;

	assign	valid_write_address = s_axi_control_AWVALID || !axi_awready;
	assign	valid_write_data    = s_axi_control_WVALID  || !axi_wready;
	assign	write_response_stall= s_axi_control_BVALID  && !s_axi_control_BREADY;

	//
	// The write address channel ready signal
	//
	initial	axi_awready = 1'b1;
	always @(posedge s_axi_control_ACLK)
	if (!s_axi_control_ARESETN)
		axi_awready <= 1'b1;
	else if (write_response_stall)
	begin
		// The output channel is stalled
		//	If our buffer is full, we need to remain stalled
		//	Likewise if it is empty, and there's a request,
		//	  we'll need to stall.
		axi_awready <= !valid_write_address;
	end else if (valid_write_data)
		// The output channel is clear, and write data
		// are available
		axi_awready <= 1'b1;
	else
		// If we were ready before, then remain ready unless an
		// address unaccompanied by data shows up
		axi_awready <= ((axi_awready)&&(!s_axi_control_AWVALID));
		// This is equivalent to
		// axi_awready <= !valid_write_address

	//
	// The write data channel ready signal
	//
	initial	axi_wready = 1'b1;
	always @(posedge s_axi_control_ACLK)
	if (!s_axi_control_ARESETN)
		axi_wready <= 1'b1;
	else if (write_response_stall)
		// The output channel is stalled
		//	We can remain ready until valid
		//	write data shows up
		axi_wready <= !valid_write_data;
	else if (valid_write_address)
		// The output channel is clear, and a write address
		// is available
		axi_wready <= 1'b1;
	else
		// if we were ready before, and there's no new data avaialble
		// to cause us to stall, remain ready
		axi_wready <= (axi_wready)&&(!s_axi_control_WVALID);
		// This is equivalent to
		// axi_wready <= !valid_write_data


	// Buffer the address
	always @(posedge s_axi_control_ACLK)
	if (s_axi_control_AWREADY)
		pre_waddr <= s_axi_control_AWADDR;

	// Buffer the data
	always @(posedge s_axi_control_ACLK)
	if (s_axi_control_WREADY)
	begin
		pre_wdata <= s_axi_control_WDATA;
		pre_wstrb <= s_axi_control_WSTRB;
	end

	always @(*)
	if (!axi_awready)
		// Read the write address from our "buffer"
		waddr = pre_waddr;
	else
		waddr = s_axi_control_AWADDR;

	always @(*)
	if (!axi_wready)
	begin
		// Read the write data from our "buffer"
		wstrb = pre_wstrb;
		wdata = pre_wdata;
	end else begin
		wstrb = s_axi_control_WSTRB;
		wdata = s_axi_control_WDATA;
	end

	//
	// Actually (finally) write the data
	//
	always @(posedge s_axi_control_ACLK )
	// If the output channel isn't stalled, and
	if (!write_response_stall
		// If we have a valid address, and
		&& valid_write_address
		// If we have valid data
		&& valid_write_data)
	begin
		   if (wstrb[0])
			 slv_mem[waddr[AW+ADDR_LSB-1:ADDR_LSB]][7:0]
			   <= wdata[7:0];
		   if (wstrb[1])
			 slv_mem[waddr[AW+ADDR_LSB-1:ADDR_LSB]][15:8]
			   <= wdata[15:8];
		   if (wstrb[2])
			 slv_mem[waddr[AW+ADDR_LSB-1:ADDR_LSB]][23:16]
			   <= wdata[23:16];
		   if (wstrb[3])
			 slv_mem[waddr[AW+ADDR_LSB-1:ADDR_LSB]][31:24]
			   <= wdata[31:24];
	end

	//
	// The write response channel valid signal
	//
	initial	axi_bvalid = 1'b0;
	always @(posedge s_axi_control_ACLK )
	if (!s_axi_control_ARESETN)
		axi_bvalid <= 1'b0;
	//
	// The outgoing response channel should indicate a valid write if ...
		// 1. We have a valid address, and
	else if (valid_write_address
			// 2. We had valid data
			&& valid_write_data)
		// It doesn't matter here if we are stalled or not
		// We can keep setting ready as often as we want
		axi_bvalid <= 1'b1;
	else if (s_axi_control_BREADY)
		// Otherwise, if BREADY was true, then it was just accepted
		// and can return to idle now
		axi_bvalid <= 1'b0;

	// Make Verilator happy
	// Verilator lint_off UNUSED
	wire	[4*ADDR_LSB+5:0]	unused;
	assign	unused = { s_axi_control_AWPROT, s_axi_control_ARPROT,
				s_axi_control_AWADDR[ADDR_LSB-1:0],
				rd_addr[ADDR_LSB-1:0],
				waddr[ADDR_LSB-1:0],
				s_axi_control_ARADDR[ADDR_LSB-1:0] };
	// Verilator lint_on UNUSED

	// Add user logic here

    topEntity dut (
      .clk(s_axi_control_ACLK),
      .rst(slv_mem[0][0:0]),
      .en(1),
      .codeWE(1),
      .codeAddr(slv_mem[13][9:0]),
      .go(slv_mem[0][1:1]),
      .unlock(slv_mem[0][2:2]),
      .gcThres(slv_mem[5][12:0]),
      .codeData(dut_cdata),
      .ret(dut_ret),
      .retVld(dut_retVld),
      .stats(dut_stats)
    );

   endmodule
