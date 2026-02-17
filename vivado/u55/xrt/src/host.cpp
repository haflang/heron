/**
* Copyright (C) 2019-2021 Xilinx, Inc
*
* Licensed under the Apache License, Version 2.0 (the "License"). You may
* not use this file except in compliance with the License. A copy of the
* License is located at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
* WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
* License for the specific language governing permissions and limitations
* under the License.
*/

#include "cmdlineparser.h"
#include <cstring>
#include <iostream>

// XRT includes
//#include "experimental/xrt_bo.h"
//#include "experimental/xrt_device.h"
//#include "experimental/xrt_kernel.h"
#include "xrt/xrt_bo.h"
#include "xrt/xrt_device.h"
#include <experimental/xrt_xclbin.h>
#include <experimental/xrt_ip.h>
#include <fstream>
#include <bitset>
#include <unistd.h>

// Architecture configuration
#define TEMPLATE_SZ 337
#define GC_THRES 122
#define HEAP_SIZE (8*1024)
#define ATOM_WIDTH 18
#define PINT_WIDTH 15
#define CTRL_RST 0x1
#define CTRL_GO 0x2
#define CTRL_UNLOCK 0x4

// Derived helpers
#define CDIV(x,y) (x/y + (x % y != 0))
#define WORDS_PER_TEMPLATE (CDIV(TEMPLATE_SZ,32))
#define IS_ERR(x) (!(x & (1<<ATOM_WIDTH)))
#define ATOM_MASK ((1<<ATOM_WIDTH)-1)
#define INT_MASK ((1<<PINT_WIDTH)-1)
#define GET_ERR(x) (x&ATOM_MASK)
#define GET_INT(x) (x&INT_MASK)

// Register mapping
#define A_CTRL       0x00
#define A_GO         0x10
#define A_GCTHRES    0x14
#define A_RETVALID   0x18
#define A_RET        0x1C
#define A_STAT_MUT   0x20
#define A_STAT_ROOT  0x24
#define A_STAT_WAIT  0x28
#define A_STAT_STALL 0x2C
#define A_STAT_CTXT  0x30
#define A_CADDR      0x34
#define A_CDATA(x)   (0x38+4*(x))

void write_templates(xrt::ip& krnl, const std::string fname) {

  // Load program binary over codeData/codeAddr
  std::ifstream f(fname);

  int i=0;
  uint t=0,t_addr=0;
  std::string line;


  std::cout << "Starting template writes..." << std::endl;
  // For each template
  while (getline(f, line)) {

    std::bitset<TEMPLATE_SZ> tmpl(line);
    std::bitset<TEMPLATE_SZ> mask{0xFFFFFFFF};

    // Set code address
    krnl.write_register(A_CADDR, t_addr);
        
    // Fill code data regs
    for (i=0; i<WORDS_PER_TEMPLATE; i++){

      t = (tmpl & mask).to_ullong();
      krnl.write_register(A_CDATA(i), t);
      tmpl = tmpl >> 32;
    }

    t_addr++;
  }

  // Bump write address to avoid conflicts
  krnl.write_register(A_CADDR, t_addr);

  return;
}

int main(int argc, char* argv[]) {
    // Command Line Parser
    sda::utils::CmdLineParser parser;

    // Switches
    //**************//"<Full Arg>",  "<Short Arg>", "<Description>", "<Default>"
    parser.addSwitch("--xclbin_file", "-x", "input binary file string", "");
    parser.addSwitch("--device_id", "-d", "device index", "0");
    parser.addSwitch("--gc_thres", "-g", "GC threshold", "128");
    parser.addSwitch("--template_file", "-t", "input template file string", "");
    parser.parse(argc, argv);

    // Read settings
    std::string binaryFile = parser.value("xclbin_file");
    std::string templateFile = parser.value("template_file");
    int device_index = stoi(parser.value("device_id"));
    int gc_thres = stoi(parser.value("gc_thres"));

    if (argc < 4) {
        parser.printHelp();
        return EXIT_FAILURE;
    }

    //xrt::xclbin(binaryFile);
    std::cout << "Open the device" << device_index << std::endl;
    auto device = xrt::device(device_index);
    std::cout << "Load the xclbin " << binaryFile << std::endl;
    auto uuid = device.load_xclbin(binaryFile);
    auto krnl = xrt::ip(device, uuid, "siege_axi");
    
    // Initialise inputs
    krnl.write_register(A_GCTHRES, gc_thres);

    // Pulse reset
    krnl.write_register(A_CTRL, CTRL_RST);
    usleep(1000);
    krnl.write_register(A_CTRL, 0);

    // Load template data into kernel
    write_templates(krnl, templateFile);

    // Send GO then Unlock signals
    krnl.write_register(A_CTRL, CTRL_GO              );
    krnl.write_register(A_CTRL, CTRL_GO | CTRL_UNLOCK);
    krnl.write_register(A_CTRL,           CTRL_UNLOCK);

    // Block until finished
    int cycles = 0;
    std::cout << "Waiting..." << std::endl;
    while (!krnl.read_register(A_RETVALID)) {
        cycles++;
    };

    // Print results
    int ret                = krnl.read_register(A_RET);
    int mutCycles          = krnl.read_register(A_STAT_MUT);
    int gcRootCycles       = krnl.read_register(A_STAT_ROOT);
    int gcWaitCycles       = krnl.read_register(A_STAT_WAIT);
    int gcWorstStallCycles = krnl.read_register(A_STAT_STALL);
    int ctxtCycles         = krnl.read_register(A_STAT_CTXT);
    int totalCycles        = mutCycles + gcRootCycles + gcWaitCycles + ctxtCycles;

    if (IS_ERR(ret))
        std::cout << "Failed with error code: " << GET_ERR(ret) << std::endl;
    else
        std::cout << "Returned " << GET_INT(ret) << std::endl;

    std::cout << "Mutator cycles          = " << mutCycles          << std::endl
              << "GC root id cycles       = " << gcRootCycles       << std::endl
              << "GC wait cycles          = " << gcWaitCycles       << std::endl
              << "Worst GC stall duration = " << gcWorstStallCycles << std::endl
              << "Context switch cycles   = " << ctxtCycles         << std::endl
              << "Total cycles            = " << totalCycles        << std::endl
              << "Raw return              = " << ret                << std::endl;

    if (IS_ERR(ret)) exit(1);
    return 0;

}
