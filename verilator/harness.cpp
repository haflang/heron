#include <verilated.h>
#include "VtopEntity.h"
#include <iostream>
#include <fstream>
#include <bitset>

using namespace std;

// Here are our interface lists for _one_ configuration... how to do we
// automatically generalise for the code data and gc threshold widths?
#define TEMPLATE_SZ 326
#define GC_THRES 1000
#define WORDS_PER_TEMPLATE 11
#define ERR_HEAP_FULL 0x27ffc

vluint64_t main_time = 1;       // Current simulation time

void reset(VtopEntity *top) {

  // Steady inputs
  top->codeWE = 0;
  top->codeAddr = 0;
  top->go       = 0;
  top->gcThres  = GC_THRES;
  top->en       = 1;

  // Raise reset
  top->rst      = 1;
  for(int i=0; i<=30; i++){
    top->clk = 0;
    top->eval();
    top->clk = 1;
    top->eval();
  }

  // Lower reset
  top->rst = 0;
  for(int i=0; i<=30; i++){
    top->clk = 0;
    top->eval();
    top->clk = 1;
    top->eval();
  }

  return;
}

void write_templates(VtopEntity *top, char *fname) {

  // Load program binary over codeData/codeAddr
  ifstream f(fname);

  int i=0;
  uint t=0,t_addr=0;
  string line;

  // For each template
  while (getline(f, line)) {

    bitset<TEMPLATE_SZ> tmpl(line);
    bitset<TEMPLATE_SZ> mask{0xFFFFFFFF};

    // Fill codeData buffer
    for (i=0; i<WORDS_PER_TEMPLATE; i++){
      top->codeData[i] = (tmpl & mask).to_ullong();
      tmpl = tmpl >> 32;
    }

    // Commit to template RAM
    top->codeWE = 1;
    top->codeAddr = t_addr;
    top->clk = 0;
    top->eval();
    top->clk = 1;
    top->eval();
    t_addr++;
  }

  return;
}

void start(VtopEntity *top) {

  top->codeWE = 0;
  top->go = 1;

  // Block until we see that the mutator has started
  while (top->stats[3]==0) {
    top->clk = 0;
    top->eval();
    top->clk = 1;
    top->eval();
  }

  top->go = 0;
  top->eval();
  return;
}

void report(VtopEntity *top) {
  int ret = top->ret;
  int mutCycles = top->stats[3];
  int gcRootCycles = top->stats[2];
  int gcWaitCycles = top->stats[1];
  int gcWorstStallCycles = top->stats[0];

  if (ret == ERR_HEAP_FULL)
    cout << "Failed with ERR_HEAP_FULL" << endl;
  else
    cout << "Returned " << (ret & 0x7FFF) << endl;

  cout << "Mutator cycles = "          << mutCycles    << endl
       << "GC root id cycles = "       << gcRootCycles << endl
       << "GC wait cycles = "          << gcWaitCycles << endl
       << "Worst GC stall duration = " << gcWorstStallCycles << endl;
  return;
}

void report_intermediate(VtopEntity *top) {
  int ret = top->ret;
  int mutCycles = top->stats[3];
  int gcRootCycles = top->stats[2];
  int gcWaitCycles = top->stats[1];
  int gcWorstStallCycles = top->stats[0];

  cout << "CTick "  << main_time
       << " MUT "   << mutCycles
       << " ROOTS " << gcRootCycles
       << " WAITS " << gcWaitCycles
       << " STALL " << gcWorstStallCycles
       << "\t\r"    << flush;
  return;
}

int main(int argc, char **argv) {

  if (argc != 2) {
    cout << "Usage: " << argv[0] << " <template_bin_file>" << endl;
    cout << "Generate the template file using `heron -d <flite_src>.fl`" << endl;
    return EXIT_FAILURE;
  }

  //Verilated::commandArgs(argc, argv);
  printf("Starting simulation...\n");

  VtopEntity *top = new VtopEntity;

  reset(top);
  cout << "Finished reset" << endl;

  write_templates(top, argv[1]);
  cout << "Finished template initialisation" << endl;

  start(top);
  cout << "Waiting for result" << endl;

  while(!top->retVld) {
    report_intermediate(top);
    top->clk = 0;
    top->eval();
    top->clk = 1;
    top->eval();
    main_time++;
  }
  cout << endl;

  report(top);
  top->final();
  delete top;
  return EXIT_SUCCESS;
}
