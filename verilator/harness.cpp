#include <verilated.h>
#include "VtopEntity.h"
#include <iostream>
#include <fstream>
#include <bitset>
#include <unistd.h>

using namespace std;

// Architecture configuration
#define TEMPLATE_SZ 337
#define GC_THRES 122
#define HEAP_SIZE (8*1024)
#define ATOM_WIDTH 18
#define INT_WIDTH 15

// Derived helpers
#define CDIV(x,y) (x/y + (x % y != 0))
#define WORDS_PER_TEMPLATE (CDIV(TEMPLATE_SZ,32))
#define IS_ERR(x) (!(x & (1<<ATOM_WIDTH)))
#define ATOM_MASK ((1<<ATOM_WIDTH)-1)
#define INT_MASK ((1<<INT_WIDTH)-1)
#define GET_ERR(x) (x&ATOM_MASK)
#define GET_INT(x) (x&INT_MASK)
#define WAIT_CYCLES(x) for (int i=0; i<(x); i++){ top->clk = 0; top->eval(); top->clk = 1; top->eval(); }

vluint64_t main_time = 1;       // Current simulation time

// Print usage
void usage() {
  cout << "Simulate the Siege core hardware with verilator" << endl << endl
       << "Usage: heron-verilated"            << endl
       << "  [-g int] (GC trigger threshold)" << endl
       << "  [-r int] (Reset delay before `go` signal)"  << endl
       << "  [-d int] (Delay before reset)"  << endl
       << "  file1 ... fileN (Template binaries to run)" << endl << endl
       << "Generate the template binaries using `heron -d <flite_src>.fl`" << endl;
}

// Reset the hardware
void reset(VtopEntity *top, int gc_thres, int wait) {

  // Steady inputs
  top->codeWE = 0;
  top->codeAddr = 0;
  top->go       = 0;
  top->unlock   = 0;
  top->gcThres  = gc_thres;
  top->en       = 1;

  // Wait before reset
  WAIT_CYCLES(wait);

  // Raise reset until everything is reset
  top->rst = 1;
  WAIT_CYCLES(HEAP_SIZE+3);

  // Lower reset for a few cycles
  top->rst = 0;
  WAIT_CYCLES(10);

  return;
}

// Write a binary template file to the code memory and wait until `limit` cycles
void write_templates(VtopEntity *top, char *fname, int limit) {

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
    WAIT_CYCLES(1);
    t_addr++;
  }

  top->codeWE = 0;
  top->codeAddr = 0;

  // Wait until GC has initialised
  while (i < limit) {
    top->clk = 0;
    top->eval();
    top->clk = 1;
    top->eval();
    i++;
  }

  return;
}

// Issue `go` signal
void start(VtopEntity *top) {

  top->codeWE = 0;
  top->go = 1;
  top->unlock = 1;
  WAIT_CYCLES(1);

  top->go = 0;
  return;
}

// Report stats from the master core
void report(VtopEntity *top) {
  int ret = top->ret;
  int mutCycles          = top->stats[4];
  int gcRootCycles       = top->stats[3];
  int gcWaitCycles       = top->stats[2];
  int gcWorstStallCycles = top->stats[1];
  int ctxtCycles         = top->stats[0];

  if (IS_ERR(ret))
    cout << "Failed with error code: " << GET_ERR(ret) << endl;
  else
    cout << "Returned " << GET_INT(ret) << endl;

  cout << "Mutator cycles          = " << mutCycles          << endl
       << "GC root id cycles       = " << gcRootCycles       << endl
       << "GC wait cycles          = " << gcWaitCycles       << endl
       << "Worst GC stall duration = " << gcWorstStallCycles << endl
       << "Context switch cycles   = " << ctxtCycles         << endl
       << "Raw return              = " << ret                << endl
       << "Main_time               = " << main_time          << endl;

  if (IS_ERR(ret))
    exit(1);

  return;
}

// Print intermediate simulation stats
void report_intermediate(VtopEntity *top) {
  cout << "\t\r"    
       << "CTick "  << main_time
       << flush;
  return;
}

// Run simulation until we get a result
void sim(VtopEntity *top, int limit, char *fname) {
  main_time=1;
  write_templates(top, fname, limit);
  cout << "Finished template initialisation" << endl;
  start(top);
  cout << "Waiting for result" << endl;

  int countdown = 0;
  while(!top->retVld || main_time < 5) { // Give it a little grace period to
                                         // reset the result register
    if (countdown==0){
      countdown=5000;
      report_intermediate(top);
    }
    countdown--;
    WAIT_CYCLES(1);
    main_time++;
  }
  cout << endl;

  report(top);
}

// Main loop
int main(int argc, char **argv) {

  int gc_thres = GC_THRES;
  int limit = HEAP_SIZE/2-1;
  int wait = 10;
  int opt;
  int iter=1;
  VtopEntity *top = new VtopEntity;

  while ((opt = getopt(argc, argv, "g:r:d:")) != -1) {
    switch (opt) {
      case 'g':
        gc_thres = atoi(optarg);
        top->gcThres  = gc_thres;
        break;
      case 'r':
        limit = atoi(optarg);
        break;
      case 'd':
        wait = atoi(optarg);
        break;
      default:
        usage();
        return 1;
    }
  }

  // Run any template files
  for (int i = optind; i < argc; i++) {
    cout << "Run " << iter++ << ": " << argv[i] << endl;

    reset(top, gc_thres, wait);
    sim(top, limit, argv[i]);
  }

  top->final();
  delete top;
  return EXIT_SUCCESS;
}
