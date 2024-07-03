HDL_DUT_DIR = $(HERON_VERILOG)/Heron.Board.topEntity
HDL_DUT_SRC = topEntity.v
HDL_DUT_SHIM_SRC  = harness.cpp

OUT_DIR	= build
SIM_BIN	= $(OUT_DIR)/VtopEntity

CPPFLAGS	= -I$(OUT_DIR)
CXXFLAGS	= -fPIC -O3
VERILATOR_FLAGS	= -CFLAGS -O3 -O3 --unroll-count 1024 -Wno-fatal -I$(HDL_DUT_DIR) -Mdir $(OUT_DIR) -j --threads 8 --timing

all: $(SIM_BIN)

clean:
	rm -rf $(OUT_DIR)

.PHONY: all clean

$(OUT_DIR)/VtopEntity.mk: $(HDL_DUT_DIR)/$(HDL_DUT_SRC)
	verilator $(VERILATOR_FLAGS) --cc $(HDL_DUT_SRC) -exe $(HDL_DUT_SHIM_SRC)

$(SIM_BIN): $(OUT_DIR)/VtopEntity.mk
	make -C $(OUT_DIR) -j -f VtopEntity.mk
