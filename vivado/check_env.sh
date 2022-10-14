type -P vivado \
    && echo "OK. Found vivado at $(which vivado)." \
    || { echo "ERROR: Could not find vivado in PATH."; exit 1; }

[ "$HERON_VERILOG" != "" ] \
    && echo "OK. Found Heron verilog sources in $HERON_VERILOG." \
    || { echo "ERROR: Could not find Heron verilog sources (please set \$HERON_VERILOG environment variable).";
         exit 1; }
