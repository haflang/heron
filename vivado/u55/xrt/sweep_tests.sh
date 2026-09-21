#!/bin/bash

GC_THRES=2000
PAR_BINS=./fl/par/*.bin
SEQ_BINS=./fl/seq/*.bin

# Run SEQ programs as a baseline
for b in $SEQ_BINS; do
    echo "$b"
    ./siege_xrt_host -x siege_container.xclbin -g ${GC_THRES} -t ${b}
done > "./seq.log"

# Setup dictionary mapping #PEs to mask bits
# Works for up to 24 cores, assuming 4x2 per SLR
declare -A mask_map
mask_map[1]=0x800000
mask_map[2]=0xC00000
mask_map[4]=0xCC0000
mask_map[6]=0xEE0000
mask_map[8]=0xFF0000
mask_map[16]=0xFFFF00
mask_map[24]=0xFFFFFF

# Iterate of number of PEs
for pes in "${!mask_map[@]}"; do
    # Find the mask for these PEs
    mask=${mask_map[$pes]}
    for b in $PAR_BINS; do
        echo "$b"
        ./siege_xrt_host -x siege_container.xclbin -g ${GC_THRES} -m ${mask} -t ${b}
    done > "./par_${pes}.log"
done
