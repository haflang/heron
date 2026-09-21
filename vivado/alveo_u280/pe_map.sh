#!/bin/bash
: '
This script looks at a Siege topEntity verilog file and creates a map of PE
coordinates to their clash generated instance names.

Very useful for floorplanning scripts.
It assumes the peIn structures and the heronPE instantiations are in the same
order.
'

if [ $# -ne 3 ]
then echo "Usage: gen_pe_map.sh {topEntity.v} {target_x} {target_y}"
     echo "  Will return the component name of the (target_x,target_y) PE instance."
fi

TOP_SRC=$1 #"../../verilog_mask_4x2x3/Heron.Board.topEntity/topEntity.v"
TARGET_X=$2
TARGET_Y=$3
IDS_FILE="/tmp/heron_pe_ids"
NAMES_FILE="/tmp/heron_pe_names"

# Find PE Ids
(
grep -A 5 "assign peIn" "$TOP_SRC" | # Find PE input definitions
    sed -z "s/\n/ /g"              | # Ignore newlines in case the ID is split
    grep -o "{[0-9' d,]*}};"       | # Find just the ID sections
    tr -s ' '                      | # Compress any gaps
    sed "s/{[0-9]'d\([0-9]\),[ ]*[0-9]'d\([0-9]\)}};/\1,\2/" # Extract IDs
) > "$IDS_FILE"

# Find PE Names
grep heronPE "$TOP_SRC" | cut -f4 -d' ' > "$NAMES_FILE"

# Find name for target ID
paste -d"," "$IDS_FILE" "$NAMES_FILE" | sort -n | grep "^$TARGET_X,$TARGET_Y," | cut -f3 -d','
