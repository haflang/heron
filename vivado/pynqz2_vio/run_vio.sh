#!/bin/bash
if [ "$#" -ne 1 ]
then
  echo "ERROR: Expected a binary template file as an argument"
  echo "You can generate the file with 'heron -d <flite_source_file>')"
  exit 1
fi

vivado -mode batch -source run_vio.tcl -tclargs $1
