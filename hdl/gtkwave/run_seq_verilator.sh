#!/usr/bin/env sh

benches="adjoxo.fl coins.fl fib.fl queens_simon.fl sumeuler.fl tak.fl treesum.fl"
for f in bigpar/*.fl
do
  echo $f;
  heron -d "bigparseq/`basename $f`" > /tmp/tmpl2; heron-verilated /tmp/tmpl2 | grep "Main_time\|Returned" | rev | cut -f1 -d' ' | rev;
done
