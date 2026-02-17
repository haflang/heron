#!/usr/bin/env sh

# benches="adjoxo.fl coins.fl fib.fl mergesort.fl minimax.fl mss.fl parfact.fl queens.fl quicksort.fl sumeuler.fl tak.fl transclos.fl"
benches="coins.fl fib.fl mergesort.fl minimax.fl mss.fl parfact.fl queens.fl quicksort.fl sumeuler.fl tak.fl transclos.fl treesum.fl"
for f in $benches
do
  echo $f;
  heron -d bigpar/$f > /tmp/tmpl2; heron-verilated /tmp/tmpl2 | grep "Main_time\|Returned" | rev | cut -f1 -d' ' | rev;
done

