#!/usr/bin/env sh

benches="adjoxo.fl coins.fl fib.fl minimax.fl mss.fl ordlist.fl queens.fl queens_simon.fl quicksort.fl sumeuler.fl tak.fl transclos.fl treesum.fl warshall.fl"
for f in $benches
do
  echo $f;
  cabal run hdl -- -a "hdl/tests/benchmarks/par/$f";
done
