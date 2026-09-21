#!/usr/bin/env sh

grep -o "writePortsPush .*" /tmp/heron_sim.log | grep -o "Just MFetch [^:]*" | uniq | cut -f3,4 -d' ' > /tmp/hfetchs.log
grep -o "writePortsPush .*" /tmp/heron_sim.log | grep -o "Just MPut [^)]*) [^)]*)"  | uniq | sed 's/Just MPut //' > /tmp/hputs.log
grep " cReq = Just CMStart" /tmp/heron_sim.log | sed 's/  cReq = Just CMStart //' > /tmp/hsparks.log

diff <(cat /tmp/hfetchs.log | cut -f1 -d' ') <(cat /tmp/hputs.log | cut -f1 -d' ')
