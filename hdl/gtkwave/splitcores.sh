#!/usr/bin/env sh

for x in `seq 0 1`
do for y in `seq 0 3`
    do
    echo "Processing core ($x,$y)"
    grep -A45 "Core ($x,$y)" /tmp/heron.log | awk '{printf("%04d %s\n", NR/47, $0)}' > "/tmp/heron_core_($x,$y).log"
    #grep -A32 "Core $n " /tmp/heron_sim.log | grep -o "mutComRequest = .* mutIdle" | cat --number | grep Just > /tmp/heron_core_$n.log
    #grep -A32 "Core $n " /tmp/heron_sim.log | grep -o "comMutRequest = .* _mutComReqPop" | cat --number | grep Just >> /tmp/heron_core_$n.log
    # sort -n /tmp/heron_core_$n.log | sponge /tmp/heron_core_$n.log
    grep "cReq = Just \| comReq = Just" "/tmp/heron_core_($x,$y).log"
    echo "--------------------"
   done
done

# grep -A31 "Core Root" /tmp/heron_sim.log > /tmp/heron_core_root.log
# grep -A31 "Core Left" /tmp/heron_sim.log > /tmp/heron_core_left.log
# grep -A31 "Core Right" /tmp/heron_sim.log > /tmp/heron_core_right.log

# Extract all resumes (sorting ensures we can uniq without getting duplicates due to multiple cores resuming simultaneously)
# grep "ComC  Ins .*Just Resume" /tmp/heron_sim.log | cut -f1,2,3 -d',' | sed 's/\((.,.)\)@.*Just Resume \([0-9]*\).*/\1 \2/' | sort -s -k1,1 | uniq > /tmp/heron_resumes.log
# grep "ComC  Out .*_readysPush = Just" /tmp/heron_sim.log | sed 's/\((.,.)\)@.*_readysPush = Just \([0-9]*\).*/\1 \2/' | sort -s -k1,1 | uniq > /tmp/heron_resumes.log
# Local suspends
# grep "cmd = Just MCLocalSuspend" /tmp/heron_sim.log | sed 's/\((.,.)\)@.*LocalSuspend \([0-9]*\).*/\1 \2/' | sort -s -k1,1 | uniq > /tmp/heron_localsuspends.log
# Remote suspends
# grep -o ".* ComC  Ins : ContextIn {cmd = Just Suspend [0-9]*" /tmp/heron_sim.log | cut -f1,11 -d' ' | tr -d '@' | sort -s -k1,1 | uniq > /tmp/heron_suspends.log

grep "comContextCmd = Just Unblock" /tmp/heron_sim.log | sed 's/\((.,.)\)@.*comContextCmd = Just Unblock \([0-9]*\).*/\1 \2/' | sort -s -k1,1 > /tmp/heron_resumes.log
grep "comContextCmd = Just PushBlock" /tmp/heron_sim.log | sed 's/\((.,.)\)@.*comContextCmd = Just PushBlock \([0-9]*\).*/\1 \2/' | sort -s -k1,1 > /tmp/heron_suspends.log

echo "Reporting unblocked TSOs"
diff <(cat /tmp/heron_suspends.log | sort -n) <(sort /tmp/heron_resumes.log | sort -n)

# # Dump GC's view of references and tsos
# grep "rcNextRef = Just" /tmp/heron_core_\(0,0\).log | sed 's/\([0-9]*\) .* rcNextRef = Just \(.*\), threadReady.*/\1 \2/'
# grep "threadRet = Just" /tmp/heron_core_\(0,0\).log | sed 's/\([0-9]*\) .* threadRet = Just \(.*\)}/\1 \2/' | less
