grep " Stk top" $1 | cut -f2- -d':' | \
	sed 's/Ptr [0-9] [a-z,A-Z]* \([0-9]\)*/h\1/g' | \
	sed 's/Con [0-9]* \([0-9]\)*/C\1/g' | \
	sed 's/PrimOp [0-9] True /swap:/g' | \
	sed 's/PrimOp [0-9] False //g' | \
	sed 's/OpAdd/(+)/g' | \
	sed 's/OpSub/(-)/g' | \
	sed 's/OpEq/(==)/g' | \
	sed 's/OpNeq/(\/=)/g' | \
	sed 's/OpLeq/(<=)/g' | \
	sed 's/OpUnwrap/(unwrap)/g' | \
	sed 's/Fun [0-9]* \([0-9]*\) [a-z,A-Z]*/F\1/g'   | \
	sed 's/PrimInt \([0-9]\)*/\1/g' | \
	sed 's/:> Nil//g' | \
	sed 's/:> //g' | \
	sed 's/h[0-9]*/h/g' | \
	sed 's/F0[ $]//g' | \
	sed 's/ $//g' | \
	sed 's/^ //g'

# flite -s -i1 -h3 -r6:4:2:1:2:16 bigpar/queens.fl | emu -n4 -t - | grep "^Stack :" | cut -f2 -d':' | sed 's/_//g' | sed 's/\*//g' | sed 's/h[0-9]*/h/g' | sed 's/ h $//g' | cut -f1-8 -d' ' > /tmp/coreemu

