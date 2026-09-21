#!/bin/bash

if [ -z "$1" ]
  then
    echo "No source file specified"
    exit 1
fi
SRC=$1
ANS=`flite -r6:4:2:1:2:16 -h3 -i1 -s $SRC | emu -n4 - | cut -f1 -d',' | tr -d '( '`
TMPL=$(mktemp /tmp/verilator_tmpl.XXXXXX)
heron -d "$SRC" > "$TMPL"

echo "Expecting return of $ANS for $SRC"
for gc in {60..256}; do
    echo -ne "Trying with $gc \r"
    heron-verilated "$TMPL" -g $gc -r 8192 | grep -c "Returned $ANS" > /dev/null
    if [ $? -ne 0 ]; then
        echo "Script failed with argument: $gc"
        rm "$TMPL"
        exit 1
    fi
done

echo -e "\e[KAll passed"
rm "$TMPL"
