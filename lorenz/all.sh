#!/bin/bash

if [ $# -eq 0 ];
then RS="28.0"
else RS=$@
fi

echo "calculating $RS"
./euler f $RS
echo "done"

function call_gnuplot {
    r=$1
    m=$2
    gnuplot -e "NUM=$r" -p "one$m.gp"
}
echo "plotting"
for i in $RS; do
    echo "r = $i"
    for m in EE RK A IE; do
        call_gnuplot $i $m
    done
done
echo "done"

