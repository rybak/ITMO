#!/bin/bash

if [ $# -eq 0 ];
then RS="28.0"
else RS=$@
fi

./euler f $RS

for i in $RS; do
    echo $i
    gnuplot -e "NUM=$1" -p oneE.gp;
done

