#!/bin/bash

if [ $# -eq 0 ];
then RS="28.0"
else RS=$@

./euler f $RS

for i in $RS
do
    export NUM=$i; gnuplot -p oneE.gp
done
