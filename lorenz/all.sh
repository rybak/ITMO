#!/bin/bash


for i in $@
do
    export NUM=$i; gnuplot oneE.gp
done
