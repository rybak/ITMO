#!/bin/sh

cd pic
for i in *.mp
do
    echo "FILE $i"
    if [ -f "$i" ];
    then
        fn=${i%.*}
        mpost $fn.mp
        mv $fn.1 $fn.eps
        mptopdf --latex $fn.mp
    fi
done
cd ../

