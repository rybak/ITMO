#!/bin/sh

cd pic
for i in *.mp
do
    if [ -f "$i" ];
    then
        fn=${i%.*}
        mpost $fn.mp
        #- mv $fn.1 $fn.eps
        mptopdf --latex $fn.mp
    fi
done
cd ../

