#!/bin/bash

set -e

ok=$@

cmake_build() {
    mkdir bin || true
    cd bin
    cmake ..
    cmake --build .
    cd - 
}

DST_DIR=`pwd`
SRC_DIR=`pwd`/../../cg2012.1/

for p in ${ok}
do
    echo "TASK: $p"
    pdir=$DST_DIR/$p
    mkdir $pdir || true
    mkdir $pdir/performance_tests || true
    mkdir $pdir/correctness_tests || true
    cp task.properties $pdir

    pushd $SRC_DIR/${p}*/

    cd checker
    cmake_build
    cp bin/*check* $pdir/check

    cd ../statement
    cp statement.pdf $pdir/$p.pdf

    cd ../testgen
    cmake_build

    cd bin
    for g in *
    do
        if [ -x $g -a -f $g ]
        then
            cp $g $pdir
            pushd $pdir
            echo Running $g
            ./$g
            popd
        fi
    done
    cd -

#    if [ -x ../testgen ]; then
#        cd ../testgen
#        bin=`pwd`/bin
#        cmake_build
#
#        pushd $DST_DIR/$p
#        cp $bin/*testgen* testgen
#        mkdir performance_tests
#        popd
#    fi

    popd

done

