#!/bin/bash

set -e

ok="01 04 09"

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
    echo $p
    cd $SRC_DIR/checkers/${p}*
    bin=`pwd`/bin
    cmake_build
    mkdir $DST_DIR/$p || true

    pushd $DST_DIR/$p
    cp $bin/*check* check
    popd

    cd ../../statements/${p}*
    cp statement.pdf $DST_DIR/$p

done

