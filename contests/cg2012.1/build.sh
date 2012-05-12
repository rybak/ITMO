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
    if [ -f CMakeLists.txt ]
    then
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
    else

        echo "Java"
        mkdir bin || true
        javac *.java -d bin
        cp -r bin/* $pdir

        cd bin
        for g in **.class
        do
            echo $g
            pushd $pdir
            echo Running "java ${g/%.class/}"
            
            java ${g/%.class/} || true
            popd
        done
        cd -
    fi

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

