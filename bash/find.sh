#!/bin/bash

d="."
n="*"

if [ $# -gt 0 ]; then
    d=$1
    shift
fi

if [ ! -d $d ] && [ ! -f $d ]; then
    echo "$d is not a directory"
    exit 1
fi

while [ $# -gt 0 ]
do
    case "$1" in
    
    -iname) n=$2
            shift 2
            ;;
    -type)  t=$2
            shift 2
            ;;
    esac
done

function searchw {
    wd=$1
    wd=${wd/%\//}
    search $wd $2
}

td="d"
tf="f"

function search {
    wd=$1
    t="-${2:-$td}"

    if [ $t $wd ]; then
        fn=${wd##*/}
        if [[ ${fn,,} == ${n,,} ]]; then
            echo $wd
        fi
    fi

    t="-${2:-$tf}"

    if [ $t == "-f" ]; then
        for f in $wd/*
        do
            if  [ $t $f ] && [ ! -d $f ]; then
                fn=${f##*/}
                if [[ ${fn,,} == ${n,,} ]]; then
                    echo $f
                fi
            fi
        done
    fi

    for nd in $wd/*
    do
        if [ -d $nd ]; then
            search $nd $2
        fi
    done
}

searchw $d $t

