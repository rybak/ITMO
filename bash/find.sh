#!/bin/bash
shopt -s dotglob nocaseglob

d="."
n="*"

if [ $# -gt 0 ]; then
    d=$1
    shift
fi

while [ $# -gt 0 ]
do
    case "$1" in
    
    -iname) n=$2
            shift 2
            ;;
    -type)  t=$2
            shift 2
            nc=false
            ;;
    esac
done

function searchw {
    wd=$1
    wd=${wd/%\//}
    search $wd $2
}
deft="d"
#echo "iname=$n"

function search {
    wd=$1
#    echo "search '$1' '$2'"
    t="-${2:-$deft}"
#    echo -e "\tt=$t"
    if [ $t $wd ]; then
        fn=${wd##*/}
#        echo -e "\t1 fn=$fn"
        if [[ "${fn,,}" == ${n,,} ]]; then
            echo $wd
        fi
    fi

    for d in $wd/*
    do
        if [ $t $d ]; then
             fn=${d##*/}
             if [[ "${fn,,}" == ${n,,} ]]; then
                echo $d
             fi
        fi        
    done

    for d in $wd/*
    do
        if [ -d d ]; then
            search $d $2
        fi
    done
}

searchw $d $t

