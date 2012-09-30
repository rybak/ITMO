#!/bin/bash
shopt -s dotglob nocaseglob

d="."
n="*"
debug=false

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
    if [ $debug = true ]; then echo -e "1 $#";
    fi
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
    if [ $debug = true ]; then echo "search '$1' '$2'";fi
    t="-${2:-$deft}"
    if [ $debug == true ]; then echo -e "\t1 t=$t";fi
    if [ $t $wd ]; then
        fn=${wd##*/}
        if [ $debug == true ]; then echo -e "\t2 fn=$fn";fi
        if [[ "${fn,,}" == ${n,,} ]]; then
            echo $wd
        fi
    fi

    for d in $wd/*
    do
        if [ $t $d ] && [ ! -d $d ]; then
             fn=${d##*/}
             if [[ "${fn,,}" == ${n,,} ]]; then
                echo $d
             fi
        fi        
    done

    for nd in $wd/*
    do
        if [ -d $nd ]; then
            search $nd $2
        fi
    done
}

searchw $d $t

