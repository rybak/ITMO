#!/bin/bash

shopt -s nullglob

QPATH=$HOME/.nyaqueue

mkdir -p "$QPATH"
mkdir -p "$QPATH/requests"

for a in "$@"
do
    r=`mktemp --tmpdir="$QPATH/requests"`
    echo "$a" > "$r"
done

if [[ ! -f "$QPATH/running" ]]; then
    touch "$QPATH/running"
    trap "rm '$QPATH/running'" 0

    while [ 1 ]
    do
        for a in "$QPATH/requests"/*
        do
            url=`cat "$a"`
            rm "$a"
            wget -c "$url"
        done

        sleep 1
    done
fi
