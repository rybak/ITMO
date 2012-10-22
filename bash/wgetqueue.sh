#!/bin/bash

shopt -s nullglob
function dpid {
    cat "$DAEMON"
}

function daemonerror {
    pid=`cat "$DAEMON"`
    echo -n `date` >> $ELOG
    echo -n " $$ " >> $ELOG
    echo "$1 PID = $pid" >> $ELOG
    exit 255
}

function daemon {
    trap "" SIGHUP
    while true
    do
        for a in "$QPATH/requests"/*
        do
            url=`cat "$a"`
            rm "$a"
            wget -c "$url" 2> $ELOG
        done
        sleep 5
    done
}

function usage {
    echo "Usage"
    echo "./wgetqueue.sh [Flags | URL]"
    echo "URL"
    echo -e "\tIf URL is given, wgetqueue.sh adds URL to queue."
    echo "Flags"
    echo -e "\t-c\n\t\tCheck if daemon is running."
    echo -e "\t-k\n\t\tKill daemon."
    echo -e "\t-h\n\t\tPrint this help."
    echo ""
}

if [[ $# -eq 0 ]] || [[ $1 == "-h" ]]; then
    usage
    exit
fi

QPATH=$HOME/.nyaqueue
ELOG=$QPATH/error.log
DAEMON=$QPATH/daemonrunning
REQUESTS=$QPATH/requests
ATOM=$QPATH/atom

mkdir -p "$QPATH"
mkdir -p "$REQUESTS"
mkdir -p "$ATOM"

if [[ $1 == "-c" ]]; then
    if [[ -f $DAEMON ]]; then
        pid=`dpid`
        echo "Daemon is running: PID = $pid"
    else
        echo "Daemon is not running."
    fi
    exit
fi

if [[ $1 == "-k" ]]; then
    if [[ -f $DAEMON ]]; then
        pid=`dpid`
        echo "Killing daemon: PID = $pid"
        kill -9 "$pid"
        rm -f $DAEMON
    else
        echo "Daemon is not running."
    fi
    exit
fi

if [[ $1 == "-d" ]]; then
    if [[ -f "$DAEMON" ]]; then
        daemonerror "Error: Daemon is already running"
        exit 255
    fi
    daemon 1> /dev/null 2>&1 & disown
    echo "$!" > "$DAEMON"
    pid=`dpid`
    echo -e "\nDaemon started: PID = $pid"
    exit
fi

for a in "$@"
do
    r=`mktemp --tmpdir="$ATOM"`
    echo "$a" > "$r"
    r=${r##*/}
    mv "$ATOM/$r" "$REQUESTS/$r"
done

if [[ ! -f $DAEMON ]]; then
    ./wgetqueue.sh -d 0<&- & disown
    sleep 1
    exit
fi
