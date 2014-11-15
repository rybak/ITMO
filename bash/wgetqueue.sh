#!/bin/bash

shopt -s nullglob
function dpid {
    cat "$DAEMON"
}

function exit_error {
    print_log $1
    exit 255
}

function print_log {
    pid=$(dpid)
    echo -n `date -Iseconds` >> "$ELOG"
    echo -n " $$ " >> "$ELOG"
    echo "PID = $pid" >> "$ELOG"
    echo -e $1 >> "$ELOG"
}

function my_notify {
    notify-send -i "$ICON" -u $1 "$TITLE" "$2"
}

function print_dl {
    echo "URL $1 > $2"
}

function daemon {
    trap "" SIGHUP
    while true
    do
        for r in "$QPATH/requests"/*$SUFREQ
        do
            echo "r = $r" >> "$ELOG"
            url=$(cat "$r")
            req="${r%.*}"
            rm "$r"
            if [[ -f "$req$SUFNAME" ]];
            then
                name=$(cat "$req$SUFNAME")
                name_arg="-O $name"
                rm "$req$SUFNAME"
            else
                name_arg=
                name=
            fi
            my_notify normal "Started download: $(print_dl $url $name)"
            echo "$name : $url" > "$LAST"
            print_log "Download started\n\t$(print_dl $url $name)"
            if wget $name_arg -c "$url" 2>> "$WGETLOG";
            then
                my_notify normal "Download complete: $(print_dl $url $name)"
                print_log "Download complete\n\t$(print_dl $url $name)"
                rm "$LAST"
            else
                my_notify critical "Error while downloading: $(print_dl $url $name)"
                print_log "Error while downloading:\n\t$(print_dl $url $name)"
            fi
        done
        sleep 5
    done
}

function usage {
    echo "Usage"
    echo "./wgetqueue.sh [Flag] | [--dry] [[-n NAME] URL]..."
    echo "URL"
    echo -e "\tIf URL is given, wgetqueue.sh adds URL to queue."
    echo "Flags"
    echo -e "\t-c\n\t\tCheck if daemon is running."
    echo -e "\t-k\n\t\tKill daemon."
    echo -e "\t-h\n\t\tPrint this help."
    echo -e "\t-n NAME\n\t\tUse NAME as a filename for the next URL."
    echo -e "\t--dry\n\t\tMake a dry run, print parsed requests and exit."
    echo -e "\nLogs :\n\t$ELOG\n\t$WGETLOG\n\t$LAST"
    echo ""
}

function show_last {
    if [[ -f "$LAST" ]];
    then
        echo "$1"
        cat "$LAST"
    fi
}

QPATH=$HOME/.nyaqueue
TITLE=wgetqueue
ICON=$QPATH/icon.png

DAEMON=$QPATH/daemonrunning
REQUESTS=$QPATH/requests
ATOM=$QPATH/atom

ELOG=$QPATH/error.log
WGETLOG=$QPATH/get.log
LAST=$QPATH/last.log

SUFNAME=.name
SUFREQ=.req

if [[ $# -eq 0 ]] || [[ $1 == "-h" ]];
then
    usage
    exit
fi

mkdir -p "$QPATH"
mkdir -p "$REQUESTS"
mkdir -p "$ATOM"

if [[ $1 == "-c" ]];
then
    if [[ -f "$DAEMON" ]];
    then
        pid=$(dpid)
        echo "Daemon is running: PID = $pid"
        show_last "Download in progress :"
    else
        echo "Daemon is not running."
        show_last "Unfinished download was interrupted:"
    fi
    exit
fi

if [[ $1 == "-k" ]];
then
    if [[ -f "$DAEMON" ]];
    then
        # TODO make a check if there is a current download // file $LAST exists
        show_last "Unfinished download was interrupted:" >> "$ELOG"
        pid=$(dpid)
        echo "Killing daemon: PID = $pid"
        kill -9 "$pid"
        rm -f "$DAEMON"
    else
        echo "Daemon is not running."
    fi
    exit
fi

if [[ $1 == "-d" ]];
then
    if [[ -f "$DAEMON" ]];
    then
        pid=$(dpid)
        echo "Error: Daemon is already running: PID = $pid"
        exit 255
    fi
    daemon 1> '/dev/null' 2>&1 & disown
    echo "$!" > "$DAEMON"
    pid=$(dpid)
    echo -e "Daemon started: PID = $pid"
    exit
fi

if [[ "$1" == "--dry" ]];
then
    DRY=true
    shift
else
    DRY=false
fi

while (( "$#" ));
do
    name=
    if [[ "$1" == "-n" ]];
    then
        if [[ $# -lt 3 ]];
        then
            echo "Wrong arguments: $@"
            usage
            exit 255
        fi
        shift
        name="$1"
        shift
    fi
    r=`mktemp -u --tmpdir="$ATOM"`
    echo "$1" > "$r$SUFREQ"
    if [[ -n "$name" ]];
    then
        echo "$name" > "$r$SUFNAME"
    fi
    if [[ "$DRY" == true ]];
    then
        echo "Request:"
        ls "$r"* | xargs cat
        rm "$r"*
    else
        r=${r##*/}
        mv "$ATOM/$r"* "$REQUESTS"
    fi
    shift
done

if [[ ! -f "$DAEMON" ]]; then
    $0 -d 0<&- & disown
    sleep 1
    exit
fi

